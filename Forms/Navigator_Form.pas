unit Navigator_Form;

interface

uses
  System.Classes, Vcl.Controls, Vcl.StdCtrls, Vcl.Graphics, System.Types, WinApi.Messages, Base_Form,
  OmniXML;

type
  TNavigatorForm = class(TBaseForm)
    chkAlphaVisible: TCheckBox;
    scbAlphaVal: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure chkAlphaVisibleClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure scbAlphaValChange(Sender: TObject);
    procedure DialogKey(var Msg: TCMDialogKey); message CM_DIALOGKEY;
  private
    { Private declarations }
    FFlowchart: TBitmap;
    FFlowchartBox: TObject;
    FFlowchartRange: TPoint;
    FFlowchartValid: boolean;
    procedure SetAlphaValVisible(AValue: boolean);
    procedure RenderFlowchart;
  public
    { Public declarations }
    InvalidateIndicator: boolean;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure RepaintIndicator;
    procedure ResetForm; override;
    procedure ExportToXML(ANode: IXMLNode); override;
    procedure ImportFromXML(ANode: IXMLNode); override;
    procedure MouseWheelHandler(var AMessage: TMessage); override;
  end;

var
  NavigatorForm: TNavigatorForm;

implementation

uses
   WinApi.Windows, System.SysUtils, System.UITypes, System.Math, Vcl.Forms, Infrastructure,
   OmniXMLUtils;

{$R *.dfm}

procedure TNavigatorForm.FormCreate(Sender: TObject);
begin
   DoubleBuffered := True;
   FFlowchart := Vcl.Graphics.TBitmap.Create;
   InvalidateIndicator := True;
   SetBounds(50, 50, 426, 341);
   Constraints.MinWidth := 150;
   Constraints.MinHeight := 150;
   ControlStyle := ControlStyle + [csOpaque];
   Constraints.MaxWidth := (Screen.Width * 9) div 10;
   Constraints.MaxHeight := (Screen.Height * 9) div 10;
   scbAlphaVal.Position := GSettings.NavigatorAlphaValue;
   scbAlphaVal.OnKeyDown := TInfra.GetMainForm.OnKeyDown;
   chkAlphaVisible.Checked := GSettings.NavigatorAlphaVisible;
end;

procedure TNavigatorForm.DialogKey(var Msg: TCMDialogKey);
begin
   var y := 0;
   case Msg.CharCode of
      vkDown, vkRight: y := 15;
      vkUp, vkLeft:    y := -15;
   end;
   if (y <> 0) and (GProject <> nil) and (GProject.ActivePage <> nil) then
   begin
      var box := GProject.ActivePage.Box;
      if Msg.CharCode in [vkLeft, vkRight] then
         box.HorzScrollBar.Position := box.HorzScrollBar.Position + y
      else
         box.VertScrollBar.Position := box.VertScrollBar.Position + y;
      RepaintIndicator;
      Update;
   end
   else
      inherited;
end;

destructor TNavigatorForm.Destroy;
begin
   FFlowchart.Free;
   inherited Destroy;
end;

// any external invalidation may mean flowchart change so its image must be rendered again
procedure TNavigatorForm.Invalidate;
begin
   FFlowchartValid := False;
   inherited Invalidate;
end;

// repaint after scrolling only; flowchart image is reused
procedure TNavigatorForm.RepaintIndicator;
begin
   if HandleAllocated then
      InvalidateRect(Handle, nil, False);
end;

type
   TEmfPoints = array[0..MaxInt div SizeOf(TPoint) - 1] of TPoint;
   PEmfPoints = ^TEmfPoints;
   TEmfSmallPoints = array[0..MaxInt div SizeOf(TSmallPoint) - 1] of TSmallPoint;
   PEmfSmallPoints = ^TEmfSmallPoints;
   PEmfExtLogPen = ^TExtLogPen;   // PExtLogPen from WinApi.Windows wrongly points to TLogPen

function GetPenColor(DC: HDC): TColorRef;
begin
   result := CLR_INVALID;
   var pen := GetCurrentObject(DC, OBJ_PEN);
   var size := GetObject(pen, 0, nil);
   if size = SizeOf(TLogPen) then
   begin
      var logPen: TLogPen;
      GetObject(pen, size, @logPen);
      if logPen.lopnStyle <> PS_NULL then
         result := logPen.lopnColor;
   end
   else if size > 0 then
   begin
      var buf: TBytes;
      SetLength(buf, size);
      GetObject(pen, size, @buf[0]);
      var extPen := PEmfExtLogPen(@buf[0]);
      if (extPen.elpPenStyle and PS_STYLE_MASK) <> PS_NULL then
         result := extPen.elpColor;
   end;
end;

function GetBrushColor(DC: HDC): TColorRef;
begin
   result := CLR_INVALID;
   var logBrush: TLogBrush;
   if (GetObject(GetCurrentObject(DC, OBJ_BRUSH), SizeOf(logBrush), @logBrush) <> 0) and (logBrush.lbStyle <> BS_NULL) then
      result := logBrush.lbColor;
end;

// GDI never draws the last pixel of a line, so after scaling down a line shorter than a pixel
// is not drawn at all; this sets that missing pixel for line records
procedure SetLineEndPixels(DC: HDC; ARecord: PEnhMetaRecord);
begin
   var params := PIntegerArray(@ARecord.dParm);
   var points: TArray<TPoint> := nil;
   case ARecord.iType of
      EMR_LINETO:
         points := [Point(params[0], params[1])];
      EMR_POLYLINE, EMR_POLYLINETO:
      begin
         // params[0..3] is bounding rectangle, params[4] is number of points
         var pts := PEmfPoints(@params[5]);
         SetLength(points, params[4]);
         for var i := 0 to High(points) do
            points[i] := pts[i];
      end;
      EMR_POLYLINE16, EMR_POLYLINETO16:
      begin
         var pts := PEmfSmallPoints(@params[5]);
         SetLength(points, params[4]);
         for var i := 0 to High(points) do
            points[i] := Point(pts[i].X, pts[i].Y);
      end;
   end;
   if points <> nil then
   begin
      var color := GetPenColor(DC);
      if color <> CLR_INVALID then
      begin
         for var pnt in points do
            SetPixelV(DC, pnt.X, pnt.Y, color);
      end;
   end;
end;

// FrameRect draws its sides as 1 unit wide PatBlt strips which after scaling down are often
// 0 pixels wide; such strip is drawn 1 pixel wide in device coordinates and is kept inside
// clip box because side lying on the edge of block is clipped out when its pixel is rounded
// onto clip boundary
procedure PlayThinPatBlt(DC: HDC; ARecord: PEMRBitBlt);
begin
   if (ARecord.cbBmiSrc <> 0) or (GetBrushColor(DC) = CLR_INVALID) then   // copies bitmap or no brush
      Exit;
   var r := Rect(ARecord.xDest, ARecord.yDest, ARecord.xDest + ARecord.cxDest, ARecord.yDest + ARecord.cyDest);
   LPtoDP(DC, r, 2);
   r.NormalizeRect;
   if (r.Width > 1) and (r.Height > 1) then   // fill, not a line
      Exit;
   var saveIndex := SaveDC(DC);
   try
      // switch to device coordinates
      var xForm := Default(TXForm);
      SetGraphicsMode(DC, GM_ADVANCED);
      ModifyWorldTransform(DC, xForm, MWT_IDENTITY);
      SetMapMode(DC, MM_TEXT);
      SetWindowOrgEx(DC, 0, 0, nil);
      SetViewportOrgEx(DC, 0, 0, nil);
      var clip: TRect;
      if GetClipBox(DC, clip) > NULLREGION then
      begin
         if r.Width <= r.Height then
         begin
            var x := EnsureRange(r.Left, clip.Left, clip.Right-1);
            var y1 := Max(r.Top, clip.Top);
            var y2 := Min(Max(r.Bottom, r.Top+1), clip.Bottom);
            if y2 > y1 then
               PatBlt(DC, x, y1, 1, y2-y1, ARecord.dwRop);
         end
         else
         begin
            var y := EnsureRange(r.Top, clip.Top, clip.Bottom-1);
            var x1 := Max(r.Left, clip.Left);
            var x2 := Min(Max(r.Right, r.Left+1), clip.Right);
            if x2 > x1 then
               PatBlt(DC, x1, y, x2-x1, 1, ARecord.dwRop);
         end;
      end;
   finally
      RestoreDC(DC, saveIndex);
   end;
end;

// when scaled down, fills and texts painted later (e.g. edit control inside block) can cover lines
// painted earlier; so after whole metafile is played, this callback plays it again but only lines
// and shape outlines and skips anything that fills
function PlayLinesRecord(DC: HDC; ATable: PHandleTable; ARecord: PEnhMetaRecord; AObjCount: Integer; AData: LPARAM): Integer; stdcall;
begin
   result := 1;
   case ARecord.iType of
      EMR_EXTTEXTOUTA, EMR_EXTTEXTOUTW, EMR_POLYTEXTOUTA, EMR_POLYTEXTOUTW, EMR_SMALLTEXTOUT,
      EMR_STRETCHBLT, EMR_MASKBLT, EMR_PLGBLT, EMR_SETDIBITSTODEVICE, EMR_STRETCHDIBITS,
      EMR_ALPHABLEND, EMR_TRANSPARENTBLT, EMR_GRADIENTFILL, EMR_FILLRGN, EMR_INVERTRGN,
      EMR_PAINTRGN, EMR_FILLPATH, EMR_EXTFLOODFILL: ;
      EMR_RECTANGLE, EMR_ELLIPSE, EMR_ROUNDRECT, EMR_CHORD, EMR_PIE, EMR_POLYGON, EMR_POLYGON16,
      EMR_POLYPOLYGON, EMR_POLYPOLYGON16, EMR_STROKEANDFILLPATH:
      begin
         var oldBrush := SelectObject(DC, GetStockObject(NULL_BRUSH));
         PlayEnhMetaFileRecord(DC, ATable^, ARecord^, AObjCount);
         SelectObject(DC, oldBrush);
      end;
      EMR_BITBLT:
         PlayThinPatBlt(DC, PEMRBitBlt(ARecord));
      EMR_INTERSECTCLIPRECT:
      begin
         // line lying on the edge of block is clipped out when its pixel is rounded onto clip boundary,
         // so clip rectangle is inflated by 1 pixel
         var params := PIntegerArray(@ARecord.dParm);
         var pixel: TArray<TPoint> := [Point(0, 0), Point(1, 1)];
         DPtoLP(DC, pixel[0], 2);
         var dx := Max(Abs(pixel[1].X - pixel[0].X), 1);
         var dy := Max(Abs(pixel[1].Y - pixel[0].Y), 1);
         IntersectClipRect(DC, params[0]-dx, params[1]-dy, params[2]+dx, params[3]+dy);
      end;
   else
      PlayEnhMetaFileRecord(DC, ATable^, ARecord^, AObjCount);
      SetLineEndPixels(DC, ARecord);
   end;
end;

// flowchart is recorded 1:1 to metafile which is then played scaled down to bitmap
procedure TNavigatorForm.RenderFlowchart;
begin
   var box := GProject.ActivePage.Box;
   FFlowchartBox := box;
   FFlowchartRange := Point(box.HorzScrollBar.Range, box.VertScrollBar.Range);
   FFlowchart.SetSize(ClientWidth, ClientHeight);
   var emf: HENHMETAFILE := 0;
   var refDC := GetDC(0);
   try
      // metafile frame is in 0.01 mm units
      var frame := Rect(0,
                        0,
                        MulDiv(FFlowchartRange.X, GetDeviceCaps(refDC, HORZSIZE) * 100, GetDeviceCaps(refDC, HORZRES)),
                        MulDiv(FFlowchartRange.Y, GetDeviceCaps(refDC, VERTSIZE) * 100, GetDeviceCaps(refDC, VERTRES)));
      var emfDC := CreateEnhMetaFile(refDC, nil, @frame, nil);
      if emfDC <> 0 then
      begin
         var emfCanvas := TCanvas.Create;
         try
            emfCanvas.Handle := emfDC;
            box.PaintToCanvas(emfCanvas);
         finally
            emfCanvas.Free;
            emf := CloseEnhMetaFile(emfDC);
         end;
      end;
   finally
      ReleaseDC(0, refDC);
   end;
   var lCanvas := FFlowchart.Canvas;
   lCanvas.Lock;
   try
      lCanvas.Brush.Style := bsSolid;
      lCanvas.Brush.Color := box.Color;
      lCanvas.FillRect(Rect(0, 0, ClientWidth, ClientHeight));
      if emf <> 0 then
      begin
         var r := Rect(0, 0, ClientWidth, ClientHeight);
         PlayEnhMetaFile(lCanvas.Handle, emf, r);
         EnumEnhMetaFile(lCanvas.Handle, emf, @PlayLinesRecord, nil, r);
      end;
   finally
      lCanvas.Unlock;
      if emf <> 0 then
         DeleteEnhMetaFile(emf);
   end;
   FFlowchartValid := True;
end;

procedure TNavigatorForm.FormPaint(Sender: TObject);
begin
   // there is no active page while project is being loaded
   if (GProject <> nil) and (GProject.ActivePage <> nil) then
   begin
      var box := GProject.ActivePage.Box;
      var range := Point(box.HorzScrollBar.Range, box.VertScrollBar.Range);
      if not FFlowchartValid or (FFlowchartBox <> box) or (FFlowchartRange <> range) or
         (FFlowchart.Width <> ClientWidth) or (FFlowchart.Height <> ClientHeight) then
         RenderFlowchart;
      Canvas.Draw(0, 0, FFlowchart);
      var r := box.GetDisplayRect;
      r := Rect(MulDiv(r.Left, ClientWidth, range.X),
                MulDiv(r.Top, ClientHeight, range.Y),
                Min(MulDiv(r.Right, ClientWidth, range.X), ClientWidth-1),
                Min(MulDiv(r.Bottom, ClientHeight, range.Y), ClientHeight-1));
      Canvas.Pen.Width := 2;
      Canvas.Pen.Color := clRed;
      Canvas.Polyline([r.TopLeft,
                       Point(r.Right, r.Top),
                       r.BottomRight,
                       Point(r.Left, r.Bottom),
                       r.TopLeft]);
   end;
end;

procedure TNavigatorForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if (Button = mbLeft) and (GProject <> nil) and (GProject.ActivePage <> nil) then
   begin
      var box := GProject.ActivePage.Box;
      box.HorzScrollBar.Position := MulDiv(X, box.HorzScrollBar.Range, ClientWidth) - (box.ClientWidth div 2);
      box.VertScrollBar.Position := MulDiv(Y, box.VertScrollBar.Range, ClientHeight) - (box.ClientHeight div 2);
      RepaintIndicator;
      Update;
      box.Repaint;
   end;
end;


procedure TNavigatorForm.ResetForm;
begin
   inherited ResetForm;
   Position := poDesigned;
   InvalidateIndicator := True;
   FFlowchartValid := False;
   SetBounds(50, 50, 426, 341);
end;

procedure TNavigatorForm.FormResize(Sender: TObject);
begin
   Invalidate;
end;

procedure TNavigatorForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then
      FormMouseDown(Sender, mbLeft, Shift, X, Y);
end;

procedure TNavigatorForm.MouseWheelHandler(var AMessage: TMessage);
begin
   inherited MouseWheelHandler(AMessage);
   if (GProject <> nil) and (GProject.ActivePage <> nil) then
      GProject.ActivePage.Box.Perform(AMessage.Msg, AMessage.WParam, AMessage.LParam);
end;

procedure TNavigatorForm.ExportToXML(ANode: IXMLNode);
begin
   if Visible then
   begin
      SetNodeAttrBool(ANode, 'nav_win_show', True);
      SetNodeAttrInt(ANode, 'nav_win_x', Left);
      SetNodeAttrInt(ANode, 'nav_win_y', Top);
      SetNodeAttrInt(ANode, 'nav_win_width', Width);
      SetNodeAttrInt(ANode, 'nav_win_height', Height);
      if WindowState = wsMinimized then
         SetNodeAttrBool(ANode, 'nav_win_min', True);
   end;
end;

procedure TNavigatorForm.ImportFromXML(ANode: IXMLNode);
begin
   if GetNodeAttrBool(ANode, 'nav_win_show', False) then
   begin
      Position := poDesigned;
      if GetNodeAttrBool(ANode, 'nav_win_min', False) then
         WindowState := wsMinimized;
      SetBounds(GetNodeAttrInt(ANode, 'nav_win_x'),
                GetNodeAttrInt(ANode, 'nav_win_y'),
                GetNodeAttrInt(ANode, 'nav_win_width'),
                GetNodeAttrInt(ANode, 'nav_win_height'));
      Show;
   end;
end;

procedure TNavigatorForm.chkAlphaVisibleClick(Sender: TObject);
begin
   SetAlphaValVisible(chkAlphaVisible.Checked);
   GSettings.NavigatorAlphaVisible := chkAlphaVisible.Checked;
   Invalidate;
end;

procedure TNavigatorForm.FormShow(Sender: TObject);
begin
   chkAlphaVisible.Checked := GSettings.NavigatorAlphaVisible;
   SetAlphaValVisible(chkAlphaVisible.Checked);
end;

procedure TNavigatorForm.SetAlphaValVisible(AValue: boolean);
begin
   scbAlphaVal.Width := if AValue then 33 else 1;
   scbAlphaVal.Height := if AValue then 89 else 1;
end;

procedure TNavigatorForm.scbAlphaValChange(Sender: TObject);
begin
   AlphaBlendValue := scbAlphaVal.Position;
   GSettings.NavigatorAlphaValue := AlphaBlendValue;
end;

end.
