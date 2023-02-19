unit Navigator_Form;

interface

uses
  System.Classes, Vcl.Controls, Vcl.StdCtrls, System.Types, WinApi.Messages,
  Base_Form, OmniXML;

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
  private
    { Private declarations }
    procedure SetAlphaValVisible(AValue: boolean);
    procedure MouseWheelHandler(var AMessage: TMessage); override;
  public
    { Public declarations }
    InvalidateIndicator: boolean;
    procedure ResetForm; override;
    procedure ExportToXML(ANode: IXMLNode); override;
    procedure ImportFromXML(ANode: IXMLNode); override;
  end;

var
  NavigatorForm: TNavigatorForm;

implementation

uses
   WinApi.Windows, System.SysUtils, Vcl.Graphics, Vcl.Forms, Infrastructure, BlockTabSheet,
   OmniXMLUtils;

{$R *.dfm}

procedure TNavigatorForm.FormCreate(Sender: TObject);
begin
   DoubleBuffered := True;
   InvalidateIndicator := True;
   SetBounds(50, 50, 426, 341);
   Constraints.MinWidth := 150;
   Constraints.MinHeight := 150;
   ControlStyle := ControlStyle + [csOpaque];
   Constraints.MaxWidth := (Screen.Width*9) div 10;
   Constraints.MaxHeight := (Screen.Height*9) div 10;
   scbAlphaVal.Position := GSettings.NavigatorAlphaValue;
   scbAlphaVal.OnKeyDown := TInfra.GetMainForm.OnKeyDown;
   chkAlphaVisible.Checked := GSettings.NavigatorAlphaVisible;
end;

procedure TNavigatorForm.FormPaint(Sender: TObject);
const
   EXTENT_X = 1024;
   EXTENT_Y = 1024;
begin
   if GProject <> nil then
   begin
      var hdc := SaveDC(Canvas.Handle);
      try
         var box := GProject.ActivePage.Box;
         var xExt := MulDiv(EXTENT_X, box.HorzScrollBar.Range, ClientWidth);
         var yExt := MulDiv(EXTENT_Y, box.VertScrollBar.Range, ClientHeight);
         SetMapMode(Canvas.Handle, MM_ANISOTROPIC);
         SetWindowExtEx(Canvas.Handle, xExt, yExt, nil);
         SetViewPortExtEx(Canvas.Handle, EXTENT_X, EXTENT_Y, nil);
         box.PaintToCanvas(Canvas);
         Canvas.Pen.Width := 2;
         Canvas.Pen.Color := clRed;
         var r := box.GetDisplayRect;
         Canvas.Polyline([r.TopLeft,
                          Point(r.Right, r.Top),
                          r.BottomRight,
                          Point(r.Left, r.Bottom),
                          r.TopLeft]);
      finally
         RestoreDC(Canvas.Handle, hdc);
      end;
   end;
end;

procedure TNavigatorForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if (Button = mbLeft) and (GProject <> nil) then
   begin
      var box := GProject.ActivePage.Box;
      box.HorzScrollBar.Position := MulDiv(X, box.HorzScrollBar.Range, ClientWidth) - (box.ClientWidth div 2);
      box.VertScrollBar.Position := MulDiv(Y, box.VertScrollBar.Range, ClientHeight) - (box.ClientHeight div 2);
      Repaint;
      box.Repaint;
   end;
end;


procedure TNavigatorForm.ResetForm;
begin
   inherited ResetForm;
   Position := poDesigned;
   InvalidateIndicator := True;
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
   if GProject <> nil then
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
   if AValue then
   begin
      scbAlphaVal.Width := 33;
      scbAlphaVal.Height := 89;
   end
   else
   begin
      scbAlphaVal.Width := 1;
      scbAlphaVal.Height := 1;
   end;
end;

procedure TNavigatorForm.scbAlphaValChange(Sender: TObject);
begin
   AlphaBlendValue := scbAlphaVal.Position;
   GSettings.NavigatorAlphaValue := AlphaBlendValue;
end;

end.
