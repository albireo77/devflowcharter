unit Navigator_Form;

interface

uses
  System.Classes, Vcl.Controls, Vcl.StdCtrls, Base_Form, OmniXML;

type
  TNavigatorForm = class(TBaseForm)
    chkAlphaVisible: TCheckBox;
    scbAlphaVal: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure chkAlphaVisibleClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure scbAlphaValChange(Sender: TObject);
  private
    { Private declarations }
    procedure SetAlphaValVisible(const AValue: boolean);
  public
    { Public declarations }
    InvalidateInd: boolean;
    procedure ResetForm; override;
    procedure ExportSettingsToXMLTag(ATag: IXMLElement); override;
    procedure ImportSettingsFromXMLTag(ATag: IXMLElement); override;
    procedure DoInvalidate;
  end;

var
  NavigatorForm: TNavigatorForm;

implementation

uses
   WinApi.Windows, System.SysUtils, Vcl.Graphics, Vcl.Forms, System.Types, ApplicationCommon,
   BlockTabSheet, XMLProcessor;

{$R *.dfm}

procedure TNavigatorForm.FormCreate(Sender: TObject);
begin
   DoubleBuffered := true;
   InvalidateInd := true;
   SetBounds(50, 50, 426, 341);
   Constraints.MinWidth := 150;
   Constraints.MinHeight := 150;
   ControlStyle := ControlStyle + [csOpaque];
   Constraints.MaxWidth := (Screen.Width*9) div 10;
   Constraints.MaxHeight := (Screen.Height*9) div 10;
   scbAlphaVal.Position := GSettings.NavigatorAlphaValue;
   scbAlphaVal.OnKeyDown := TInfra.GetMainForm.OnKeyDown;
   chkAlphaVisible.Checked := GSettings.NavigatorAlphaVisible;
   OnMouseWheel := TInfra.GetMainForm.OnMouseWheel;
end;

procedure TNavigatorForm.FormPaint(Sender: TObject);
const
   EXTENT_X = 1024;
   EXTENT_Y = 1024;
var
   lhdc: HDC;
   edit: TCustomEdit;
   selStart, selLength, xExt, yExt: integer;
   page: TBlockTabSheet;
   R: TRect;
begin
   if GProject <> nil then
   begin
      edit := TInfra.GetActiveEdit;
      if edit <> nil then
      begin
         selStart := edit.SelStart;
         selLength := edit.SelLength;
      end;
      lhdc := SaveDC(Canvas.Handle);
      try
         page := GProject.GetActivePage;
         xExt := MulDiv(EXTENT_X, page.ClientWidth, ClientWidth);
         yExt := MulDiv(EXTENT_Y, page.ClientHeight, ClientHeight);
         SetMapMode(Canvas.Handle, MM_ANISOTROPIC);
         SetWindowExtEx(Canvas.Handle, xExt, yExt, nil);
         SetViewPortExtEx(Canvas.Handle, EXTENT_X, EXTENT_Y, nil);
         page.PaintTo(Canvas, 0, 0);
         Canvas.Pen.Width := 2;
         Canvas.Pen.Color := clRed;
         R := TInfra.GetDisplayRect(page);
         Canvas.Polyline([R.TopLeft,
                          Point(R.Right, R.Top),
                          R.BottomRight,
                          Point(R.Left, R.Bottom),
                          R.TopLeft]);
      finally
         RestoreDC(Canvas.Handle, lhdc);
         if edit <> nil then
         begin
            edit.SelStart := selStart;
            edit.SelLength := selLength;
         end;
      end;
   end;
end;

procedure TNavigatorForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   page: TBlockTabSheet;
begin
   if (Button = mbLeft) and (GProject <> nil) then
   begin
      page := GProject.GetActivePage;
      page.Form.HorzScrollBar.Position := MulDiv(X, page.ClientWidth, ClientWidth) - (page.Form.ClientWidth div 2);
      page.Form.VertScrollBar.Position := MulDiv(Y, page.ClientHeight, ClientHeight) - (page.Form.ClientHeight div 2);
      Invalidate;
      page.Form.Repaint;
   end;
end;


procedure TNavigatorForm.ResetForm;
begin
   inherited ResetForm;
   Position := poDesigned;
   InvalidateInd := true;
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

procedure TNavigatorForm.ExportSettingsToXMLTag(ATag: IXMLElement);
begin
   if Visible then
   begin
      ATag.SetAttribute('nav_win_show', 'true');
      ATag.SetAttribute('nav_win_x', Left.ToString);
      ATag.SetAttribute('nav_win_y', Top.ToString);
      ATag.SetAttribute('nav_win_width', Width.ToString);
      ATag.SetAttribute('nav_win_height', Height.ToString);
      if WindowState = wsMinimized then
         ATag.SetAttribute('nav_win_min', 'true');
   end;
end;

procedure TNavigatorForm.ImportSettingsFromXMLTag(ATag: IXMLElement);
var
   x, y, w, h: integer;
begin
   if TXMLProcessor.GetBoolFromAttr(ATag, 'nav_win_show') then
   begin
      Position := poDesigned;
      if TXMLProcessor.GetBoolFromAttr(ATag, 'nav_win_min') then
         WindowState := wsMinimized;
      x := StrToIntDef(ATag.GetAttribute('nav_win_x'), 50);
      y := StrToIntDef(ATag.GetAttribute('nav_win_y'), 50);
      w := StrToIntDef(ATag.GetAttribute('nav_win_width'), 426);
      h := StrToIntDef(ATag.GetAttribute('nav_win_height'), 341);
      SetBounds(x, y, w, h);
      Show;
   end;
end;

procedure TNavigatorForm.chkAlphaVisibleClick(Sender: TObject);
begin
   SetAlphaValVisible(chkAlphaVisible.Checked);
   GSettings.NavigatorAlphaVisible := chkAlphaVisible.Checked;
end;

procedure TNavigatorForm.FormShow(Sender: TObject);
begin
   chkAlphaVisible.Checked := GSettings.NavigatorAlphaVisible;
   SetAlphaValVisible(chkAlphaVisible.Checked);
end;

procedure TNavigatorForm.SetAlphaValVisible(const AValue: boolean);
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

procedure TNavigatorForm.DoInvalidate;
begin
   if InvalidateInd then
      Invalidate;
end;

procedure TNavigatorForm.scbAlphaValChange(Sender: TObject);
begin
   AlphaBlendValue := scbAlphaVal.Position;
   GSettings.NavigatorAlphaValue := AlphaBlendValue;
end;

end.
