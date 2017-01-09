unit Navigator_Form;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Base_Form, OmniXML, ComCtrls, StdCtrls, ExtCtrls;

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
    procedure ExportSettingsToXMLTag(const root: IXMLElement); override;
    procedure ImportSettingsFromXMLTag(const root: IXMLElement); override;
    procedure DoInvalidate;
  end;

var
  NavigatorForm: TNavigatorForm;

implementation

uses
   ApplicationCommon, BlockTabSheet;

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
   selStart, xExt, yExt: integer;
   page: TBlockTabSheet;
   R: TRect;
begin
   if GProject <> nil then
   begin
      edit := TInfra.GetActiveEdit;
      if edit <> nil then
         selStart := edit.SelStart;
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
         Canvas.Polyline([Point(R.Left, R.Top),
                          Point(R.Right, R.Top),
                          Point(R.Right, R.Bottom),
                          Point(R.Left, R.Bottom),
                          Point(R.Left, R.Top)]);
      finally
         RestoreDC(Canvas.Handle, lhdc);
         if edit <> nil then
            edit.SelStart := selStart;
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

procedure TNavigatorForm.ExportSettingsToXMLTag(const root: IXMLElement);
begin
   if Visible then
   begin
      root.SetAttribute('nav_win_show', '1');
      root.SetAttribute('nav_win_x', IntToStr(Left));
      root.SetAttribute('nav_win_y', IntToStr(Top));
      root.SetAttribute('nav_win_width', IntToStr(Width));
      root.SetAttribute('nav_win_height', IntToStr(Height));
      if WindowState = wsMinimized then
         root.SetAttribute('nav_win_min', '1');
   end;
end;

procedure TNavigatorForm.ImportSettingsFromXMLTag(const root: IXMLElement);
var
   x, y, w, h: integer;
begin
   if root.GetAttribute('nav_win_show') = '1' then
   begin
      Position := poDesigned;
      if root.GetAttribute('nav_win_min') = '1' then
         WindowState := wsMinimized;
      x := StrToIntDef(root.GetAttribute('nav_win_x'), 50);
      y := StrToIntDef(root.GetAttribute('nav_win_y'), 50);
      w := StrToIntDef(root.GetAttribute('nav_win_width'), 426);
      h := StrToIntDef(root.GetAttribute('nav_win_height'), 341);
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
