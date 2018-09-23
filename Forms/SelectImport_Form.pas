unit SelectImport_Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons,
  Base_Form;

type
  TSelectImportForm = class(TBaseForm)
    pnlImports: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    chkSelectAll: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure chkSelectAllClick(Sender: TObject);
  private
     FList: TStringList;
     procedure CreateCheckBoxList;
     procedure ClearCheckBoxList;
  public
     procedure SetSelectList(AList: TStringList);
     procedure ResetForm; override;
  end;

var
  SelectImportForm: TSelectImportForm;

implementation

uses
   System.UITypes;

{$R *.dfm}

procedure TSelectImportForm.SetSelectList(AList: TStringList);
begin
   FList := AList;
end;

procedure TSelectImportForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
   i: integer;
begin
   if (FList <> nil) and not IsAbortResult(ModalResult) then
   begin
      for i := pnlImports.ControlCount-1 downto 0 do
      begin
         if not TCheckBox(pnlImports.Controls[i]).Checked then
            FList.Delete(i);
      end;
   end;
   FList := nil;
end;

procedure TSelectImportForm.FormCreate(Sender: TObject);
begin
   Constraints.MaxHeight := (Screen.Height * 9) div 10;
   Constraints.MaxWidth := 279;
   Constraints.MinWidth := 279;
end;

procedure TSelectImportForm.FormShow(Sender: TObject);
begin
   chkSelectAll.Checked := true;
   ClearCheckBoxList;
   CreateCheckBoxList;
end;

procedure TSelectImportForm.ResetForm;
begin
   Caption := '';
   chkSelectAll.Checked := true;
   ClearCheckBoxList;
   FList := nil;
   Close;
end;

procedure TSelectImportForm.chkSelectAllClick(Sender: TObject);
var
   i: integer;
begin
   for i := 0 to pnlImports.ControlCount-1 do
      TCheckBox(pnlImports.Controls[i]).Checked := chkSelectAll.Checked;
end;

procedure TSelectImportForm.CreateCheckBoxList;
var
   i, t: integer;
   chkBox: TCheckBox;
begin
   t := 10;
   if FList <> nil then
   begin
      for i := 0 to FList.Count-1 do
      begin
         chkBox := TCheckBox.Create(pnlImports);
         chkBox.Parent := pnlImports;
         chkBox.Caption := FList[i];
         chkBox.Checked := true;
         chkBox.Left := 10;
         chkBox.Top := t;
         t := t + chkBox.Height + 10;
      end;
   end;
   pnlImports.Height := t + 5;
   chkSelectAll.Top := pnlImports.Top + pnlImports.Height + 11;
   btnOk.Top := chkSelectAll.Top - 4;
   btnCancel.Top := chkSelectAll.Top - 4;
   Height := btnCancel.BoundsRect.Bottom + 45;
end;

procedure TSelectImportForm.ClearCheckBoxList;
begin
   while pnlImports.ControlCount > 0 do
      pnlImports.Controls[0].Free;
end;

end.
