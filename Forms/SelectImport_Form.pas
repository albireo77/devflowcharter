unit SelectImport_Form;

interface

uses
   System.Classes, Vcl.Forms, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Controls, Base_Form;

type
  TSelectImportForm = class(TBaseForm)
    pnlImports: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    chkSelectAll: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure chkSelectAllClick(Sender: TObject);
  private
     FList: TStringList;
     procedure SetComponents;
     procedure ClearCheckBoxes;
     procedure SetHeight(AHeight: integer);
  public
     procedure SetSelectList(AList: TStringList);
     procedure ResetForm; override;
  end;

var
  SelectImportForm: TSelectImportForm;

implementation

uses
   Infrastructure, WinApi.Windows;

{$R *.dfm}

procedure TSelectImportForm.SetSelectList(AList: TStringList);
begin
   FList := AList;
end;

procedure TSelectImportForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   if (FList <> nil) and not IsAbortResult(ModalResult) then
   begin
      for var i := pnlImports.ControlCount-1 downto 0 do
      begin
         if not TCheckBox(pnlImports.Controls[i]).Checked then
            FList.Delete(i);
      end;
   end;
   FList := nil;
end;

procedure TSelectImportForm.FormShow(Sender: TObject);
begin
   chkSelectAll.Checked := True;
   ClearCheckBoxes;
   SetComponents;
   Width := Constraints.MaxWidth;
   SetHeight(btnCancel.Top + btnCancel.Height + TInfra.Scaled(Self, 45));
end;

procedure TSelectImportForm.ResetForm;
begin
   Caption := '';
   chkSelectAll.Checked := True;
   ClearCheckBoxes;
   FList := nil;
   Close;
end;

procedure TSelectImportForm.chkSelectAllClick(Sender: TObject);
begin
   for var i := 0 to pnlImports.ControlCount-1 do
      TCheckBox(pnlImports.Controls[i]).Checked := chkSelectAll.Checked;
end;

procedure TSelectImportForm.SetComponents;
begin
   var t := 10;
   if FList <> nil then
   begin
      for var i := 0 to FList.Count-1 do
      begin
         var chkBox := TCheckBox.Create(pnlImports);
         chkBox.Parent := pnlImports;
         chkBox.Caption := FList[i];
         chkBox.Checked := True;
         chkBox.Left := 10;
         chkBox.Top := t;
         t := t + chkBox.Height + 10;
      end;
   end;
   pnlImports.Height := t + 5;
   chkSelectAll.Top := pnlImports.Top + pnlImports.Height + 11;
   btnOk.Top := chkSelectAll.Top - 4;
   btnCancel.Top := btnOk.Top;
end;

procedure TSelectImportForm.ClearCheckBoxes;
begin
   while pnlImports.ControlCount > 0 do
      pnlImports.Controls[0].Free;
end;

procedure TSelectImportForm.SetHeight(AHeight: integer);
begin
   var h := MulDiv(Screen.Height, 9, 10);
   if AHeight < h then
      h := AHeight;
   Constraints.MaxHeight := h;
   Constraints.MinHeight := h;
   Height := h;
end;

end.
