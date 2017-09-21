unit EditMemo_Form;

interface

uses
  System.Classes, Vcl.StdCtrls, Vcl.Controls, Base_Form, Base_Block, CommonInterfaces;

type

  TMemoEditorForm = class(TBaseForm)
    memEditor: TMemo;
    btnOK: TButton;
    btnCancel: TButton;
    procedure ResetForm; override;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Source: IMemo;
  end;

var
  MemoEditorForm: TMemoEditorForm;

implementation

uses
   WinApi.Windows, Vcl.Graphics, Vcl.Forms, ApplicationCommon;

{$R *.dfm}

procedure TMemoEditorForm.ResetForm;
begin
   Source := nil;
end;

procedure TMemoEditorForm.btnOKClick(Sender: TObject);
var
   memo: TMemo;
begin
   if (Sender = btnOK) and (Source <> nil) then
   begin
      memo := Source.GetMemo;
      if memo <> nil then
      begin
         memo.Text := memEditor.Text;
         //memo.Width := Width;
         //memo.Height := Height;
      end;
      Source := nil;
   end;
   Close;
end;

procedure TMemoEditorForm.FormShow(Sender: TObject);
var
   memo: TMemo;
   pnt: TPoint;
begin
   if Source <> nil then
   begin
      memo := Source.GetMemo;
      if memo <> nil then
      begin
         pnt := memo.ClientOrigin;
         SetBounds(pnt.X, pnt.Y, memo.Width, memo.Height);
         memEditor.Font.Assign(memo.Font);
         memEditor.Font.Color := clNavy;
         memEditor.Text := memo.Text;
      end;
   end;
end;

procedure TMemoEditorForm.FormCreate(Sender: TObject);
begin
   memEditor.DoubleBuffered := true;
   if (i18Manager.LoadStaticLabels(GSettings.TranslateFile) = 0) and (i18Manager.LoadDefaultLabels = 0) then
      Application.Terminate;
end;

end.
