unit EditMemo_Form;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, Base_Form, Base_Block;

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
    SourceBlock: TBlock;
  end;

var
  MemoEditorForm: TMemoEditorForm;

implementation

uses
   ApplicationCommon, Dialogs;

{$R *.dfm}

procedure TMemoEditorForm.ResetForm;
begin
   SourceBlock := nil;
end;

procedure TMemoEditorForm.btnOKClick(Sender: TObject);
var
   memo: TMemo;
begin
   if (Sender = btnOK) and (SourceBlock <> nil) then
   begin
      memo := SourceBlock.GetFrontMemo;
      if memo <> nil then
      begin
         memo.Text := memEditor.Text;
         SourceBlock.memoWidth := Width;
         SourceBlock.memoHeight := Height;
      end;
      SourceBlock := nil;
   end;
   Close;
end;

procedure TMemoEditorForm.FormShow(Sender: TObject);
var
   memo: TMemo;
   pnt: TPoint;
begin
   if SourceBlock <> nil then
   begin
      memo := SourceBlock.GetFrontMemo;
      if memo <> nil then
      begin
         pnt := memo.ClientOrigin;
         SetBounds(pnt.X, pnt.Y, SourceBlock.memoWidth, SourceBlock.memoHeight);
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
