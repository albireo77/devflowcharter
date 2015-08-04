unit EditMemo_Form;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Base_Form, Base_Block;

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
   ApplicationCommon;

{$R *.dfm}

procedure TMemoEditorForm.ResetForm;
begin
   SourceBlock := nil;
end;

procedure TMemoEditorForm.btnOKClick(Sender: TObject);
var
   lMemo: TMemo;
begin
   if (Sender = btnOK) and (SourceBlock <> nil) then
   begin
      lMemo := SourceBlock.GetFrontMemo;
      if lMemo <> nil then
      begin
         lMemo.Text := memEditor.Text;
         SourceBlock.memoWidth := Width;
         SourceBlock.memoHeight := Height;
      end;
      SourceBlock := nil;
   end;
   Close;
end;

procedure TMemoEditorForm.FormShow(Sender: TObject);
var
   lMemo: TMemo;
   lPoint: TPoint;
begin
   if SourceBlock <> nil then
   begin
      lMemo := SourceBlock.GetFrontMemo;
      if lMemo <> nil then
      begin
         lPoint := lMemo.ClientToScreen(Point(0, 0));
         SetBounds(lPoint.X, lPoint.Y, SourceBlock.memoWidth, SourceBlock.memoHeight);
         memEditor.Font.Assign(lMemo.Font);
         memEditor.Font.Color := clNavy;
         memEditor.Text := lMemo.Text;
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
