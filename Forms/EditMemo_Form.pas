unit EditMemo_Form;

interface

uses
  System.Classes, Vcl.StdCtrls, Vcl.Controls, Base_Form, Base_Block, Interfaces, MemoEx;

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
    Source: TMemoEx;
  end;

var
  MemoEditorForm: TMemoEditorForm;

implementation

uses
   Vcl.Graphics, Vcl.Forms, Infrastructure;

{$R *.dfm}

procedure TMemoEditorForm.ResetForm;
begin
   Source := nil;
end;

procedure TMemoEditorForm.btnOKClick(Sender: TObject);
begin
   if (Sender = btnOK) and (Source <> nil) then
   begin
      Source.Text := memEditor.Text;
      Source.EditFormWidth := Width;
      Source.EditFormHeight := Height;
   end;
   Source := nil;
   Close;
end;

procedure TMemoEditorForm.FormShow(Sender: TObject);
begin
   if Source <> nil then
   begin
      var pnt := Source.ClientOrigin;
      SetBounds(pnt.X, pnt.Y, Source.EditFormWidth, Source.EditFormHeight);
      memEditor.Font.Assign(Source.Font);
      memEditor.Font.Color := clNavy;
      memEditor.Alignment := Source.Alignment;
      memEditor.WordWrap := Source.WordWrap;
      memEditor.Text := Source.Text;
      memEditor.SelStart := Source.SelStart;
      memEditor.SelLength := Source.SelLength;
      memEditor.SetFocus;
   end;
end;

procedure TMemoEditorForm.FormCreate(Sender: TObject);
begin
   memEditor.DoubleBuffered := True;
   if (i18Manager.LoadStaticLabels(GSettings.TranslateFile) = 0) and (i18Manager.LoadDefaultLabels = 0) then
      Application.Terminate;
end;

end.
