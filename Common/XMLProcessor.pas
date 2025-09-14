{
   Copyright (C) 2006 The devFlowcharter project.
   The initial author of this file is Michal Domagala.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
}

unit XMLProcessor;

interface

{$R XSD_FILE.RES}

uses
   Vcl.Controls, Vcl.Dialogs, System.SysUtils, MSXML2_TLB, Base_Block, Types;

type

   TXMLProcessor = class(TObject)
   private
      class function DialogXMLFile(ADialog: TOpenDialog; const AFileName: string): string;
      class function GetXSD(const AResourceName: string): string;
   public
      class function ExportToXMLFile(AExportProc: TProc<IXMLDOMElement>; const AFilePath: string): TError;
      class function ImportFromXMLFile(AImportFunc: TFunc<IXMLDOMElement, TImportMode, TError>; AImportMode: TImportMode; const AFileName: string = ''; APreserveSpace: boolean = False): string;
      class function ImportFlowchartFromXML(ATag: IXMLDOMElement; AParent: TWinControl; APrevBlock: TBlock; ABranchIdx: integer; var AError: TError): TBlock;
   end;

implementation

uses
   Infrastructure, BlockFactory, BlockTabSheet, Constants, System.Classes;

class function TXMLProcessor.ImportFlowchartFromXML(ATag: IXMLDOMElement; AParent: TWinControl; APrevBlock: TBlock; ABranchIdx: integer; var AError: TError): TBlock;
var
   node: IXMLDOMNode;
   branch: TBranch;
   initCount: integer;
   tab: TBlockTabSheet;
   control: TControl;
   newBlock: TBlock;
begin
    result := nil;
    tab := nil;
    branch := nil;
    AError := errNone;
    Gerr_text := '';
    initCount := AParent.ControlCount;
    node := ATag;

    if AParent is TGroupBlock then
    begin
       if APrevBlock <> nil then
          branch := APrevBlock.ParentBranch
       else
          branch := TGroupBlock(AParent).GetBranch(ABranchIdx);
    end
    else if AParent is TBlockTabSheet then
       tab := TBlockTabSheet(AParent);

    while (node <> nil) and (AError = errNone) do
    begin
       if node.NodeName = BLOCK_TAG then
       begin
          newBlock := nil;
          if tab <> nil then
          begin
             newBlock := TBlockFactory.Create(node as IXMLDOMElement, tab);
             tab := nil;
          end
          else if branch <> nil then
          begin
             newBlock := TBlockFactory.Create(node as IXMLDOMElement, branch);
             if newBlock <> nil then
             begin
                branch.InsertAfter(newBlock, APrevBlock);
                APrevBlock := newBlock;
             end;
          end;
          if newBlock = nil then
             AError := errValidate
          else
             result := newBlock;
       end;
       node := node.NextSibling;
    end;

    if AError <> errNone then
    begin
       while initCount < AParent.ControlCount do
       begin
          control := AParent.Controls[initCount];
          if control is TBlock then
          begin
             var block := TBlock(control);
             if block.ParentBranch <> nil then
                block.ParentBranch.Remove(block);
          end;
          control.Destroy;
       end;
       result := nil;
    end;

end;

class function TXMLProcessor.DialogXMLFile(ADialog: TOpenDialog; const AFileName: string): string;
begin
   result := '';
   ADialog.Filter := trnsManager.GetString('XMLFilesFilter');
   ADialog.FileName := AFileName;
   if ADialog.Execute then
      result := ADialog.FileName;
end;

class function TXMLProcessor.ImportFromXMLFile(AImportFunc: TFunc<IXMLDOMElement, TImportMode, TError>; AImportMode: TImportMode; const AFileName: string = ''; APreserveSpace: boolean = False): string;
begin
   result := '';
   if Assigned(AImportFunc) then
   begin
      result := AFileName;
      if result.IsEmpty then
         result := DialogXMLFile(TInfra.GetMainForm.OpenDialog, '');
      if result.IsEmpty then
         Exit;
      var errText := '';
      var status := errSyntax;

      var xsdDoc := CoDOMDocument60.Create;
      xsdDoc.Async := False;
      xsdDoc.ValidateOnParse := False;
      if not xsdDoc.LoadXML(GetXSD('XSD_DEFINITION')) then
         ShowMessage(xsdDoc.ParseError.Reason);

      var schemaCache := CoXMLSchemaCache60.Create;
      schemaCache.Add('', xsdDoc);

      var xmlDoc := CoDOMDocument60.Create;
      xmlDoc.Async := False;
      xmlDoc.ValidateOnParse := True;
      xmlDoc.PreserveWhiteSpace := APreserveSpace;
      xmlDoc.Schemas := schemaCache;
      try
         if xmlDoc.Load(result) then
            status := AImportFunc(xmlDoc.DocumentElement, AImportMode)
         else with xmlDoc.ParseError do
            errText := trnsManager.GetFormattedString('ParserError', [ErrorCode, Line, LinePos, Reason]);
      except on E: Exception do
         begin
            status := errIO;
            errText := E.Message;
         end;
      end;
      if status <> errNone then
      begin
         if errText.IsEmpty then
            errText := GErr_text;
         errText := trnsManager.GetFormattedString('FileError', [result]) + sLineBreak + errText;
         TInfra.ShowErrorBox('ImportFailed', [sLineBreak, errText], errImport);
         result := '';
      end;
   end;
end;

class function TXMLProcessor.ExportToXMLFile(AExportProc: TProc<IXMLDOMElement>; const AFilePath: string): TError;
const
   XML_HEADER = 'version="1.0" encoding="UTF-8"';
begin
   result := errNone;
   if Assigned(AExportProc) then
   begin
      var filePath := AFilePath;
      if ExtractFilePath(filePath).IsEmpty then
         filePath := DialogXMLFile(TInfra.GetMainForm.ExportDialog, filePath);
      if not filePath.IsEmpty then
      begin
         if FileExists(filePath) and FileIsReadOnly(filePath) then
         begin
            TInfra.ShowErrorBox('SaveReadOnlyFile', [filePath], errIO);
            result := errIO;
         end
         else
         begin
            var xmlDoc := CoDOMDocument60.Create;
            var xmlInstr := xmlDoc.CreateProcessingInstruction('xml', XML_HEADER);
            xmlDoc.AppendChild(xmlInstr);
            var tag := xmlDoc.CreateElement('project');
            xmlDoc.AppendChild(tag);
            AExportProc(tag);
            try
               xmlDoc.Save(filePath);
            except on E: Exception do
               begin
                  result := errIO;
                  TInfra.ShowErrorBox('SaveError', [filePath, sLineBreak, E.Message], result);
               end;
            end;
         end;
      end;
   end;
end;

class function TXMLProcessor.GetXSD(const AResourceName: string): string;
begin
   var resourceStream := TResourceStream.Create(Hinstance, AResourceName, 'XSD_FILE');
   var stringStream := TStringStream.Create('', TEncoding.UTF8);
   try
      stringStream.CopyFrom(resourceStream);
      result := stringStream.DataString;
   finally
      stringStream.Free;
      resourceStream.Free;
   end;
end;

end.





