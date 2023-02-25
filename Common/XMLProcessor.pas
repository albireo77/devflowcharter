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

uses
   Vcl.Controls, Vcl.Dialogs, OmniXML, Base_Block, Types;

type

   TXMLExportProc = procedure(ANode: IXMLNode) of object;
   TXMLImportProc = function(ANode: IXMLNode; AImportMode: TImportMode): TError of object;

   TXMLProcessor = class(TObject)
   private
      class function DialogXMLFile(ADialog: TOpenDialog; const AFileName: string): string;
   public
      class function ExportToXMLFile(AExportProc: TXMLExportProc; const AFilePath: string): TError;
      class function ImportFromXMLFile(AImportProc: TXMLImportProc; AImportMode: TImportMode; const AFileName: string = ''; APreserveSpace: boolean = False): string;
      class function ImportFlowchartFromXML(ANode: IXMLNode; AParent: TWinControl; APrevBlock: TBlock; ABranchIdx: integer; var AError: TError): TBlock;
   end;

const
   XML_HEADER = 'version="1.0" encoding="UTF-8"';

implementation

uses
   System.SysUtils, Infrastructure, BlockFactory, BlockTabSheet, Constants;

class function TXMLProcessor.ImportFlowchartFromXML(ANode: IXMLNode; AParent: TWinControl; APrevBlock: TBlock; ABranchIdx: integer; var AError: TError): TBlock;
var
   node: IXMLNode;
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
    node := ANode;

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
             newBlock := TBlockFactory.Create(node, tab);
             tab := nil;
          end
          else if branch <> nil then
          begin
             newBlock := TBlockFactory.Create(node, branch);
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
   ADialog.Filter := i18Manager.GetString('XMLFilesFilter');
   ADialog.FileName := AFileName;
   if ADialog.Execute then
      result := ADialog.FileName;
end;

class function TXMLProcessor.ImportFromXMLFile(AImportProc: TXMLImportProc; AImportMode: TImportMode; const AFileName: string = ''; APreserveSpace: boolean = False): string;
begin
   result := '';
   if Assigned(AImportProc) then
   begin
      result := AFileName;
      if result.IsEmpty then
         result := DialogXMLFile(TInfra.GetMainForm.OpenDialog, '');
      if result.IsEmpty then
         Exit;
      var errText := '';
      var status := errSyntax;
      var docXML := CreateXMLDoc;
      docXML.PreserveWhiteSpace := APreserveSpace;
      try
         if docXML.Load(result) then
            status := AImportProc(docXML.DocumentElement, AImportMode)
         else with docXML.ParseError do
            errText := i18Manager.GetFormattedString('ParserError', [ErrorCode, Line, LinePos, Reason]);
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
         errText := i18Manager.GetFormattedString('FileError', [result]) + sLineBreak + errText;
         TInfra.ShowErrorBox('ImportFailed', [sLineBreak, errText], errImport);
         result := '';
      end;
   end;
end;

class function TXMLProcessor.ExportToXMLFile(AExportProc: TXMLExportProc; const AFilePath: string): TError;
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
            var docXML := CreateXMLDoc;
            var xmlInstr := docXML.CreateProcessingInstruction('xml', XML_HEADER);
            docXML.AppendChild(xmlInstr);
            var tag := docXML.CreateElement('project');
            docXML.AppendChild(tag);
            AExportProc(tag);
            try
               docXML.Save(filePath, ofIndent);
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

end.





