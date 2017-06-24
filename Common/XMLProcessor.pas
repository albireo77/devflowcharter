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



{ This unit contains routines to read/write XML files }

unit XMLProcessor;

interface

uses
   Vcl.Controls, Vcl.Dialogs, OmniXML, Base_Block, CommonTypes;

type

   TXMLExportProc = procedure(ATag: IXMLElement) of object;
   TXMLImportProc = function(ATag: IXMLElement; ASelect: boolean = false): TErrorType of object;

   TXMLProcessor = class(TObject)
   private
      class function DialogXMLFile(ADialog: TOpenDialog; const AFileName: string = ''): string;
   public
      class function ExportToXMLFile(AExportProc: TXMLExportProc; const AFilePath: string = ''): TErrorType;
      class function ImportFromXMLFile(AImportProc: TXMLImportProc; const AFileName: string = ''; APreserveSpace: boolean = false): string;
      class function FindChildTag(ATag: IXMLElement; const AName: string): IXMLElement;
      class function FindNextTag(ATag: IXMLElement): IXMLElement;
      class procedure AddText(ATag: IXMLElement; const AText: string);
      class procedure AddCDATA(ATag: IXMLElement; const AText: string);
      class procedure ExportBlockToXML(ABlock: TBlock; ATag: IXMLElement);
      class function CountChildTags(ATag: IXMLElement; const AChildTagName: string; AWithText: boolean = false): integer;
      class function GetBoolFromChildTag(ATag: IXMLElement; const ATagName: string; ADefault: boolean = false): boolean;
      class function ImportFlowchartFromXMLTag(ATag: IXMLElement;
                                               AParent: TWinControl;
                                               APrevBlock: TBlock;
                                               var AErrorType: TErrorType;
                                               ABranchInd: integer = PRIMARY_BRANCH_IND): TBlock;
   end;

const
   XML_HEADER = 'version="1.0" encoding="UTF-8"';

implementation

uses
   System.SysUtils, ApplicationCommon, BlockFactory, BlockTabSheet;

class function TXMLProcessor.FindChildTag(ATag: IXMLElement; const AName: string): IXMLElement;
var
   node: IXMLNode;
begin
    result := nil;
    if (ATag <> nil) and ATag.HasChildNodes then
    begin
        node := ATag.FirstChild;
        while node <> nil do
        begin
            if node.NodeName = AName then
            begin
               result := node as IXMLElement;
               break;
            end;
            node := node.NextSibling;
        end;
    end;
end;

class function TXMLProcessor.FindNextTag(ATag: IXMLElement): IXMLElement;
var
   node: IXMLNode;
begin
    result := nil;
    node := nil;
    if ATag <> nil then
       node := ATag.NextSibling;
    while node <> nil do
    begin
        if node.NodeName = ATag.NodeName then
        begin
           result := node as IXMLElement;
           break;
        end;
        node := node.NextSibling;
    end;
end;

class function TXMLProcessor.GetBoolFromChildTag(ATag: IXMLElement; const ATagName: string; ADefault: boolean = false): boolean;
var
   ctag: IXMLElement;
   i: integer;
   ctext: string;
begin
   result := ADefault;
   ctag := FindChildTag(ATag, ATagName);
   if ctag <> nil then
   begin
      ctext := Trim(ctag.Text);
      if TryStrToInt(ctext, i) then
         result := i <> 0
      else if SameText('true', ctext) then
         result := true
      else if SameText('false', ctext) then
         result := false;
   end;
end;

class procedure TXMLProcessor.AddText(ATag: IXMLElement; const AText: string);
begin
   if (ATag <> nil) and (AText <> '') then
      ATag.AppendChild(ATag.OwnerDocument.CreateTextNode(AText));
end;

class procedure TXMLProcessor.AddCDATA(ATag: IXMLElement; const AText: string);
begin
   if (ATag <> nil) and (AText <> '') then
      ATag.AppendChild(ATag.OwnerDocument.CreateCDATASection(AText));
end;

class function TXMLProcessor.CountChildTags(ATag: IXMLElement; const AChildTagName: string; AWithText: boolean = false): integer;
var
   tag: IXMLElement;
begin
   result := 0;
   tag := FindChildTag(ATag, AChildTagName);
   while tag <> nil do
   begin
      if not (AWithText and (Trim(tag.Text) = '')) then
         result := result + 1;
      tag := FindNextTag(tag);
   end;
end;

class procedure TXMLProcessor.ExportBlockToXML(ABlock: TBlock; ATag: IXMLElement);
var
   tag: IXMLElement;
begin
   if (ATag <> nil) and (ABlock <> nil) then
   begin
      tag := ATag.OwnerDocument.CreateElement(BLOCK_TAG);
      ATag.AppendChild(tag);
      ABlock.SaveInXML(tag);
   end;
end;

class function TXMLProcessor.ImportFlowchartFromXMLTag(ATag: IXMLElement;      // root XML tag
                                                       AParent: TWinControl;       // Parent window for new block
                                                       APrevBlock: TBlock;
                                                       var AErrorType: TErrorType;
                                                       ABranchInd: integer = PRIMARY_BRANCH_IND): TBlock;
var
   tag: IXMLElement;
   newBlock: TBlock;
   branch: TBranch;
   initCount: integer;
   tab: TBlockTabSheet;
   control: TControl;
begin
    result := nil;
    tab := nil;
    branch := nil;
    AErrorType := errNone;
    Gerr_text := '';
    initCount := AParent.ControlCount;
    tag := ATag;

    if AParent is TGroupBlock then
    begin
       if APrevBlock <> nil then                                  // predBlock is not nil so newBlock will be put into list
          branch := APrevBlock.ParentBranch                       // containing predBlock, just after predBlock
       else                                                      // predBlock is nil so newBlock will be put at the beginning of the list
          branch := TGroupBlock(AParent).GetBranch(ABranchInd);   // branch is determined by branch_id
    end
    else if AParent is TBlockTabSheet then
       tab := TBlockTabSheet(AParent);

    while tag <> nil do
    begin
       newBlock := TBlockFactory.Create(tag, branch, tab);
       tab := nil;
       if newBlock <> nil then
       begin
          result := newBlock;
          if branch <> nil then
          begin
             branch.InsertAfter(newBlock, APrevBlock);
             APrevBlock := newBlock;
          end;
       end
       else
       begin
          AErrorType := errValidate;
          break;
       end;
       tag := FindNextTag(tag);
    end;

    if AErrorType <> errNone then
    begin
       while initCount < AParent.ControlCount do   // destroy all previously created blocks
       begin
          branch := nil;
          control := AParent.Controls[initCount];
          if control is TBlock then
             branch := TBlock(control).ParentBranch;
          if branch <> nil then
             branch.Remove(control);
          control.Destroy;
       end;
       result := nil;
    end;

end;

class function TXMLProcessor.DialogXMLFile(ADialog: TOpenDialog; const AFileName: string = ''): string;
begin
   result := '';
   ADialog.Filter := i18Manager.GetString('XMLFilesFilter');
   ADialog.FileName := AFileName;
   if ADialog.Execute then
      result := ADialog.FileName;
end;

class function TXMLProcessor.ImportFromXMLFile(AImportProc: TXMLImportProc; const AFileName: string = ''; APreserveSpace: boolean = false): string;
var
   docXML: IXMLDocument;
   errText: string;
   status: TErrorType;
begin
   result := '';
   if Assigned(AImportProc) then
   begin
      result := AFileName;
      if result = '' then
         result := TXMLProcessor.DialogXMLFile(TInfra.GetMainForm.OpenDialog);
      if result = '' then
         exit;
      docXML := CreateXMLDoc;
      docXML.PreserveWhiteSpace := APreserveSpace;
      try
         if docXML.Load(result) then
            status := AImportProc(docXML.DocumentElement)
         else
         begin
            status := errSyntax;
            with docXML.ParseError do
               errText := i18Manager.GetFormattedString('ParserError', [ErrorCode, Line, LinePos, Reason]);
         end;
      except on E: Exception do
         begin
            status := errIO;
            errText := E.Message;
         end;
      end;
      if status <> errNone then
      begin
         errText := i18Manager.GetFormattedString('FileError', [result]) + CRLF + errText;
         TInfra.ShowFormattedErrorBox('ImportFailed', [CRLF, errText], errImport);
         result := '';
      end;
   end;
end;

class function TXMLProcessor.ExportToXMLFile(AExportProc: TXMLExportProc; const AFilePath: string = ''): TErrorType;
var
   docXML: IXMLDocument;
   xmlInstr: IXMLProcessingInstruction;
   tag: IXMLElement;
   filePath: string;
begin
   result := errNone;
   if Assigned(AExportProc) then
   begin
      if ExtractFilePath(AFilePath) = '' then
         filePath := TXMLProcessor.DialogXMLFile(TInfra.GetMainForm.ExportDialog, AFilePath)
      else
         filePath := AFilePath;
      if FileExists(filePath) and FileIsReadOnly(filePath) then
      begin
         TInfra.ShowFormattedErrorBox('SaveReadOnlyFile', [filePath], errIO);
         result := errIO;
      end
      else if filePath <> '' then
      begin
         docXML := CreateXMLDoc;
         xmlInstr := docXML.CreateProcessingInstruction('xml', XML_HEADER);
         docXML.AppendChild(xmlInstr);
         tag := docXML.CreateElement('project');
         docXML.AppendChild(tag);
         AExportProc(tag);
         try
            docXML.Save(filePath, ofIndent);
         except on E: Exception do
            begin
               result := errIO;
               TInfra.ShowFormattedErrorBox('SaveError', [filePath, CRLF, E.Message], result);
            end;
         end;
      end;
   end;
end;

end.





