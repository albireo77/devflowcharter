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
   Windows, Controls, Forms, StdCtrls, SysUtils, Classes, StrUtils, OmniXML, Base_Block, CommonTypes, Graphics;

type

   TExportProc = procedure(const root: IXMLElement) of object;
   TImportProc = function(const root: IXMLElement): TErrorType of object;

   TXMLProcessor = class(TObject)
   public
      class function ExportToXMLFile(AExportProc: TExportProc; const AFilePath: string = ''): TErrorType;
      class function ImportFromXMLFile(AImportProc: TImportProc; const AFileName: string = ''; const APreserveSpace: boolean = false): string;
      class function FindChildTag(const ATag: IXMLElement; const AName: string): IXMLElement;
      class function FindNextTag(const ATag: IXMLElement): IXMLElement;
      class procedure AddText(const ATag: IXMLElement; const AText: string);
      class procedure AddCDATA(const ATag: IXMLElement; const AText: string);
      class procedure ExportBlockToXML(const ABlock: TBlock; const ATag: IXMLElement);
      class function CountChildTags(const ATag: IXMLElement; const AChildTagName: string; const AWithText: boolean = false): integer;
      class function ImportFlowchartFromXMLTag(const ATag: IXMLElement;
                                               const AParent: TWinControl;
                                               APrevBlock: TBlock;
                                               var AErrorType: TErrorType;
                                               const ABranchInd: integer = PRIMARY_BRANCH_IND): TBlock;
   end;

const
   XML_HEADER = 'version="1.0" encoding="UTF-8"';

implementation

uses
   ApplicationCommon, BlockFactory, BlockTabSheet, Dialogs;

class function TXMLProcessor.FindChildTag(const ATag: IXMLElement; const AName: string): IXMLElement;
var
   lNode: IXMLNode;
begin
    result := nil;
    if (ATag <> nil) and ATag.HasChildNodes then
    begin
        lNode := ATag.FirstChild;
        while lNode <> nil do
        begin
            if lNode.NodeName = AName then
            begin
               result := lNode as IXMLElement;
               break;
            end;
            lNode := lNode.NextSibling;
        end;
    end;
end;

class function TXMLProcessor.FindNextTag(const ATag: IXMLElement): IXMLElement;
var
   lNode: IXMLNode;
begin
    result := nil;
    lNode := nil;
    if ATag <> nil then
       lNode := ATag.NextSibling;
    while lNode <> nil do
    begin
        if lNode.NodeName = ATag.NodeName then
        begin
           result := lNode as IXMLElement;
           break;
        end;
        lNode := lNode.NextSibling;
    end;
end;

class procedure TXMLProcessor.AddText(const ATag: IXMLElement; const AText: string);
begin
   if (ATag <> nil) and (AText <> '') then
      ATag.AppendChild(ATag.OwnerDocument.CreateTextNode(AText));
end;

class procedure TXMLProcessor.AddCDATA(const ATag: IXMLElement; const AText: string);
begin
   if (ATag <> nil) and (AText <> '') then
      ATag.AppendChild(ATag.OwnerDocument.CreateCDATASection(AText));
end;

class function TXMLProcessor.CountChildTags(const ATag: IXMLElement; const AChildTagName: string; const AWithText: boolean = false): integer;
var
   lTag: IXMLElement;
begin
   result := 0;
   lTag := FindChildTag(ATag, AChildTagName);
   while lTag <> nil do
   begin
      result := result + 1;
      if AWithText and (Trim(lTag.Text) = '') then
         result := result - 1;
      lTag := FindNextTag(lTag);
   end;
end;

class procedure TXMLProcessor.ExportBlockToXML(const ABlock: TBlock; const ATag: IXMLElement);
var
   lTag: IXMLElement;
begin
   if (ATag <> nil) and (ABlock <> nil) then
   begin
      lTag := ATag.OwnerDocument.CreateElement(BLOCK_TAG);
      ATag.AppendChild(lTag);
      ABlock.SaveInXML(lTag);
   end;
end;

class function TXMLProcessor.ImportFlowchartFromXMLTag(const ATag: IXMLElement;      // root XML tag
                                                       const AParent: TWinControl;       // Parent window for new block
                                                       APrevBlock: TBlock;
                                                       var AErrorType: TErrorType;
                                                       const ABranchInd: integer = PRIMARY_BRANCH_IND): TBlock;
var
   lTag: IXMLElement;
   lNewBlock: TBlock;
   lBranch: TBranch;
   initCount: integer;
   lTab: TBlockTabSheet;
   lControl: TControl;
begin
    result := nil;
    lTab := nil;
    lBranch := nil;
    AErrorType := errNone;
    Gerr_text := '';
    initCount := AParent.ControlCount;
    lTag := ATag;

    if AParent is TGroupBlock then
    begin
       if APrevBlock <> nil then                                  // predBlock is not nil so newBlock will be put into list
          lBranch := APrevBlock.ParentBranch                       // containing predBlock, just after predBlock
       else                                                      // predBlock is nil so newBlock will be put at the beginning of the list
          lBranch := TGroupBlock(AParent).GetBranch(ABranchInd);   // branch is determined by branch_id
    end
    else if AParent is TBlockTabSheet then
       lTab := TBlockTabSheet(AParent);

    while lTag <> nil do
    begin
       lNewBlock := TBlockFactory.Create(lTag, lBranch, lTab);
       lTab := nil;
       if lNewBlock <> nil then
       begin
          result := lNewBlock;
          if lBranch <> nil then
          begin
             lBranch.InsertAfter(lNewBlock, APrevBlock);
             APrevBlock := lNewBlock;
          end;
       end
       else
       begin
          AErrorType := errValidate;
          break;
       end;
       lTag := FindNextTag(lTag);
    end;

    if AErrorType <> errNone then
    begin
       while initCount < AParent.ControlCount do   // destroy all previously created blocks
       begin
          lBranch := nil;
          lControl := AParent.Controls[initCount];
          if lControl is TBlock then
             lBranch := TBlock(lControl).ParentBranch;
          if lBranch <> nil then
             lBranch.Remove(lControl);
          lControl.Destroy;
       end;
       result := nil;
    end;

end;

class function TXMLProcessor.ImportFromXMLFile(AImportProc: TImportProc; const AFileName: string = ''; const APreserveSpace: boolean = false): string;
var
   docXML: IXMLDocument;
   lDialog: TOpenDialog;
   lErrText: string;
   status: TErrorType;
begin
   result := '';
   if Assigned(AImportProc) then
   begin
      result := AFileName;
      if result = '' then
      begin
         lDialog := TInfra.GetMainForm.OpenDialog;
         lDialog.Filter := i18Manager.GetString('XMLFilesFilter');
         lDialog.FileName := '';
         if lDialog.Execute then
            result := lDialog.FileName
         else
            exit;
      end;
      docXML := CreateXMLDoc;
      docXML.PreserveWhiteSpace := APreserveSpace;
      try
         if docXML.Load(result) then
            status := AImportProc(docXML.DocumentElement)
         else
         begin
            status := errSyntax;
            with docXML.ParseError do
               lErrText := i18Manager.GetFormattedString('ParserError', [ErrorCode, Line, LinePos, Reason]);
         end;
      except on E: Exception do
         begin
            status := errIO;
            lErrText := E.Message;
         end;
      end;
      if status <> errNone then
      begin
         lErrText := i18Manager.GetFormattedString('FileError', [result]) + CRLF + lErrText;
         TInfra.ShowFormattedErrorBox('ImportFailed', [CRLF, lErrText], errImport);
         result := '';
      end;
   end;
end;

class function TXMLProcessor.ExportToXMLFile(AExportProc: TExportProc; const AFilePath: string = ''): TErrorType;
var
   docXML: IXMLDocument;
   lInstr: IXMLProcessingInstruction;
   lTag: IXMLElement;
   lDialog: TSaveDialog;
   lFilePath: string;
begin
   result := errNone;
   if Assigned(AExportProc) then
   begin
      if ExtractFilePath(AFilePath) = '' then
      begin
         lDialog := TInfra.GetMainForm.ExportDialog;
         lDialog.FileName := AFilePath;
         lDialog.Filter := i18Manager.GetString('XMLFilesFilter');
         if lDialog.Execute then
            lFilePath := lDialog.FileName
         else
            exit;
      end
      else
         lFilePath := AFilePath;
      if FileExists(lFilePath) and FileIsReadOnly(lFilePath) then
      begin
         TInfra.ShowFormattedErrorBox('SaveReadOnlyFile', [lFilePath], errIO);
         result := errIO;
      end
      else
      begin
         docXML := CreateXMLDoc;
         lInstr := docXML.CreateProcessingInstruction('xml', XML_HEADER);
         docXML.AppendChild(lInstr);
         lTag := docXML.CreateElement('project');
         docXML.AppendChild(lTag);
         AExportProc(lTag);
         try
            docXML.Save(lFilePath, ofIndent);
         except on E: Exception do
            begin
               result := errIO;
               TInfra.ShowFormattedErrorBox('SaveError', [lFilePath, CRLF, E.Message], result);
            end;
         end;
      end;
   end;
end;

end.





