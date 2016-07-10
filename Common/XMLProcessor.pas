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
      class function ExportToXMLFile(const AFilename: string; AExportProc: TExportProc): TErrorType;
      class function ImportFromXMLFile(const AFilename: string; AImportProc: TImportProc; const APreserveSpace: boolean = false): TErrorType;
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
   ApplicationCommon, BlockFactory, BlockTabSheet;

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

class function TXMLProcessor.ImportFromXMLFile(const AFilename: string; AImportProc: TImportProc; const APreserveSpace: boolean = false): TErrorType;
var
   docXML: IXMLDocument;
begin
   docXML := CreateXMLDoc;
   docXML.PreserveWhiteSpace := APreserveSpace;
   try
      if docXML.Load(AFilename) then
         result := AImportProc(docXML.DocumentElement)
      else
      begin
         result := errSyntax;
         with docXML.ParseError do
            Gerr_text := i18Manager.GetFormattedString('ParserError', [ErrorCode, Line, LinePos, Reason]);
      end;
   except on E: Exception do
      begin
         Gerr_text := E.Message;
         result := errIO;
      end;
   end;
   if result <> errNone then
      Gerr_text := i18Manager.GetFormattedString('FileError', [ExpandFileName(AFileName)]) + CRLF + Gerr_text;
end;

class function TXMLProcessor.ExportToXMLFile(const AFilename: string; AExportProc: TExportProc): TErrorType;
var
   docXML: IXMLDocument;
   lInstr: IXMLProcessingInstruction;
   lRootTag: IXMLElement;
begin
   result := errNone;
   docXML := CreateXMLDoc;
   lInstr := docXML.CreateProcessingInstruction('xml', XML_HEADER);
   docXML.AppendChild(lInstr);
   lRootTag := docXML.CreateElement('project');
   docXML.AppendChild(lRootTag);
   AExportProc(lRootTag);
   try
      docXML.Save(AFilename, ofIndent);
   except
      result := errIO;
   end;
end;

end.





