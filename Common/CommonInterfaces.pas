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



unit CommonInterfaces;

interface

uses
   Classes, Controls, StdCtrls, OmniXML, Graphics, Types, ComCtrls, Forms;

const
   ID_INVALID      = -1;

   NO_SORT         = -1;
   PAGE_INDEX_SORT = 0;
   Z_ORDER_SORT    = 1;

type

   TFocusInfo = record
      LineText,
      SelText: string;
      Line,
      RelativeLine,
      SelStart: integer;
      FocusEdit: TCustomEdit;
      FocusEditForm: TForm;
      FocusEditCallBack: procedure(AEdit: TCustomEdit) of object;
      ActiveControl: TWinControl;
   end;

   IIdentifiable = interface
      ['{A7ED5085-D43B-49B3-879E-272A64A766B7}']
      function GetId: integer;
      property Id: integer read GetId;
   end;

   ISortable = interface
      ['{67CF9A42-5F40-4514-A352-E7FE29F8CF43}']
      function GetSortValue(const ASortType: integer): integer;
   end;

   IActivable = interface
      ['{FC98CF4A-67AE-4111-AE9A-38C3F5FE861E}']
      function GetActive: boolean;
      procedure SetActive(const AValue: boolean);
      property Active: boolean read GetActive write SetActive;
   end;

   IXMLable = interface(IActivable)
      ['{371696A8-8B69-4293-8A9B-B00ACEE315C6}']
      procedure ExportToXMLTag(const rootTag: IXMLElement);
      procedure ImportFromXMLTag(const rootTag: IXMLElement; const APinControl: TControl = nil);
   end;

   ITabbable = interface(IActivable)
      ['{1E316044-89E6-4FFB-AC4D-FA773D4774D8}']
      function GetName: string;
      function GetLibName: string;
   end;

   ISizeEditable = interface
      ['{B711195E-B798-49D4-8C8E-159F81C42EA2}']
      procedure RefreshSizeEdits;
   end;

   IWinControl = interface
      ['{83E56064-8CD4-4E91-BB66-47EDBCF2C697}']
      procedure PaintToCanvas(const ACanvas: TCanvas);
      function GetHandle: THandle;
      procedure BringAllToFront;
      procedure SetZOrder(const AValue: integer);
      function GetZOrder: integer;
   end;

   IMaxBoundable = interface
      ['{4C655B74-7C42-47BD-B443-B1A15B237912}']
      function GetMaxBounds: TPoint;
   end;

   IFocusable = interface
      ['{35418E64-5114-4412-9913-B58489A8E499}']
      function RetrieveFocus(AInfo: TFocusInfo): boolean;
      function CanBeFocused: boolean;
      function GetFocusColor: TColor;
      function Remove: boolean;
      function CanBeRemoved: boolean;
      function IsBoldDesc: boolean;
   end;

   IIterator = interface
      ['{7F35650E-FF9C-4D33-8A09-7DF81DC30474}']
      function GetCount: integer;
      function HasNext: boolean;
      function Next: TObject;
      function GetObjectIndex(const AObject: TObject): integer;
      procedure Reverse;
      property Count: integer read GetCount;
   end;

implementation

end.
