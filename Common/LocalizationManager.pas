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

unit LocalizationManager;

interface

{$R ENGLISH_LOC.RES}

uses
   StdCtrls, Classes, IniFiles, Forms;

const
   BUTTON       = 1;
   MENU_ITEM    = 2;
   DIALOG       = 3;
   GROUP_BOX    = 4;
   EDIT_HINT    = 5;
   LABELL       = 6;
   RADIO_BUTTON = 7;
   CHECK_BOX    = 8;
   SPEED_BUTTON = 9;
   EDIT_TEXT    = 10;
   
type

   Ti18Manager = class(TObject)
      private
         FRepository: TStringList;
      public
         constructor Create;
         destructor Destroy; override;
         function GetString(const AKey: string): string;
         function LoadDefaultLabels: integer;
         function GetFormattedString(const AKey: string; Args: array of const): string;
         function LoadStaticLabels(const AFileName: string): integer;
         function LoadDynamicLabels(const AFileName: string; const AClearRepository: boolean = false): integer;
         function LoadAllLabels(const AFilename: string): integer;
   end;

implementation

uses
   SysUtils, Dialogs, Menus, Base_Form, Buttons, Windows, StrUtils, ApplicationCommon, Controls;

type
   THackControl = class(TControl);

constructor Ti18Manager.Create;
begin
   inherited Create;
   FRepository := TStringList.Create;
end;

destructor Ti18Manager.Destroy;
begin
   FRepository.Free;
   inherited Destroy;
end;

// this function load labels that are needed all the time during application use (e.g. error message to be displayed
// on incorrect action); in ini file section names with dynamic labels don't end with 'Form'
function Ti18Manager.LoadDynamicLabels(const AFileName: string; const AClearRepository: boolean = false): integer;
var
   lKeys, lSections: TStringList;
   i: integer;
   lIniFile: TIniFile;
begin
   result := 0;
   if FileExists(AFileName) then
   begin
      lSections := TStringList.Create;
      lKeys := TStringList.Create;
      lInifile := TIniFile.Create(AFilename);
      try
         lInifile.ReadSections(lSections);
         if lSections.Count > 0 then
         begin
            if AClearRepository then
               FRepository.Clear;
            FRepository.Sorted := false;
            for i := 0 to lSections.Count-1 do
            begin
               if not AnsiEndsText('Form', lSections[i]) then
               begin
                  lInifile.ReadSectionValues(lSections[i], lKeys);
                  FRepository.AddStrings(lKeys);
                  result := result + lKeys.Count;
                  lKeys.Clear;
               end
            end;
         end;
      finally
         lKeys.Free;
         lSections.Free;
         lIniFile.Free;
      end;
   end;
   FRepository.Sort;
end;

// this function load labels that are to be used only once (e.g. button caption); after labelling visual component,
// such label is no longer needed; it is important to call this function when all application's forms are already created;
// in ini file section names with static labels end with 'Form' - one section for each application form
function Ti18Manager.LoadStaticLabels(const AFileName: string): integer;
var
   lComponent: TComponent;
   i, a, lPos: integer;
   lKeys, lSections: TStringList;
   lBaseForm: TBaseForm;
   lValue, lName, lField: string;
   lIniFile: TIniFile;
begin
   result := 0;
   if FileExists(AFileName) then
   begin
      lSections := TStringList.Create;
      lKeys := TStringList.Create;
      lInifile := TIniFile.Create(AFilename);
      try
         lIniFile.ReadSections(lSections);
         if lSections.Count > 0 then
         begin
            for i := 0 to lSections.Count-1 do
            begin
               lIniFile.ReadSectionValues(lSections[i], lKeys);
               lComponent := Application.FindComponent(lSections[i]);
               if lComponent is TBaseForm then
               begin
                  lBaseForm := TBaseForm(lComponent);
                  for a := 0 to lKeys.Count-1 do
                  begin
                     lField := '';
                     lName := lKeys.Names[a];
                     lPos := AnsiPos('.', lName);
                     if lPos > 0 then
                     begin
                        lField := AnsiRightStr(lName, Length(lName)-lPos);
                        lName := AnsiLeftStr(lName, lPos-1);
                     end;
                     lComponent := lBaseForm.FindComponent(lName);
                     if lComponent <> nil then
                     begin
                        lValue := lKeys.ValueFromIndex[a];
                        if AnsiSameText(lField, 'Caption') then
                        begin
                           if lComponent is TMenuItem then
                              TMenuItem(lComponent).Caption := lValue
                           else if lComponent is TControl then
                              THackControl(lComponent).Caption := lValue;
                        end
                        else if AnsiSameText(lField, 'Text') then
                        begin
                           if lComponent is TControl then
                              THackControl(lComponent).Text := lValue;
                        end
                        else if AnsiSameText(lField, 'Hint') then
                        begin
                           if lComponent is TControl then
                              TControl(lComponent).Hint := lValue;
                        end
                        else if AnsiSameText(lField, 'Filter') then
                        begin
                           if lComponent is TOpenDialog then
                              TOpenDialog(lComponent).Filter := lValue;
                        end
                        else
                        begin
                           case lComponent.Tag of
                              BUTTON:       TButton(lComponent).Caption := lValue;
                              MENU_ITEM:    TMenuItem(lComponent).Caption := lValue;
                              DIALOG:       TOpenDialog(lComponent).Filter := lValue;
                              GROUP_BOX:    TGroupBox(lComponent).Caption := lValue;
                              EDIT_HINT:    TEdit(lComponent).Hint := lValue;
                              EDIT_TEXT:    TEdit(lComponent).Text := lValue;
                              LABELL:       TLabel(lComponent).Caption := lValue;
                              RADIO_BUTTON: TRadioButton(lComponent).Caption := lValue;
                              CHECK_BOX:    TCheckBox(lComponent).Caption := lValue;
                              SPEED_BUTTON: TSpeedButton(lComponent).Hint := lValue;
                           end;
                        end;
                     end;
                  end;
                  lBaseForm.Localize(lKeys);
                  result := result + lKeys.Count;
               end;
               lKeys.Clear;
            end;
         end;
      finally
         lSections.Free;
         lKeys.Free;
         lIniFile.Free;
      end;
   end;
end;

function Ti18Manager.LoadDefaultLabels: integer;
var
   lResStream: TResourceStream;
   lLangFile: string;
   lTmpPath: array[0..MAX_PATH] of Char;
begin
   GetTempPath(SizeOf(lTmpPath)-1, lTmpPath);
   lLangFile := lTmpPath + 'english.lng';
   lResStream := TResourceStream.Create(Hinstance, 'DEFAULT_LOCALIZATION_FILE', 'LNG_FILE');
   try
      try
         lResStream.SaveToFile(lLangFile);
         result := LoadAllLabels(lLangFile);
      except on E: EFCreateError do
         begin
            Application.MessageBox(PChar('Could not create default translation file ' + lLangFile + ':' + CRLF + E.Message), 'IO Error', MB_ICONERROR);
            result := 0;
         end;
      end;
   finally
      SysUtils.DeleteFile(lLangFile);
      lResStream.Free;
   end;
end;

function Ti18Manager.GetString(const AKey: string): string;
begin
   result := FRepository.Values[AKey];
   if result = '' then
      result := AKey;
end;



function Ti18Manager.LoadAllLabels(const AFilename: string): integer;
begin
   FRepository.Clear;
   result := LoadStaticLabels(AFilename);
   result := result + LoadDynamicLabels(AFilename);
end;

function Ti18Manager.GetFormattedString(const AKey: string; Args: array of const): string;
begin
   result := Format(GetString(AKey), Args);
end;

end.