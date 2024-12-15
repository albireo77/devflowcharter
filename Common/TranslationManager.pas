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

unit TranslationManager;

interface

{$R ENGLISH_TRANSLATION.RES}

uses
   System.Classes, System.IniFiles, System.Generics.Collections;

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
   STATIC_TEXT  = 11;
   COMBO_BOX    = 12;

type

   TTranslationManager = class(TObject)
      private
         FRepository: TDictionary<string, string>;
         function LoadStaticLabels(AIniFile: TCustomIniFile): integer;
         function LoadDynamicLabels(AIniFile: TCustomIniFile): integer;
         function LoadLabels(AStream: TStream; ALoadDynamic, ALoadStatic: boolean): integer; overload;
      public
         constructor Create;
         destructor Destroy; override;
         function LoadLabels(const AFilename: string; ALoadDynamic: boolean = True; ALoadStatic: boolean = True): integer; overload;
         function LoadDefaultLabels(ALoadDynamic: boolean = True; ALoadStatic: boolean = True): integer;
         function GetString(const AKey: string): string;
         function GetFormattedString(const AKey: string; const Args: array of const): string;
         function GetJoinedString(const AJoiner: string; const AKeys: TArray<string>): string;
   end;

implementation

uses
   Vcl.StdCtrls, Vcl.Forms, System.SysUtils, Vcl.Dialogs, Vcl.Menus, Vcl.Buttons,
   System.StrUtils, Vcl.Controls, Base_Form;

type
   TControlHack = class(TControl);

constructor TTranslationManager.Create;
begin
   inherited Create;
   FRepository := TDictionary<string, string>.Create;
end;

destructor TTranslationManager.Destroy;
begin
   FRepository.Free;
   inherited Destroy;
end;

// This function loads labels that are needed during application use like error messages.
// In ini file section names with dynamic labels don't end with 'Form'.
function TTranslationManager.LoadDynamicLabels(AIniFile: TCustomIniFile): integer;
begin
   result := 0;
   FRepository.Clear;
   var sections := TStringList.Create;
   var values := TStringList.Create;
   try
      AIniFile.ReadSections(sections);
      for var section in sections do
      begin
         if not section.EndsWith('Form', True) then
         begin
            values.Clear;
            AIniFile.ReadSectionValues(section, values);
            for var a := 0 to values.Count-1 do
               FRepository.AddOrSetValue(values.Names[a], values.ValueFromIndex[a]);
            Inc(result, values.Count);
         end
      end;
   finally
      values.Free;
      sections.Free;
   end;
end;

// This function loads labels that are to be used only once. After labelling non-dynamic, visual component such label is no longer needed.
// In ini file section names with static labels end with 'Form'. One section per each application form.
function TTranslationManager.LoadStaticLabels(AIniFile: TCustomIniFile): integer;
begin
   result := 0;
   var sections := TStringList.Create;
   var values := TStringList.Create;
   try
      AIniFile.ReadSections(sections);
      for var section in sections do
      begin
         AIniFile.ReadSectionValues(section, values);
         var comp := Application.FindComponent(section);
         if comp is TBaseForm then
         begin
            var form := TBaseForm(comp);
            for var a := 0 to values.Count-1 do
            begin
               var field := '';
               var lName := values.Names[a];
               var pos := System.Pos('.', lName);
               if pos > 0 then
               begin
                  field := Copy(lName, pos+1);
                  SetLength(lName, pos-1);
               end;
               comp := form.FindComponent(lName);
               if comp <> nil then
               begin
                  var value := values.ValueFromIndex[a];
                  if SameText(field, 'Caption') then
                  begin
                     if comp is TMenuItem then
                        TMenuItem(comp).Caption := value
                     else if comp is TControl then
                        TControlHack(comp).Caption := value;
                  end
                  else if SameText(field, 'Text') then
                  begin
                     if comp is TControl then
                        TControlHack(comp).Text := value;
                  end
                  else if SameText(field, 'Hint') then
                  begin
                     if comp is TMenuItem then
                        TMenuItem(comp).Hint := value
                     else if comp is TControl then
                        TControl(comp).Hint := value;
                  end
                  else if SameText(field, 'Filter') then
                  begin
                     if comp is TOpenDialog then
                        TOpenDialog(comp).Filter := value;
                  end
                  else
                  begin
                     case comp.Tag of
                        MENU_ITEM:    TMenuItem(comp).Caption := value;
                        DIALOG:       TOpenDialog(comp).Filter := value;
                        EDIT_TEXT:    TEdit(comp).Text := value;
                        EDIT_HINT,
                        SPEED_BUTTON: TControlHack(comp).Hint := value;
                        COMBO_BOX:
                        begin
                           pos := StrToIntDef(field, -1);
                           if (pos >= 0) and (pos < TComboBox(comp).Items.Count) then
                              TComboBox(comp).Items[pos] := value;
                        end
                        else
                           TControlHack(comp).Caption := value;
                     end;
                  end;
               end;
            end;
            form.AfterTranslation(values);
            Inc(result, values.Count);
         end;
         values.Clear;
      end;
   finally
      sections.Free;
      values.Free;
   end;
end;

function TTranslationManager.LoadDefaultLabels(ALoadDynamic: boolean = True; ALoadStatic: boolean = True): integer;
begin
   result := LoadLabels(TResourceStream.Create(Hinstance, 'DEFAULT_TRANSLATION_FILE', 'LNG_FILE'), ALoadDynamic, ALoadStatic);
end;

function TTranslationManager.LoadLabels(const AFileName: string; ALoadDynamic: boolean = True; ALoadStatic: boolean = True): integer;
begin
   result := 0;
   if FileExists(AFileName) then
      result := LoadLabels(TFileStream.Create(AFileName, fmOpenRead), ALoadDynamic, ALoadStatic);
end;

function TTranslationManager.LoadLabels(AStream: TStream; ALoadDynamic, ALoadStatic: boolean): integer;
begin
   result := 0;
   var iniFile := TMemIniFile.Create(AStream);
   try
      if ALoadDynamic then
         result := LoadDynamicLabels(iniFile);
      if ALoadStatic then
         Inc(result, LoadStaticLabels(iniFile));
   finally
      iniFile.Free;
      AStream.Free;
   end;
end;

function TTranslationManager.GetString(const AKey: string): string;
begin
   if not FRepository.TryGetValue(AKey, result) then
      result := AKey;
end;

function TTranslationManager.GetFormattedString(const AKey: string; const Args: array of const): string;
begin
   result := Format(GetString(AKey), Args);
end;

function TTranslationManager.GetJoinedString(const AJoiner: string; const AKeys: TArray<string>): string;
begin
   result := '';
   for var i := 0 to High(AKeys) do
      result := result + IfThen(i > 0, AJoiner) + GetString(AKeys[i]);
end;

end.

