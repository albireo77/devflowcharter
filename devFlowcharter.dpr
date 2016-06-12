{
   Copyright (C) 2006 The devFlowcharter project 
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



program devFlowcharter;

uses
  FastMM4,
  Forms,
  ApplicationCommon in 'Common\ApplicationCommon.pas',
  Assign_Block in 'Blocks\Assign_Block.pas',
  Base_Block in 'Blocks\Base_Block.pas',
  ForDo_Block in 'Blocks\ForDo_Block.pas',
  IfElse_Block in 'Blocks\IfElse_Block.pas',
  InOut_Block in 'Blocks\InOut_Block.pas',
  Main_Block in 'Blocks\Main_Block.pas',
  MulAssign_Block in 'Blocks\MulAssign_Block.pas',
  FunctionCall_Block in 'Blocks\FunctionCall_Block.pas',
  RepeatUntil_Block in 'Blocks\RepeatUntil_Block.pas',
  WhileDo_Block in 'Blocks\WhileDo_Block.pas',
  About_Form in 'Forms\About_Form.pas' {AboutForm},
  Declarations_Form in 'Forms\Declarations_Form.pas' {DeclarationsForm},
  Main_Form in 'Forms\Main_Form.pas' {MainForm},
  Settings_Form in 'Forms\Settings_Form.pas' {SettingsForm},
  Editor_Form in 'Forms\Editor_Form.pas' {EditorForm},
  Toolbox_Form in 'Forms\Toolbox_Form.pas' {ToolboxForm},
  Explorer_Form in 'Forms\Explorer_Form.pas' {ExplorerForm},
  Pascal_Parser in 'Parsers\Pascal\Pascal_Parser.pas',
  FlashThread in 'Common\FlashThread.pas',
  History in 'Common\History.pas',
  XMLProcessor in 'Common\XMLProcessor.pas',
  If_Block in 'Blocks\If_Block.pas',
  Functions_Form in 'Forms\Functions_Form.pas' {FunctionsForm},
  UserFunction in 'Common\UserFunction.pas',
  Goto_Form in 'Forms\Goto_Form.pas' {GotoForm},
  Pascal_Template in 'LangTemplates\Pascal_Template.pas',
  C_Template in 'LangTemplates\C_Template.pas',
  LexFile in 'Parsers\Common\LexFile.Pas',
  LexLib in 'Parsers\Common\Lexlib.pas',
  ParseGlobals in 'Parsers\Common\ParseGlobals.pas',
  YaccLib in 'Parsers\Common\Yacclib.pas',
  ParserHelper in 'Parsers\Common\ParserHelper.pas',
  DataTypes_Form in 'Forms\DataTypes_Form.pas' {DataTypesForm},
  UserDataType in 'Common\UserDataType.pas',
  Help_Form in 'Forms\Help_Form.pas' {HelpForm},
  LocalizationManager in 'Common\LocalizationManager.pas',
  Case_Block in 'Blocks\Case_Block.pas',
  Statement in 'Common\Statement.pas',
  TiBasic68k_Template in 'LangTemplates\TiBasic68k_Template.pas',
  Return_Block in 'Blocks\Return_Block.pas',
  Base_Form in 'Forms\Base_Form.pas',
  BaseIterator in 'Common\BaseIterator.pas',
  Project in 'Common\Project.pas',
  DeclareList in 'Common\DeclareList.pas',
  Comment in 'Common\Comment.pas',
  Settings in 'Common\Settings.pas',
  VCLFixes in 'Common\VCLFixes.pas',
  SizeEdit in 'Common\SizeEdit.pas',
  CommonInterfaces in 'Common\CommonInterfaces.pas',
  PageControl_Form in 'Forms\PageControl_Form.pas' {PageControlForm},
  TabComponent in 'Common\TabComponent.pas',
  Element in 'Common\Element.pas',
  Navigator_Form in 'Forms\Navigator_Form.pas' {NavigatorForm},
  CommonTypes in 'Common\CommonTypes.pas',
  Text_Block in 'Blocks\Text_Block.pas',
  SortListDecorator in 'Common\SortListDecorator.pas',
  EditMemo_Form in 'Forms\EditMemo_Form.pas' {MemoEditorForm},
  LangDefinition in 'Common\LangDefinition.pas',
  Dummy_Template in 'LangTemplates\Dummy_Template.pas',
  BlockFactory in 'Common\BlockFactory.pas',
  StatementMemo in 'Common\StatementMemo.pas',
  MultiLine_Block in 'Blocks\MultiLine_Block.pas',
  Folder_Block in 'Blocks\Folder_Block.pas',
  BlockTabSheet in 'Common\BlockTabSheet.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.CreateForm(TToolboxForm, ToolboxForm);
  Application.CreateForm(TDataTypesForm, DataTypesForm);
  Application.CreateForm(TDeclarationsForm, DeclarationsForm);
  Application.CreateForm(TFunctionsForm, FunctionsForm);
  Application.CreateForm(TEditorForm, EditorForm);
  Application.CreateForm(TExplorerForm, ExplorerForm);
  Application.CreateForm(TGotoForm, GotoForm);
  Application.CreateForm(THelpForm, HelpForm);
  Application.CreateForm(TNavigatorForm, NavigatorForm);
  Application.CreateForm(TMemoEditorForm, MemoEditorForm);
  Application.Run;
end.
