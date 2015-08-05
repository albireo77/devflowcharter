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



unit TreeView_Form;

interface

uses
   Graphics, Controls, Forms, StdCtrls, ExtCtrls, Menus, ComCtrls, SysUtils,
   Classes, Types, OmniXML, Base_Form, CommonTypes, CommonInterfaces;

type
  TTreeViewForm = class(TBaseForm)
    tvExplorer: TTreeView;
    lblErrors: TLabel;
    lblWarnings: TLabel;
    PopupMenu: TPopupMenu;
    miExpand: TMenuItem;
    miCollapse: TMenuItem;
    miRefresh: TMenuItem;
    miNextError: TMenuItem;
    miPrevError: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    miRemove: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure tvExplorerChange(Sender: TObject; Node: TTreeNode);
    procedure miExpandClick(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure miRefreshClick(Sender: TObject);
    procedure miNextErrorClick(Sender: TObject);
    procedure tvExplorerGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure tvExplorerGetSelectedIndex(Sender: TObject;
      Node: TTreeNode);
    procedure tvExplorerCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure miRemoveClick(Sender: TObject);
    procedure Localize(const AList: TStringList); override;
    procedure ResetForm; override;
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
  private
    { Private declarations }
    FErrWarnCount: TErrWarnCount;
    function GetFocusable(const ANode: TTreeNode): IFocusable;

  public
    { Public declarations }
    procedure ExportSettingsToXMLTag(const root: IXMLElement); override;
    procedure ImportSettingsFromXMLTag(const root: IXMLElement); override;
  end;

var
  TreeViewForm: TTreeViewForm;

implementation
{$R *.dfm}

uses
   ApplicationCommon, SourceEditor_Form, Statement, MulAssign_Block, Case_Block,
   Base_Block, Navigator_Form, Return_Block, Windows;

const
   NODE_OK   = 15;      // indexes for images in MainForm.ImageList1
   NODE_ERR  = 14;      // as icon nodes on TreeViewForm.tvFlowChart
   NODE_WARN = 16;
   NODE_NONE = 17;

procedure TTreeViewForm.FormShow(Sender: TObject);
begin
    if GProject <> nil then
    begin
       FErrWarnCount := GProject.CountErrWarn;
       if FErrWarnCount.ErrorCount > 0 then
          lblErrors.Font.Color := NOK_COLOR
       else
          lblErrors.Font.Color := OK_COLOR;
       lblErrors.Caption := i18Manager.GetFormattedString('lblErrors', [FErrWarnCount.ErrorCount]);
       lblWarnings.Caption := i18Manager.GetFormattedString('lblWarnings', [FErrWarnCount.WarningCount]);
       with tvExplorer do
       begin
          Items.BeginUpdate;
          Items.Clear;
          Selected := Items.AddChild(nil, i18Manager.GetFormattedString('RootNodeText', [GProject.Name]));
          GProject.GenerateTree(Selected);
          Items.EndUpdate;
       end;
    end;
end;

procedure TTreeViewForm.ResetForm;
begin
   Width := 498;
   Height := 574;
   FErrWarnCount.ErrorCount := 0;
   FErrWarnCount.WarningCount := 0;
   inherited ResetForm;
end;

function TTreeViewForm.GetFocusable(const ANode: TTreeNode): IFocusable;
var
   lControl: TWinControl;
begin
   result := nil;
   if (ANode <> nil) and TInfra.IsValid(ANode.Data) then
   begin
      lControl := ANode.Data;
      if TObject(lControl) is TWinControl then
         Supports(lControl, IFocusable, result);
   end;
end;

procedure TTreeViewForm.Localize(const AList: TStringList);
begin
   if tvExplorer.CanFocus then
      miRefresh.Click;
   inherited Localize(AList);
end;

procedure TTreeViewForm.tvExplorerChange(Sender: TObject; Node: TTreeNode);
var
   lFocusable: IFocusable;
   lFocusInfo: TFocusInfo;
begin
   lFocusable := GetFocusable(Node);
   if (lFocusable <> nil) and lFocusable.CanBeFocused then
   begin
      TInfra.InitFocusInfo(lFocusInfo);
      lFocusInfo.ActiveControl := tvExplorer;
      lFocusable.RetrieveFocus(lFocusInfo);
      GProject.RepaintFlowcharts;
   end;
end;

procedure TTreeViewForm.miExpandClick(Sender: TObject);
begin
   if tvExplorer.Selected <> nil then
   begin
      tvExplorer.Items.BeginUpdate;
      if Sender = miExpand then
         tvExplorer.Selected.Expand(true)
      else if Sender = miCollapse then
         tvExplorer.Selected.Collapse(true);
      tvExplorer.Items.EndUpdate;
   end;
end;

procedure TTreeViewForm.PopupMenuPopup(Sender: TObject);
var
   lFocusable: IFocusable;
begin
   miExpand.Enabled    := false;
   miCollapse.Enabled  := false;
   miNextError.Enabled := false;
   miPrevError.Enabled := false;
   miRemove.Enabled    := false;
   if tvExplorer.Selected <> nil then
   begin
      miNextError.Enabled := (FErrWarnCount.ErrorCount > 0) or (FErrWarnCount.WarningCount > 0);
      miPrevError.Enabled := miNextError.Enabled;
      miExpand.Enabled := tvExplorer.Selected.HasChildren;
      miCollapse.Enabled := miExpand.Enabled;
      lFocusable := GetFocusable(tvExplorer.Selected);
      miRemove.Enabled := (lFocusable <> nil) and lFocusable.CanBeRemoved;
   end;
end;

procedure TTreeViewForm.miRefreshClick(Sender: TObject);
begin
   tvExplorer.Enabled := false;
   FormShow(Self);
   tvExplorer.Enabled := true;
end;

procedure TTreeViewForm.miNextErrorClick(Sender: TObject);
var
   i, c, endValue: integer;
   lFocusable: IFocusable;
begin
   if tvExplorer.Selected <> nil then
   begin
      if Sender = miNextError then
      begin
         c := 1;
         endValue := tvExplorer.Items.Count;
      end
      else
      begin
         c := -1;
         endValue := -1;
      end;
      i := tvExplorer.Selected.AbsoluteIndex + c;
      while i <> endValue do
      begin
         lFocusable := GetFocusable(tvExplorer.Items[i]);
         if (lFocusable <> nil) and TInfra.IsRestricted(lFocusable.GetFocusColor) then
         begin
            if not tvExplorer.Items[i].IsVisible then
               tvExplorer.Items[i].MakeVisible;
            tvExplorer.Selected := tvExplorer.Items[i];
            break;
         end;
         i := i + c;
      end;
   end;
end;

procedure TTreeViewForm.tvExplorerGetImageIndex(Sender: TObject; Node: TTreeNode);
var
   lFocusable: IFocusable;
begin
   Node.ImageIndex := NODE_NONE;
   lFocusable := GetFocusable(Node);
   if (lFocusable <> nil) and not lFocusable.IsBoldDesc then
   begin
      case lFocusable.GetFocusColor of
         NOK_COLOR:  Node.ImageIndex := NODE_ERR;
         WARN_COLOR: Node.ImageIndex := NODE_WARN;
      else
         Node.ImageIndex := NODE_OK;
      end;
   end;
end;

procedure TTreeViewForm.tvExplorerGetSelectedIndex(Sender: TObject; Node: TTreeNode);
var
   lFocusable: IFocusable;
begin
   Node.SelectedIndex := NODE_NONE;
   lFocusable := GetFocusable(Node);
   if (lFocusable <> nil) and not lFocusable.IsBoldDesc then
   begin
      case lFocusable.GetFocusColor of
         NOK_COLOR:  Node.SelectedIndex := NODE_ERR;
         WARN_COLOR: Node.SelectedIndex := NODE_WARN;
      else
         Node.SelectedIndex := NODE_OK;
      end;
   end;
end;

procedure TTreeViewForm.tvExplorerCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
   NodeRect: TRect;
   lFocusable: IFocusable;
   lColor: TColor;
begin
   lFocusable := GetFocusable(Node);
   with Sender.Canvas do
   begin
      lColor := OK_COLOR;
      if cdsSelected in State then
         lColor := TTreeView(Sender).Color
      else if (lFocusable <> nil) and TInfra.IsRestricted(lFocusable.GetFocusColor) then
         lColor := lFocusable.GetFocusColor;
      Font.Color := lColor;
      if (lFocusable <> nil) and lFocusable.IsBoldDesc then
         Font.Style := Font.Style + [fsBold];
      NodeRect := Node.DisplayRect(True);
      TextOut(NodeRect.Left, NodeRect.Top, Node.Text);
   end;
end;

procedure TTreeViewForm.miRemoveClick(Sender: TObject);

 function RemoveData(const ANode: TTreeNode): boolean;
 var
    lControl: TWinControl;
    lCaseBlock: TCaseBlock;
    iter: IIterator;
    lBranch: TBranch;
    lFocusable: IFocusable;
 begin
    result := false;
    if ANode <> nil then
    begin
       lControl := ANode.Data;
       if not (TInfra.IsValid(lControl) or (TObject(lControl) is TWinControl)) then
          exit;
       if (lControl.Parent is TCaseBlock) and (lControl <> TCaseBlock(lControl.Parent).GetTextControl) then
       begin
          lCaseBlock := TCaseBlock(lControl.Parent);
          iter := lCaseBlock.GetBranchIterator(PRIMARY_BRANCH_IND+1);
          while iter.HasNext do
          begin
             lBranch := TBranch(iter.Next);
             if lBranch.Statement = lControl then
             begin
                lCaseBlock.Ired := lBranch.Index;
                lCaseBlock.RemoveBranch;
                lCaseBlock.Ired := -1;
                result := true;
                break;
             end;
          end;
       end
       else
       begin
          lFocusable := GetFocusable(ANode);
          if (lFocusable <> nil) and lFocusable.CanBeRemoved then
          begin
             lFocusable.Remove;
             result := true;
          end;
       end;
    end;
 end;
 
begin
   if RemoveData(tvExplorer.Selected) then
   begin
      tvExplorer.Items.BeginUpdate;
      tvExplorer.Selected.Delete;
      tvExplorer.Items.EndUpdate;
   end;
end;

procedure TTreeViewForm.ExportSettingsToXMLTag(const root: IXMLElement);
begin
   if Visible then
   begin
      root.SetAttribute('tree_win_show', '1');
      root.SetAttribute('tree_win_x', IntToStr(Left));
      root.SetAttribute('tree_win_y', IntToStr(Top));
      root.SetAttribute('tree_win_w', IntToStr(Width));
      root.SetAttribute('tree_win_h', IntToStr(Height));
      root.SetAttribute('tree_top_y', IntToStr(tvExplorer.TopItem.AbsoluteIndex));
      if WindowState = wsMinimized then
         root.SetAttribute('tree_win_min', '1');
   end;
end;

procedure TTreeViewForm.ImportSettingsFromXMLTag(const root: IXMLElement);
var
   lRect: TRect;
   lTopIndex: integer;
begin
   if (root.GetAttribute('tree_win_show') = '1') and GInfra.CurrentLang.EnabledExplorer then
   begin
      lRect.Left := StrToIntDef(root.GetAttribute('tree_win_x'), 50);
      lRect.Top := StrToIntDef(root.GetAttribute('tree_win_y'), 50);
      lRect.Right := StrToIntDef(root.GetAttribute('tree_win_w'), 498);
      lRect.Bottom := StrToIntDef(root.GetAttribute('tree_win_h'), 574);
      Position := poDesigned;
      SetBounds(lRect.Left, lRect.Top, lRect.Right, lRect.Bottom);
      if root.GetAttribute('tree_win_min') = '1' then
         WindowState := wsMinimized;
      Show;
      lTopIndex := StrToIntDef(root.GetAttribute('tree_top_y'), -2);
      if (lTopIndex >= 0) and (lTopIndex < tvExplorer.Items.Count) then
         tvExplorer.TopItem := tvExplorer.Items[lTopIndex];
   end;
end;

procedure TTreeViewForm.FormMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
   if (ssCtrl in Shift) and (tvExplorer.Selected <> nil) then
   begin
      tvExplorer.Selected := tvExplorer.Selected.GetPrevVisible;
      Handled := true;
   end;
end;

procedure TTreeViewForm.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
   if (ssCtrl in Shift) and (tvExplorer.Selected <> nil) then
   begin
      tvExplorer.Selected := tvExplorer.Selected.GetNextVisible;
      Handled := true;
   end;
end;

end.

