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



unit Explorer_Form;

interface

uses
   Vcl.Controls, Vcl.StdCtrls, Vcl.Menus, Vcl.ComCtrls, System.Classes, System.Types,
   OmniXML, Base_Form, Types, Interfaces;

type
  TExplorerForm = class(TBaseForm)
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
    chkAutoNav: TCheckBox;
    miRebuild: TMenuItem;
    N3: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure tvExplorerChange(Sender: TObject; Node: TTreeNode);
    procedure miExpandClick(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure miRefreshClick(Sender: TObject);
    procedure miNextErrorClick(Sender: TObject);
    procedure tvExplorerCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure miRemoveClick(Sender: TObject);
    procedure Localize(AList: TStringList); override;
    procedure ResetForm; override;
    procedure FormCreate(Sender: TObject);
    procedure chkAutoNavClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure tvExplorerCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure tvExplorerDeletion(Sender: TObject; Node: TTreeNode);
    procedure miRebuildClick(Sender: TObject);
  private
    { Private declarations }
    FErrWarnCount: TErrWarnCount;
    function GetWithFocus(ANode: TTreeNode): IWithFocus;
    procedure ClearTreeViewItems;
  public
    { Public declarations }
    procedure ExportSettingsToXMLTag(ATag: IXMLElement); override;
    procedure ImportSettingsFromXMLTag(ATag: IXMLElement); override;
  end;

var
  ExplorerForm: TExplorerForm;

implementation
{$R *.dfm}

uses
   Vcl.Graphics, Vcl.Forms, System.SysUtils, System.UITypes, System.Math,
   Infrastructure, Base_Block, XMLProcessor, Constants;

procedure TExplorerForm.FormShow(Sender: TObject);
begin
    if GProject <> nil then
    begin
       FErrWarnCount := GProject.CountErrWarn;
       lblErrors.Font.Color := IfThen(FErrWarnCount.ErrorCount = 0, OK_COLOR, NOK_COLOR);
       lblErrors.Caption := i18Manager.GetFormattedString('lblErrors', [FErrWarnCount.ErrorCount]);
       lblWarnings.Caption := i18Manager.GetFormattedString('lblWarnings', [FErrWarnCount.WarningCount]);
       ClearTreeViewItems;
       with tvExplorer do
       begin
          Items.BeginUpdate;
          try
             Selected := Items.AddChild(nil, i18Manager.GetFormattedString('RootNodeText', [GProject.Name]));
             GProject.GenerateTree(Selected);
          finally
             Items.EndUpdate;
          end;
       end;
    end;
end;

procedure TExplorerForm.ResetForm;
begin
   Width := 498;
   Height := 574;
   FErrWarnCount.ErrorCount := 0;
   FErrWarnCount.WarningCount := 0;
   ClearTreeViewItems;
   inherited ResetForm;
end;

procedure TExplorerForm.ClearTreeViewItems;
var
   deletionEvent:  TTVExpandedEvent;
begin
   deletionEvent := tvExplorer.OnDeletion;
   tvExplorer.OnDeletion := nil;
   try
      tvExplorer.Items.Clear;
   finally
      tvExplorer.OnDeletion := deletionEvent;
   end;
end;

function TExplorerForm.GetWithFocus(ANode: TTreeNode): IWithFocus;
begin
   result := nil;
   if (ANode <> nil) and TInfra.IsValidControl(ANode.Data) then
      Supports(ANode.Data, IWithFocus, result);
end;

procedure TExplorerForm.Localize(AList: TStringList);
begin
   if tvExplorer.CanFocus then
      miRefresh.Click;
   inherited Localize(AList);
end;

procedure TExplorerForm.tvExplorerChange(Sender: TObject; Node: TTreeNode);
var
   withFocus: IWithFocus;
   focusInfo: TFocusInfo;
begin
   if chkAutoNav.Checked then
   begin
      withFocus := GetWithFocus(Node);
      if (withFocus <> nil) and withFocus.CanBeFocused then
      begin
         focusInfo := TFocusInfo.New;
         focusInfo.ActiveControl := tvExplorer;
         withFocus.RetrieveFocus(focusInfo);
         GProject.RepaintFlowcharts;
      end;
   end;
end;

procedure TExplorerForm.tvExplorerCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
   NodeClass := TTreeNodeWithFriend;
end;

procedure TExplorerForm.miExpandClick(Sender: TObject);
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

procedure TExplorerForm.PopupMenuPopup(Sender: TObject);
var
   withFocus: IWithFocus;
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
      withFocus := GetWithFocus(tvExplorer.Selected);
      miRemove.Enabled := (withFocus <> nil) and withFocus.CanRemove;
   end;
end;

procedure TExplorerForm.miRebuildClick(Sender: TObject);
begin
   tvExplorer.Enabled := false;
   try
      FormShow(Self);
   finally
      tvExplorer.Enabled := true;
   end;
end;

procedure TExplorerForm.miRefreshClick(Sender: TObject);
var
   i: integer;
   withFocus: IWithFocus;
   origText: string;
   node: TTreeNodeWithFriend;
begin
   tvExplorer.Items.BeginUpdate;
   try
      for i := 0 to tvExplorer.Items.Count-1 do
      begin
         node := TTreeNodeWithFriend(tvExplorer.Items[i]);
         withFocus := GetWithFocus(node);
         if withFocus <> nil then
         begin
            origText := withFocus.GetTreeNodeText(node.Offset);
            if (origText <> '') and (origText <> node.Text) then
               node.Text := origText;
         end;
      end;
   finally
      tvExplorer.Items.EndUpdate;
   end;
end;

procedure TExplorerForm.miNextErrorClick(Sender: TObject);
var
   i, c, last: integer;
   withFocus: IWithFocus;
begin
   if tvExplorer.Selected <> nil then
   begin
      if Sender = miNextError then
      begin
         c := 1;
         last := tvExplorer.Items.Count;
      end
      else
      begin
         c := -1;
         last := -1;
      end;
      i := tvExplorer.Selected.AbsoluteIndex + c;
      while i <> last do
      begin
         withFocus := GetWithFocus(tvExplorer.Items[i]);
         if (withFocus <> nil) and TInfra.IsNOkColor(withFocus.GetFocusColor) then
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

procedure TExplorerForm.tvExplorerCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
   nodeRect: TRect;
   withFocus: IWithFocus;
   lColor, lColor2: TColor;
   x, y: integer;
   lFont: TFont;
begin
   x := 0;
   y := 0;
   lFont := Sender.Canvas.Font;
   lColor := OK_COLOR;
   if cdsSelected in State then
   begin
      Sender.Canvas.Brush.Style := bsClear;
      x := 2;
      y := 1;
   end;
   withFocus := GetWithFocus(Node);
   if withFocus <> nil then
   begin
      lColor2 := withFocus.GetFocusColor;
      if TInfra.IsNOkColor(lColor2) or (lColor2 = TEXT_COLOR) then
         lColor := lColor2;
      if withFocus.IsBoldDesc then
         lFont.Style := lFont.Style + [fsBold];
   end;
   lFont.Color := lColor;
   nodeRect := Node.DisplayRect(True);
   Sender.Canvas.TextOut(nodeRect.Left+x, nodeRect.Top+y, Node.Text);
   DefaultDraw := true;
end;

procedure TExplorerForm.tvExplorerDeletion(Sender: TObject; Node: TTreeNode);
begin
   TInfra.DecrementNodeSiblingOffsets(Node);
end;

procedure TExplorerForm.miRemoveClick(Sender: TObject);
var
   withFocus: IWithFocus;
   friendNode, selectedNode, toDelete: TTreeNodeWithFriend;
begin
   selectedNode := TTreeNodeWithFriend(tvExplorer.Selected);
   withFocus := GetWithFocus(selectedNode);
   if (withFocus <> nil) and withFocus.Remove(selectedNode) then
   begin
      tvExplorer.Items.BeginUpdate;
      friendNode := selectedNode.Friend;
      if friendNode <> nil then
      begin
         if friendNode.HasAsParent(selectedNode) then
            toDelete := selectedNode
         else if selectedNode.HasAsParent(friendNode) then
            toDelete := friendNode
         else
         begin
            friendNode.Delete;
            toDelete := selectedNode;
         end;
      end
      else
         toDelete := selectedNode;
      toDelete.Delete;
      tvExplorer.Items.EndUpdate;
   end;
end;

procedure TExplorerForm.ExportSettingsToXMLTag(ATag: IXMLElement);
begin
   if Visible then
   begin
      ATag.SetAttribute('tree_win_show', 'true');
      ATag.SetAttribute('tree_win_x', Left.ToString);
      ATag.SetAttribute('tree_win_y', Top.ToString);
      ATag.SetAttribute('tree_win_w', Width.ToString);
      ATag.SetAttribute('tree_win_h', Height.ToString);
      ATag.SetAttribute('tree_top_y', tvExplorer.TopItem.AbsoluteIndex.ToString);
      if WindowState = wsMinimized then
         ATag.SetAttribute('tree_win_min', 'true');
   end;
end;

procedure TExplorerForm.ImportSettingsFromXMLTag(ATag: IXMLElement);
var
   topY: integer;
begin
   if TXMLProcessor.GetBool(ATag, 'tree_win_show') and GInfra.CurrentLang.EnabledExplorer then
   begin
      Position := poDesigned;
      SetBounds(TXMLProcessor.GetInt(ATag, 'tree_win_x', 50),
                TXMLProcessor.GetInt(ATag, 'tree_win_y', 50),
                TXMLProcessor.GetInt(ATag, 'tree_win_w', 498),
                TXMLProcessor.GetInt(ATag, 'tree_win_h', 574));
      if TXMLProcessor.GetBool(ATag, 'tree_win_min') then
         WindowState := wsMinimized;
      Show;
      topY := TXMLProcessor.GetInt(ATag, 'tree_top_y', -2);
      if (topY >= 0) and (topY < tvExplorer.Items.Count) then
         tvExplorer.TopItem := tvExplorer.Items[topY];
   end;
end;

procedure TExplorerForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
   WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
   if (ssCtrl in Shift) and (tvExplorer.Selected <> nil) then
   begin
      if WheelDelta < 0 then
         tvExplorer.Selected := tvExplorer.Selected.GetNextVisible
      else
         tvExplorer.Selected := tvExplorer.Selected.GetPrevVisible;
   end;
end;

procedure TExplorerForm.FormCreate(Sender: TObject);
begin
   chkAutoNav.Checked := GSettings.ExplorerAutoNav;
end;

procedure TExplorerForm.chkAutoNavClick(Sender: TObject);
begin
   GSettings.ExplorerAutoNav := chkAutoNav.Checked;
end;

end.

