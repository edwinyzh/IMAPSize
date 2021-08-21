{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: RestoreBackupDlg.pas 30 2006-01-15 21:06:52Z Ivan $
|==============================================================================|
| Copyright 2003-2006 Ivan Vecanski                                            |
| =============================================================================}


unit RestoreBackupDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VirtualTrees, ExtCtrls, CommonTypes, Menus;

type

  TFRestoreBackupDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    FolderTree: TVirtualStringTree;
    Button3: TButton;
    Button4: TButton;
    Label1: TLabel;
    CBAccountDest: TComboBox;
    Button1: TButton;
    Button2: TButton;
    Label2: TLabel;
    CBAccountNameSrc: TComboBox;
    Label3: TLabel;
    LblRootFolder: TLabel;
    PopupMenu1: TPopupMenu;
    Renamedestinationfolder1: TMenuItem;
    procedure FolderTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure FolderTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure FolderTreeFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBAccountNameSrcChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CBAccountDestChange(Sender: TObject);
    procedure FolderTreeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Renamedestinationfolder1Click(Sender: TObject);
  private
    mTreeNodes: TListOfBackupFolderInfos;  // Nodes in the tree
    mInitNodeDataCnt: Integer;       // Internal counter for initializing attach tree data
    mTreeX, mTreeY: Integer;     // coordinates of the last mouse click on the FolderTree (due to problems with getting the selected node)
    procedure CheckNode(Node: PVirtualNode; check: Boolean);
    procedure PopulateTree;
    procedure ClearTree;
    procedure CheckAllParts(check: Boolean);
    function NumChecked: Integer;
    procedure SetDestinationAccounts;
    procedure ManageNewFolderNameColumn;
  public
    function SetBackedUpAccounts(defaultAccount: String): Boolean;
    function GetSourceAccount: String;
    function GetDestinationAccount: String;
    procedure GetFoldersToBackup(var folders: TListOfBackupFolderInfos);
  end;

var
  FRestoreBackupDlg: TFRestoreBackupDlg;

implementation

uses Main, SQLiteTable3, Log, AppSettings;

{$R *.DFM}

procedure TFRestoreBackupDlg.FormCreate(Sender: TObject);
begin
    SetLength(mTreeNodes,0);
end;

procedure TFRestoreBackupDlg.FormDestroy(Sender: TObject);
begin
    SetLength(mTreeNodes,0);
end;

{ Populate CBAccountNameSrc with accounts that have backups.
  If the defaultAccount has a backup, select it. If not, select the first account.
  @return true if there is at least one backed up account }
function TFRestoreBackupDlg.SetBackedUpAccounts(defaultAccount: String): Boolean;
var i, index : Integer; sltb: TSQLIteTable;
begin
    LblRootFolder.Caption:=settings.MiscSettings.BackupDir;
    sltb := FMain.backupDb.db.GetTable('select distinct(accountName) from backup_folder');
    if sltb.rowcount>0 then begin
        for i:=0 to sltb.rowcount-1 do begin
            CBAccountNameSrc.Items.Add(sltb.FieldAsString(sltb.FieldIndex['accountName']));
            sltb.Next;
        end;
        // Select defaultAccount if it exists, if not select the first one
        index := CBAccountNameSrc.Items.IndexOf(defaultAccount);
        if index>-1 then CBAccountNameSrc.ItemIndex:=index
        else CBAccountNameSrc.ItemIndex:=0;

        SetDestinationAccounts;
        PopulateTree;

        Result:=true;
    end
    else
        // There are no backed up accounts
        Result:=false;
end;

procedure TFRestoreBackupDlg.SetDestinationAccounts;
var i, index: Integer;
begin
    CBAccountDest.Items.Clear;
    for i:=0 to FMain.ComboBox1.Items.Count-1 do begin
        CBAccountDest.Items.Add(FMain.ComboBox1.Items[i]);
    end;
    // Try to select the source account
    index:=CBAccountDest.Items.IndexOf(CBAccountNameSrc.Text);
    if index>-1 then
        CBAccountDest.ItemIndex:=index
    else begin
        // Backed up account doesn't exist. Select active account
        CBAccountDest.ItemIndex:=FMain.ComboBox1.ItemIndex;
    end;
end;

procedure TFRestoreBackupDlg.FolderTreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var Data: PBackupFolderInfo;
begin
    with Sender do begin
        Data := GetNodeData(Node);
        Data.remoteFolder:=mTreeNodes[mInitNodeDataCnt].remoteFolder;
        Data.localFolder:=mTreeNodes[mInitNodeDataCnt].localFolder;
        // Data.numMessages:=mTreeNodes[mInitNodeDataCnt].numMessages;
        Data.lastUpdated:=mTreeNodes[mInitNodeDataCnt].lastUpdated;
        // Set the check boxes (requires TreeOptions.MiscOptions.toCheckSupport:=true)
        Data.newRemoteName:=mTreeNodes[mInitNodeDataCnt].newRemoteName;
        Node.CheckType := ctCheckBox;
        CheckNode(Node,true);
    end;
    // Increment global counter for the array
    Inc(mInitNodeDataCnt);
end;

procedure TFRestoreBackupDlg.FolderTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var CurrNodeRec: PBackupFolderInfo;
begin
    { Get a pointer to the data we reserved and initialized in OnInitNode }
    CurrNodeRec := PBackupFolderInfo( Sender.GetNodeData(Node));
    if Assigned(CurrNodeRec) then begin
        case Column of
            0: CellText:=CurrNodeRec.remoteFolder;
            1: CellText:=CurrNodeRec.localFolder;
            // 2: CellText:=IntToStr(CurrNodeRec.numMessages);
            2: CellText:=CurrNodeRec.lastUpdated;
            3: CellText:=CurrNodeRec.newRemoteName;
        end;
    end;
end;

procedure TFRestoreBackupDlg.FolderTreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var Data: PBackupFolderInfo;
begin
    Data := Sender.GetNodeData(Node);
    if Assigned(Data) then Finalize(Data^);
end;

procedure TFRestoreBackupDlg.CheckNode(Node: PVirtualNode; check: Boolean);
begin
    if Node.CheckType=ctCheckBox then begin
        if check then Node.CheckState:=csCheckedNormal
        else Node.CheckState:=csUncheckedNormal;
    end;
end;

procedure TFRestoreBackupDlg.PopulateTree;
var i,j: Integer;
    CurrNode: PVirtualNode;
begin
    ClearTree;

    // Get data from database
    // CBAccountNameSrc.Items[CBAccountNameSrc.ItemIndex]
    FMain.backupDb.GetBackupFolderInfos(CBAccountNameSrc.Items[CBAccountNameSrc.ItemIndex],mTreeNodes);

    // Now populate
    mInitNodeDataCnt:=0;
    FolderTree.NodeDataSize := SizeOf(TBackupFolderInfo);
    FolderTree.RootNodeCount:=Length(mTreeNodes);  // all nodes are root
    CurrNode:=FolderTree.GetFirst;

    // Add nodes to the tree
    for i:=0 to Length(mTreeNodes)-1 do begin
        FolderTree.ChildCount[CurrNode] := 0;
        CurrNode:=FolderTree.GetNext(CurrNode);
    end;

    FolderTree.OffsetX:=0;
    FolderTree.OffsetY:=0;
end;

procedure TFRestoreBackupDlg.ClearTree;
begin
    FolderTree.Clear;
    FolderTree.Repaint;
end;

{ (Un)check all parts in the parts tree. }
procedure TFRestoreBackupDlg.CheckAllParts(check: Boolean);
var i: Integer;
    CurrNode: PVirtualNode;
begin
    CurrNode:=FolderTree.GetFirst;
    for i:=0 to Length(mTreeNodes)-1 do begin
        CheckNode(CurrNode, check);
        CurrNode:=FolderTree.GetNext(CurrNode);
    end;
    FolderTree.Refresh;
end;

function TFRestoreBackupDlg.NumChecked: Integer;
var i, count: Integer;
    CurrNode: PVirtualNode;
begin
    count:=0;
    CurrNode:=FolderTree.GetFirst;
    for i:=0 to Length(mTreeNodes)-1 do begin
        if CurrNode.CheckState=csCheckedNormal then Inc(count);
        CurrNode:=FolderTree.GetNext(CurrNode);
    end;
    Result:=count;
end;


procedure TFRestoreBackupDlg.CBAccountNameSrcChange(Sender: TObject);
begin
    SetDestinationAccounts;
    PopulateTree;
    ManageNewFolderNameColumn;
end;

procedure TFRestoreBackupDlg.CBAccountDestChange(Sender: TObject);
begin
    ManageNewFolderNameColumn;
end;

procedure TFRestoreBackupDlg.ManageNewFolderNameColumn;
begin
    if CBAccountNameSrc.Text<>CBAccountDest.Text then
        FolderTree.Header.Columns[3].Options:=FolderTree.Header.Columns[3].Options+[coVisible]
    else
        FolderTree.Header.Columns[3].Options:=FolderTree.Header.Columns[3].Options-[coVisible]
end;

procedure TFRestoreBackupDlg.Button1Click(Sender: TObject);
begin
    CheckAllParts(true);
end;

procedure TFRestoreBackupDlg.Button2Click(Sender: TObject);
begin
    CheckAllParts(false);
end;

procedure TFRestoreBackupDlg.Button3Click(Sender: TObject);
var proceed: Boolean;
begin
    proceed:=false;
    if NumChecked>0 then begin
        if CBAccountNameSrc.Text<>CBAccountDest.Text then begin
            if MessageDlg('You have chosen to restore backups to a different account. Are you sure?',mtConfirmation,[mbOK,mbCancel],0)=mrOK then begin
                proceed:=true;
            end
        end
        else proceed:=true;
    end
    else MessageDlg('Please select folders to be restored',mtInformation,[mbOK],0);
    if proceed then begin
        ModalResult:=mrOK;
    end;
end;

function TFRestoreBackupDlg.GetSourceAccount: String;
begin
    Result:= CBAccountNameSrc.Text;
end;

function TFRestoreBackupDlg.GetDestinationAccount: String;
begin
    Result:=CBAccountDest.Text; 
end;

procedure TFRestoreBackupDlg.GetFoldersToBackup(var folders: TListOfBackupFolderInfos);
var i,j: Integer;
    CurrNode: PVirtualNode;
    Data: PBackupFolderInfo;
begin
    SetLength(folders,NumChecked);
    CurrNode:=FolderTree.GetFirst;
    j:=0;
    for i:=0 to Length(mTreeNodes)-1 do begin
        if CurrNode.CheckState=csCheckedNormal then begin
            Data := FolderTree.GetNodeData(CurrNode);
            folders[j].remoteFolder:=Data.remoteFolder;
            folders[j].localFolder:=Data.localFolder;
            folders[j].newRemoteName:=Data.newRemoteName;
            Inc(j);
        end;
        CurrNode:=FolderTree.GetNext(CurrNode);
    end;
end;

procedure TFRestoreBackupDlg.FolderTreeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var AnItem: PVirtualNode; p: TPoint; Data: PBackupFolderInfo;
begin
    if Button=mbRight then begin
        if coVisible in FolderTree.Header.Columns[3].Options then begin
            AnItem := FolderTree.GetNodeAt(X,Y);
            if AnItem<>nil then begin
                FolderTree.Selected[AnItem]:=true;
                FolderTree.FocusedNode:=AnItem;
                p.x := X; p.y:= Y;
                p := FolderTree.ClientToScreen(p);
                Data := FolderTree.GetNodeData(AnItem);

                mTreeX:=x;
                mTreeY:=y;
                PopupMenu1.Popup(p.X,p.Y);
            end;
        end;
    end
end;

procedure TFRestoreBackupDlg.Renamedestinationfolder1Click(
  Sender: TObject);
var Data: PBackupFolderInfo; AnItem: PVirtualNode; currentFolderName, newFolderName: String;
begin
    AnItem := FolderTree.GetNodeAt(mTreeX,mTreeY);
    Data := FolderTree.GetNodeData(AnItem);
    currentFolderName:=Data.newRemoteName;
    newFolderName:=InputBox('Change Remote Folder Name','Enter the new name for "'+currentFolderName+'" on the remote server',currentFolderName);
    Data.newRemoteName:=newFolderName;
end;

end.
