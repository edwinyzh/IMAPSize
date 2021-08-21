unit MboxInspectorDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, VirtualTrees, Buttons, PBFolderDialog, CommonTypes,
  ComCtrls;

type

  TFMboxInspectorDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    EditMboxFile: TEdit;
    SpeedButton1: TSpeedButton;
    MessageList: TVirtualStringTree;
    Splitter1: TSplitter;
    Memo1: TMemo;
    Button1: TButton;
    Label2: TLabel;
    LblMsgCount: TLabel;
    OpenDialog: TOpenDialog;
    Button2: TButton;
    StatusBar1: TStatusBar;
    procedure FormActivate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MessageListInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure MessageListGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure MessageListFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
  private
    mTreeNodes: TListOfMboxMessageInfo;  // Nodes in the tree
    mInitNodeDataCnt: Integer;
    mTreeX, mTreeY: Integer;             // coordinates of the last mouse click on the FolderTree (due to problems with getting the selected node)
    procedure PopulateTree;
    procedure ClearTree;
  public
    { Public declarations }
  end;

var
  FMboxInspectorDlg: TFMboxInspectorDlg;

implementation

uses Main;

{$R *.DFM}

procedure TFMboxInspectorDlg.FormActivate(Sender: TObject);
begin
    EditMboxFile.Text:=settings.MiscSettings.MboxDir;
end;

procedure TFMboxInspectorDlg.SpeedButton1Click(Sender: TObject);
begin
    OpenDialog.InitialDir:=settings.MiscSettings.MboxDir;
    OpenDialog.Filter := 'Unix mailbox files (*.mbox)|*.MBOX|All files (*.*)|*.*';
    if OpenDialog.Execute then begin
        EditMboxFile.Text:=OpenDialog.Filename;
        settings.MiscSettings.MboxDir:=ExtractFilePath(OpenDialog.Filename);
    end;
end;

procedure TFMboxInspectorDlg.PopulateTree;
var i,j: Integer;
    CurrNode: PVirtualNode;
begin
    ClearTree;

    // Now populate
    mInitNodeDataCnt:=0;
    MessageList.NodeDataSize := SizeOf(TMboxMessageInfo);
    MessageList.RootNodeCount:=Length(mTreeNodes);  // all nodes are root
    CurrNode:=MessageList.GetFirst;

    // Add nodes to the tree
    for i:=0 to Length(mTreeNodes)-1 do begin
        MessageList.ChildCount[CurrNode] := 0;
        CurrNode:=MessageList.GetNext(CurrNode);
    end;

    MessageList.OffsetX:=0;
    MessageList.OffsetY:=0;
end;

procedure TFMboxInspectorDlg.ClearTree;
begin
    MessageList.Clear;
    MessageList.Repaint;
end;

procedure TFMboxInspectorDlg.FormCreate(Sender: TObject);
begin
    SetLength(mTreeNodes,0);
end;

procedure TFMboxInspectorDlg.FormDestroy(Sender: TObject);
begin
    SetLength(mTreeNodes,0);
end;

procedure TFMboxInspectorDlg.MessageListInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var Data: PMboxMessageInfo;
begin
    with Sender do begin
        Data := GetNodeData(Node);
        Data.LineNum:=mTreeNodes[mInitNodeDataCnt].LineNum;
        Data.FromLine:=mTreeNodes[mInitNodeDataCnt].FromLine;
        Data.Subject:=mTreeNodes[mInitNodeDataCnt].Subject;
    end;
    // Increment global counter for the array
    Inc(mInitNodeDataCnt);
end;

procedure TFMboxInspectorDlg.MessageListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var CurrNodeRec: PMboxMessageInfo;
begin
    { Get a pointer to the data we reserved and initialized in OnInitNode }
    CurrNodeRec := PMboxMessageInfo( Sender.GetNodeData(Node));
    if Assigned(CurrNodeRec) then begin
        case Column of
            0: CellText:=CurrNodeRec.LineNum;
            1: CellText:=CurrNodeRec.FromLine;
            2: CellText:=CurrNodeRec.Subject;
        end;
    end;
end;

procedure TFMboxInspectorDlg.MessageListFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var Data: PMboxMessageInfo;
begin
    Data := Sender.GetNodeData(Node);
    if Assigned(Data) then Finalize(Data^);
end;
end.
