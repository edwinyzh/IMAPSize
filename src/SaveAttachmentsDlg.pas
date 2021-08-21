unit SaveAttachmentsDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, VirtualTrees, Buttons, PBFolderDialog, BodyStructure,
  GlobalConstants, RegExpr, MailUtil, RequestManager, Log;

type
  // A node in the tree. Can be a message or a message attachment
  TMsgAttachNode = record
      // Even though we don't need most of this info for displaying, we need it in the process of getting the attachment
      fullMailboxName: String;
      msgUID: String;        // UID of the message (folder unique), used for parenting
      isAttachment: Boolean; // if true, display as attachment, otherwise as message
      msgCaption: String;    // Caption of the message (empty for attachments)
      msgFrom: String;       // Non-visible field, holds the Sender of the message
      msgDate: String;       // Non-visible field, holds the timestamp of the message
      size: Integer;
      bodyPartID: String;    // ID of the part (e.g. 1.1.2)
      bodyPartStr: String;   // The raw bodypart string (as returned by the server)
      childCount: Integer;   // Number of children for this node
      filename: String;      // Potential name of the file where the attachment will be saved
      encoding: String;
      partMimeType: String;
      partMimeSubtype: String;
  end;
  PMsgAttachNode = ^TMsgAttachNode;
  TListOfMsgAttachNodes = array of TMsgAttachNode;

  TFSaveAttachmentsDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    BtnSaveAttachments: TButton;
    Button2: TButton;
    AttachTree: TVirtualStringTree;
    Panel3: TPanel;
    Panel4: TPanel;
    Button3: TButton;
    Button4: TButton;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    EditDir: TEdit;
    CBFineTuneCompare: TComboBox;
    EditFineTune: TEdit;
    CBFineTuneType: TComboBox;
    Button5: TButton;
    Button6: TButton;
    DirDialog1: TPBFolderDialog;
    SpeedButton1: TSpeedButton;
    Label1: TLabel;
    LabelStatus: TLabel;
    Label3: TLabel;
    LabelSize: TLabel;
    GroupBox2: TGroupBox;
    CBPrefixSender: TCheckBox;
    CBPrefixTime: TCheckBox;
    procedure AttachTreeFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure AttachTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure AttachTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure FormCreate(Sender: TObject);
    procedure CBFineTuneTypeChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure AttachTreeChecked(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure BtnSaveAttachmentsClick(Sender: TObject);
  private
    mFullMailboxName: String;           // Remote name of the selected mailbox
    mTreeNodes: TListOfMsgAttachNodes;  // Nodes in the tree
    mInitNodeDataCnt: Integer;          // Internal counter for initializing attach tree data
    mMsgUIDs: TStringList;              // Holder for message UIDs
    mBodyStructures: TStringList;       // Holder for raw body structures
    mMsgTitles: TStringList;            // Holder for message titles (passed from Main)
    mMsgFroms: TStringList;             // Holder for message From fields
    mMsgDates: TStringList;             // Holder for message Date fields
    function ParseBodyStructure(var bs: TBodyStructure; bsString: String): Boolean;
    procedure PopulateAttachTree;
    procedure ClearPartsTree;
    procedure CheckAllParts(check: Boolean);
    procedure CheckNode(Node: PVirtualNode; check: Boolean);
    procedure SetCompareOptions;
    procedure SetSelectedSize;
    procedure FineSelect(select: Boolean);
    function ValidateFilename(dir,filename: String; checkExistance: Boolean): String;
    function NumChecked: Integer;
  public
    procedure SetMsgTitles(var msgTitles: TStringList);
    procedure SetMsgFroms(var msgFroms: TStringList);
    procedure SetMsgDates(var msgDates: TStringList);
    procedure SetParams(fullMailboxName: String);
    procedure PrepareList(mailboxIndex: Integer; var msgs: TStringList);
    procedure HandleRequestOver(var Message: TMessage); message WM_REQUEST_OVER;
    procedure CreateAttSavingRequest(var requestPtr: PRequest);
  end;

var
  FSaveAttachmentsDlg: TFSaveAttachmentsDlg;

implementation

uses Main, CustomThread, CheckerThread, IMAPWorker, MyUtil, VecLog;

{$R *.DFM}

procedure TFSaveAttachmentsDlg.FormCreate(Sender: TObject);
begin
    CBFineTuneType.ItemIndex:=0;
    SetCompareOptions;
    SetLength(mTreeNodes,0);
    mMsgUIDs:=TStringList.Create;
    mBodyStructures:=TStringList.Create;
    mMsgTitles:=TStringList.Create;
    mMsgFroms:=TStringList.Create;
    mMsgDates:=TStringList.Create;
    LabelSize.Caption:='0 K';
end;

procedure TFSaveAttachmentsDlg.FormDestroy(Sender: TObject);
begin
    SetLength(mTreeNodes,0);
    mMsgUIDs.Free;
    mBodyStructures.Free;
    mMsgTitles.Free;
    mMsgFroms.Free;
    mMsgDates.Free;
end;

procedure TFSaveAttachmentsDlg.SetMsgTitles(var msgTitles: TStringList);
begin
    CopyStringList(TStrings(mMsgTitles), TStrings(msgTitles));
end;

procedure TFSaveAttachmentsDlg.SetMsgFroms(var msgFroms: TStringList);
begin
    CopyStringList(TStrings(mMsgFroms), TStrings(msgFroms));
end;

procedure TFSaveAttachmentsDlg.SetMsgDates(var msgDates: TStringList);
begin
    CopyStringList(TStrings(mMsgDates), TStrings(msgDates));
end;

procedure TFSaveAttachmentsDlg.SetParams(fullMailboxName: String);
begin
    mFullMailboxName:=fullMailboxName;
end;

procedure TFSaveAttachmentsDlg.PrepareList(mailboxIndex: Integer; var msgs: TStringList);
var requestPtr: PRequest;
begin
    EditDir.Text:=settings.MiscSettings.SaveAttachmentDir;
    LabelStatus.Caption:='Getting list of attachments. Please wait...';
    New(requestPtr);
    requestPtr^ := TRequest.Create(thtIMAPWorker);
    requestPtr^.requestType:=rtGetBodyStructures;
    requestPtr^.requestComponent:=rcSaveAttachmentsDlg;
    requestPtr^.imapOperation:=iopGetBodyStructures;
    requestPtr^.accountInfoPtr:=FMain.pActiveAccount;
    requestPtr^.data.mailboxIndex:=mailboxIndex;
    requestPtr^.SetMsgUIDs(msgs);
    requestMgr.InvokeRequest(requestPtr);
end;

{ Handles messages sent by the request manager when a request is over }
procedure TFSaveAttachmentsDlg.HandleRequestOver(var Message: TMessage);
var requestID, i: Integer; success: Integer; request: TRequest;
begin
    devLog.Trace('TFSaveAttachmentsDlg..HandleRequestOver');
    requestId:=Message.WParam;
    try
        request := requestMgr.GetRequest(requestId);
        success:=Message.LParam;
        if request.requestType = rtGetBodyStructures then begin
            if success=1 then begin
                // Got the body structures, populate the tree
                LabelStatus.Caption:='';
                // Copy the retrieved list of raw body structures
                CopyStringList(TStrings(mBodyStructures), TStrings(request.data.msgContent));
                CopyStringList(TStrings(mMsgUIDs),TStrings(request.data.msgUIDs));
                PopulateAttachTree;
            end;
        end;
    finally
        requestMgr.RemoveRequest(requestId, false);
    end;
end;

procedure TFSaveAttachmentsDlg.PopulateAttachTree;
var i,j: Integer;
    CurrNode: PVirtualNode;
    advancedMode: Boolean;
    bs: TBodyStructure;
begin
    bs:=TBodyStructure.Create;
    ClearPartsTree;

    // Get the data for the tree
    mInitNodeDataCnt:=0;
    for i:=0 to mBodyStructures.Count-1 do begin
        bs.Clear;
        if ParseBodyStructure(bs,mBodyStructures[i]) then begin
            // Body structure successfully parsed, add to the tree model
            bs.PopulateBodyPartsForDisplay(false);
            // Expand the tree nodes array to CurrentLength + number of parts for display + 1 for the title
            SetLength(mTreeNodes,Length(mTreeNodes)+Length(bs.mBodyPartsForDisplay)+1);
            mTreeNodes[mInitNodeDataCnt].msgUID:=mMsgUIDs[i];
            mTreeNodes[mInitNodeDataCnt].isAttachment:=false;
            mTreeNodes[mInitNodeDataCnt].msgCaption:=mMsgTitles[i];  // Message title (From, Subject, etc)
            mTreeNodes[mInitNodeDataCnt].msgFrom:=mMsgFroms[i];
            mTreeNodes[mInitNodeDataCnt].msgDate:=mMsgDates[i];

            // If msgs without attachments should be marked somehow, use this:
            // if Length(bs.mBodyPartsForDisplay) = 0 then
            //    mTreeNodes[mInitNodeDataCnt].msgCaption:='No attachments in this message'

            mTreeNodes[mInitNodeDataCnt].childCount:=Length(bs.mBodyPartsForDisplay);
            Inc(mInitNodeDataCnt);
            for j:=0 to Length(bs.mBodyPartsForDisplay)-1 do begin
                mTreeNodes[mInitNodeDataCnt].fullMailboxName:=mFullMailboxName; // Currently we support only messages from the same mailbox
                mTreeNodes[mInitNodeDataCnt].msgUID:=mMsgUIDs[i];
                mTreeNodes[mInitNodeDataCnt].isAttachment:=true;
                mTreeNodes[mInitNodeDataCnt].Size:=bs.mBodyPartsForDisplay[j].mSize;
                mTreeNodes[mInitNodeDataCnt].bodyPartID:=bs.mBodyPartsForDisplay[j].mID;
                mTreeNodes[mInitNodeDataCnt].msgCaption:=bs.mBodyPartsForDisplay[j].mName;
                mTreeNodes[mInitNodeDataCnt].msgFrom:='';
                mTreeNodes[mInitNodeDataCnt].msgDate:='';
                mTreeNodes[mInitNodeDataCnt].childCount:=0;
                mTreeNodes[mInitNodeDataCnt].encoding:=bs.mBodyPartsForDisplay[j].mEncoding;
                mTreeNodes[mInitNodeDataCnt].partMimeType:=bs.mBodyPartsForDisplay[j].mType; // LowerCase(bs.GetBodyMimePart(Data^.mId).mType);
                mTreeNodes[mInitNodeDataCnt].partMimeSubtype:=bs.mBodyPartsForDisplay[j].mSubtype;  // bs.GetBodyMimePart(Data^.mId).mSubType;
                mTreeNodes[mInitNodeDataCnt].filename:=bs.mBodyPartsForDisplay[j].mName;
                // Use the From and Date of the message
                mTreeNodes[mInitNodeDataCnt].msgFrom:=mMsgFroms[i];
                mTreeNodes[mInitNodeDataCnt].msgDate:=mMsgDates[i];

                Inc(mInitNodeDataCnt);
            end;
        end
        else begin
            // Parsing failed, currently just add info to the node
            SetLength(mTreeNodes,Length(mTreeNodes)+1);
            mTreeNodes[mInitNodeDataCnt].msgUID:=mMsgUIDs[i];
            mTreeNodes[mInitNodeDataCnt].isAttachment:=false;
            mTreeNodes[mInitNodeDataCnt].msgCaption:=IntToStr(i)+' Message parsing failed, please report to author!';
            mTreeNodes[mInitNodeDataCnt].msgFrom:='';
            mTreeNodes[mInitNodeDataCnt].msgDate:='';
            mTreeNodes[mInitNodeDataCnt].childCount:=0;
            Inc(mInitNodeDataCnt);
        end;
    end;
    bs.Free;

    // Now populate
    mInitNodeDataCnt:=0;
    AttachTree.NodeDataSize := SizeOf(TMsgAttachNode);
    AttachTree.RootNodeCount:=mBodyStructures.Count;
    CurrNode:=AttachTree.GetFirst;

    // Add nodes to the tree
    for i:=0 to Length(mTreeNodes)-1 do begin
        AttachTree.ChildCount[CurrNode] := mTreeNodes[i].childCount;
        CurrNode:=AttachTree.GetNext(CurrNode);
    end;

    // All nodes expanded
    CurrNode:=AttachTree.GetFirst;
    for i:=0 to Length(mTreeNodes)-1 do begin
        AttachTree.Expanded[CurrNode]:=true;
        CurrNode:=AttachTree.GetNext(CurrNode);
    end;

    AttachTree.OffsetX:=0;
    AttachTree.OffsetY:=0;

    SetSelectedSize;
end;

function TFSaveAttachmentsDlg.ParseBodyStructure(var bs: TBodyStructure; bsString: String): Boolean;
var wholeBS: String; p: Integer;
begin
    Result:=false;
    p := Pos('BODY (',bsString);
    if p>0 then begin
        wholeBS := Copy(bsString,p,Length(bsString)-p);
        Result := bs.Parse(wholeBS);
    end;
end;

procedure TFSaveAttachmentsDlg.ClearPartsTree;
begin
    AttachTree.Clear;
    AttachTree.Repaint;
end;


procedure TFSaveAttachmentsDlg.AttachTreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var Data: PMsgAttachNode;
begin
    try
        with Sender do begin
            Data := GetNodeData(Node);
            Data.msgUID:=mTreeNodes[mInitNodeDataCnt].msgUID;
            Data.isAttachment:=mTreeNodes[mInitNodeDataCnt].isAttachment;
            Data.msgCaption:=mTreeNodes[mInitNodeDataCnt].msgCaption;
            Data.msgFrom:=mTreeNodes[mInitNodeDataCnt].msgFrom;
            Data.msgDate:=mTreeNodes[mInitNodeDataCnt].msgDate;
            Data.bodyPartID:=mTreeNodes[mInitNodeDataCnt].bodyPartID;
            if Data.isAttachment then begin
                Data.fullMailboxName:=mTreeNodes[mInitNodeDataCnt].fullMailboxName;
                Data.size:=mTreeNodes[mInitNodeDataCnt].Size;
                Data.encoding:=mTreeNodes[mInitNodeDataCnt].encoding;
                Data.partMimeType:=mTreeNodes[mInitNodeDataCnt].partMimeType;
                Data.partMimeSubtype:=mTreeNodes[mInitNodeDataCnt].partMimeSubtype;
                Data.msgFrom:=mTreeNodes[mInitNodeDataCnt].msgFrom;
                Data.msgDate:=mTreeNodes[mInitNodeDataCnt].msgDate;
                Data.filename:=mTreeNodes[mInitNodeDataCnt].filename;
                // Set the check boxes
                Node.CheckType := ctCheckBox;
                CheckNode(Node,true);
            end;
        end;
        // Increment global counter for the array
        Inc(mInitNodeDataCnt);
    except
        devLog.Error('TFMsgPeeker.AttachTreeInitNode - Problem adding body part to the attachment list');
    end;
end;

procedure TFSaveAttachmentsDlg.AttachTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var CurrNodeRec: PMsgAttachNode;
begin
    { Get a pointer to the data we reserved and initialized in OnInitNode }
    CurrNodeRec := PMsgAttachNode( Sender.GetNodeData(Node));
    if Assigned(CurrNodeRec) then begin
        case Column of
            0: CellText:=CurrNodeRec.msgCaption;
            1: if CurrNodeRec.isAttachment then CellText := FloatToStrF(CurrNodeRec.size,ffNumber,12,0)
                                           else CellText := '';
        end;
    end;
end;

procedure TFSaveAttachmentsDlg.AttachTreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var Data: PMsgAttachNode;
begin
    Data := Sender.GetNodeData(Node);
    if Assigned(Data) then Finalize(Data^);
end;

{ (Un)check a single node (part) }
procedure TFSaveAttachmentsDlg.CheckNode(Node: PVirtualNode; check: Boolean);
begin
    if Node.CheckType=ctCheckBox then begin
        if check then Node.CheckState:=csCheckedNormal
        else Node.CheckState:=csUncheckedNormal;
    end;
end;

{ (Un)check all parts in the parts tree. }
procedure TFSaveAttachmentsDlg.CheckAllParts(check: Boolean);
var i: Integer;
    CurrNode: PVirtualNode;
begin
    CurrNode:=AttachTree.GetFirst;
    for i:=0 to Length(mTreeNodes)-1 do begin
        if mTreeNodes[i].isAttachment then begin
            CheckNode(CurrNode, check);
        end;
        CurrNode:=AttachTree.GetNext(CurrNode);
    end;
    AttachTree.Refresh;
    SetSelectedSize;
end;

procedure TFSaveAttachmentsDlg.Button3Click(Sender: TObject);
begin
    CheckAllParts(true);
end;

procedure TFSaveAttachmentsDlg.Button4Click(Sender: TObject);
begin
    CheckAllParts(false);
end;

{ Get the size of checked attachments. Return a string in KB }
procedure TFSaveAttachmentsDlg.SetSelectedSize;
var i, size: Integer; CurrNode: PVirtualNode; Data: PMsgAttachNode;
begin
    size:=0;
    CurrNode:=AttachTree.GetFirst;
    for i:=0 to Length(mTreeNodes)-1 do begin
        if mTreeNodes[i].isAttachment then begin
            if CurrNode.CheckState=csCheckedNormal then begin
                Data := AttachTree.GetNodeData(CurrNode);
                size := size + Data.Size;
            end;
        end;
        CurrNode:=AttachTree.GetNext(CurrNode);
    end;
    LabelSize.Caption:=FloatToStrF(size/1024,ffNumber,6,1) + ' K';;
end;

procedure TFSaveAttachmentsDlg.AttachTreeChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
    SetSelectedSize;
end;

procedure TFSaveAttachmentsDlg.CBFineTuneTypeChange(Sender: TObject);
begin
    SetCompareOptions;
end;

procedure TFSaveAttachmentsDlg.SetCompareOptions;
begin
    CBFineTuneCompare.Clear;
    case CBFineTuneType.ItemIndex of
        0: begin
              EditFineTune.ReadOnly:=true;
              CBFineTuneCompare.Items.Add('is archive');
              CBFineTuneCompare.Items.Add('is image');
              CBFineTuneCompare.Items.Add('is pdf');
              CBFineTuneCompare.Items.Add('is word document');
              CBFineTuneCompare.Items.Add('is movie');
           end;
        1: begin
              // by extension
              EditFineTune.ReadOnly:=false;
              CBFineTuneCompare.Items.Add('is');
              CBFineTuneCompare.Items.Add('is not');
              CBFineTuneCompare.Items.Add('does not exist');
           end;
        2: begin
              // by size
              EditFineTune.ReadOnly:=false;
              CBFineTuneCompare.Items.Add('is greater than (KB)');
              CBFineTuneCompare.Items.Add('is less than (KB)');
           end;
        3: begin
              // name contains
              EditFineTune.ReadOnly:=false;
              CBFineTuneCompare.Items.Add('contains');
              CBFineTuneCompare.Items.Add('matches regex');  
           end;

    end;
    CBFineTuneCompare.ItemIndex:=0;
end;

procedure TFSaveAttachmentsDlg.FineSelect(select: Boolean);
var i, ftSize: Integer; CurrNode: PVirtualNode; Data: PMsgAttachNode;
    re: TRegExpr;
begin
    case CBFineTuneType.ItemIndex of
        0: begin
            // by attachment type
            try
                re:=TRegExpr.Create;
                re.Expression:=EditFineTune.Text;
                re.ModifierI:=true;
                case CBFineTuneCompare.ItemIndex of
                    // @todo make this customizable by the user (in settings?)
                    0: re.Expression:='(\.zip|\.arj|\.rar|\.gz|\.7z)$';
                    1: re.Expression:='(\.gif|\.jpg|\.jpeg|\.bmp)$';
                    2: re.Expression:='\.pdf$';
                    3: re.Expression:='\.doc$';
                    4: re.Expression:='(\.mp3|\.wav)$';
                    5: re.Expression:='(\.mpg|\.mpeg|\.wmv)$';
                end;

                CurrNode:=AttachTree.GetFirst;
                for i:=0 to Length(mTreeNodes)-1 do begin
                    if mTreeNodes[i].isAttachment then begin
                        Data := AttachTree.GetNodeData(CurrNode);
                        if re.exec(Data.msgCaption) then CheckNode(CurrNode,select);
                    end;
                    CurrNode:=AttachTree.GetNext(CurrNode);
                end;
            finally
                re.Free;
            end;
           end;
        1: begin
            // by extension

            if (CBFineTuneCompare.ItemIndex<2) and (EditFineTune.Text='') then
                MessageDlg('Please provide a string for comparison',mtInformation,[mbOk],0)
            else begin
                try
                    re:=TRegExpr.Create;
                    re.Expression:=EditFineTune.Text;
                    re.ModifierI:=true;
                    case CBFineTuneCompare.ItemIndex of
                        0,1: re.Expression:='\.'+EditFineTune.Text+'$';
                        2: re.Expression:='\.';
                    end;

                    CurrNode:=AttachTree.GetFirst;
                    for i:=0 to Length(mTreeNodes)-1 do begin
                        if mTreeNodes[i].isAttachment then begin
                            Data := AttachTree.GetNodeData(CurrNode);
                            case CBFineTuneCompare.ItemIndex of
                                0: if re.exec(Data.msgCaption) then CheckNode(CurrNode,select);
                                1: if not re.exec(Data.msgCaption) then CheckNode(CurrNode,select);
                                2: if not re.exec(Data.msgCaption) then CheckNode(CurrNode,select);
                            end;
                        end;
                        CurrNode:=AttachTree.GetNext(CurrNode);
                    end;
                finally
                    if Assigned(re) then re.Free;
                end;
            end;
           end;
        2: begin
            // by size
            try
                if EditFineTune.Text <> '' then begin
                    ftSize := StrToInt(EditFineTune.Text);
                    CurrNode:=AttachTree.GetFirst;
                    for i:=0 to Length(mTreeNodes)-1 do begin
                        if mTreeNodes[i].isAttachment then begin
                            Data := AttachTree.GetNodeData(CurrNode);
                            if CBFineTuneCompare.ItemIndex = 0 then begin
                                if Data.Size div 1024 > ftSize then CheckNode(CurrNode,select);
                            end
                            else if CBFineTuneCompare.ItemIndex = 1 then begin
                                if Data.Size div 1024 < ftSize then CheckNode(CurrNode,select);
                            end;
                        end;
                        CurrNode:=AttachTree.GetNext(CurrNode);
                    end;
                end
                else raise Exception.Create('No value provided');
            except
                MessageDlg('Please enter an integer value for size comparison',mtInformation,[mbOk],0);
            end;
           end;
        3: begin
            // name contains/matches regex
            if EditFineTune.Text <> '' then begin
                // Create and compile regex if needed
                try
                    try
                        if CBFineTuneCompare.ItemIndex = 1 then begin
                            re:=TRegExpr.Create;
                            re.Expression:=EditFineTune.Text;
                            re.ModifierI:=true;
                        end;

                        CurrNode:=AttachTree.GetFirst;
                        for i:=0 to Length(mTreeNodes)-1 do begin
                            if mTreeNodes[i].isAttachment then begin
                                Data := AttachTree.GetNodeData(CurrNode);
                                if CBFineTuneCompare.ItemIndex = 0 then begin
                                    if Pos(EditFineTune.Text,Data.msgCaption)>0 then CheckNode(CurrNode,select);
                                end
                                else if CBFineTuneCompare.ItemIndex = 1 then begin
                                    if re.exec(Data.msgCaption) then CheckNode(CurrNode,select);
                                end;
                            end;
                            CurrNode:=AttachTree.GetNext(CurrNode);
                        end;
                    except
                        MessageDlg('Problem with the regular exception. Please review it!',mtError,[mbOK],0);
                    end;
                finally
                    if (CBFineTuneCompare.ItemIndex = 1) and Assigned(re) then re.Free;
                end;
            end
            else MessageDlg('Please provide a string for comparison',mtInformation,[mbOk],0);
           end;
    end;
    AttachTree.Refresh;
    SetSelectedSize;
end;

procedure TFSaveAttachmentsDlg.Button5Click(Sender: TObject);
begin
    FineSelect(true);
end;

procedure TFSaveAttachmentsDlg.Button6Click(Sender: TObject);
begin
    FineSelect(false);
end;

procedure TFSaveAttachmentsDlg.SpeedButton1Click(Sender: TObject);
begin
    DirDialog1.Folder:=settings.MiscSettings.SaveAttachmentDir;
    if DirDialog1.Execute then begin
        settings.MiscSettings.SaveAttachmentDir:=DirDialog1.Folder;
        EditDir.Text:=DirDialog1.Folder;
    end;
end;

procedure TFSaveAttachmentsDlg.BtnSaveAttachmentsClick(Sender: TObject);
begin
    if NumChecked>0 then begin
        // Return control to Main, the request will be populated and invoked from there
        ModalResult:=mrOK;
    end
    else MessageDlg('You haven''t selected any attachments for saving!',mtInformation,[mbOk],0);
end;

{ Invoked from TFMain.InvokeSaveAttachments. Populates the request with necessary information for saving attachments.
  The request itself is passed back to FMain and invoked from there, so this dialog can be freed. }
procedure TFSaveAttachmentsDlg.CreateAttSavingRequest(var requestPtr: PRequest);
var i: Integer; CurrNode: PVirtualNode; Data: PMsgAttachNode; mimeType, mimeSubtype: String;
    attSavingInfo: TAttachmentSavingInfo; fname, nowStr: String;
begin
    CurrNode:=AttachTree.GetFirst;
    for i:=0 to Length(mTreeNodes)-1 do begin
        if mTreeNodes[i].isAttachment then begin
            if CurrNode.CheckState=csCheckedNormal then begin
                Data := AttachTree.GetNodeData(CurrNode);

                attSavingInfo:=TAttachmentSavingInfo.Create;
                attSavingInfo.fullMailboxName:=Data.fullMailboxName;
                attSavingInfo.msgUID:=Data.msgUID;
                attSavingInfo.bodyPartID:=Data.bodyPartID;
                attSavingInfo.encoding:=Data.encoding;
                attSavingInfo.partMimeType:=Data.partMimeType;
                attSavingInfo.partMimeSubtype:=Data.partMimeSubtype;
                fname:=Data.filename;
                if CBPrefixSender.Checked then fname:=Data.msgFrom+'_'+Data.msgDate+'_'+fname;
                if CBPrefixTime.Checked then begin
                    DateTimeToString(nowStr,'yyyymmddhhnnss',Now);
                    fname:=nowStr+'_'+fname;
                end;
                attSavingInfo.filename:= ValidateFilename(settings.MiscSettings.SaveAttachmentDir,fname,true);
                attSavingInfo.size:=Data.size;

                requestPtr^.data.generalList.Add(attSavingInfo);

            end;
        end;
        CurrNode:=AttachTree.GetNext(CurrNode);
    end;
end;

{ Validates the filename for any invalid characters and replaces them with a '_'
  If checkExistance is true, will check if the file exists and add a unique integer.
  Returns the full valid file path }
function TFSaveAttachmentsDlg.ValidateFilename(dir,filename: String; checkExistance: Boolean): String;
var fName, fNameOrg: String; invCharRegEx: TRegExpr; i:Integer;
begin
    invCharRegEx:=TRegExpr.Create;
    invCharRegEx.Expression:=invCharRE;
    fName:=invCharRegEx.Replace(filename,'_');
    fNameOrg:=fName;
    if checkExistance then begin
        i:=0;
        while FileExists(dir+'\'+fName) do begin
            fName:=Format('%d_%s',[i,fNameOrg]);
            Inc(i);
        end;
    end;
    Result:=dir+'\'+fName;
end;

{ Returns number of checked items in the check box list }
function TFSaveAttachmentsDlg.NumChecked: Integer;
var i, count: Integer;
    CurrNode: PVirtualNode;
begin
    count:=0;
    CurrNode:=AttachTree.GetFirst;
    for i:=0 to Length(mTreeNodes)-1 do begin
        if CurrNode.CheckState=csCheckedNormal then Inc(count);
        CurrNode:=AttachTree.GetNext(CurrNode);
    end;
    Result:=count;
end;

end.
