{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: MsgPeeker.pas,v 1.14 2004/03/31 23:27:34 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit MsgPeeker;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, Log, VecLog, Buttons,
  BodyStructure, MessageLists, IMAPWorker, GlobalConstants, SynaUtil, SynaCode, RequestManager,
  CheckLst, EnhListView, ExtListView, VirtualTrees, BodyPartsFetch,
  HTMLLite, Menus, CustomThread, RegExpr, XPMenu, MailUtil;

const
    PC_IDX_TEXT = 0;  // PageControl index for the text tab
    PC_IDX_HTML = 1;
    PC_IDX_ATTACH = 2;
    PC_IDX_HEADER = 3;
    PC_IDX_RAW = 4;

type
  TFMsgPeeker = class(TForm)
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    FontDialog1: TFontDialog;
    SaveDialog: TSaveDialog;
    PanelBodyParts: TPanel;
    PanelBodyDisplay: TPanel;
    PageControl1: TPageControl;
    TSText: TTabSheet;
    TSHtml: TTabSheet;
    RichEditBody: TRichEdit;
    TSAttachments: TTabSheet;
    TSHeader: TTabSheet;
    RichEditHeader: TRichEdit;
    PanelParts: TPanel;
    Panel5: TPanel;
    Label1: TLabel;
    PartsTree: TVirtualStringTree;
    TSRaw: TTabSheet;
    RichEditRaw: TRichEdit;
    Panel4: TPanel;
    htmlLite1: ThtmlLite;
    LabelFrom: TLabel;
    LabelTo: TLabel;
    LabelDate: TLabel;
    LabelSubject: TLabel;
    PopupMenu1: TPopupMenu;
    SaveAttachment1: TMenuItem;
    ViewAttachment1: TMenuItem;
    SaveDialogAttach: TSaveDialog;
    MainMenu1: TMainMenu;
    Manage1: TMenuItem;
    MenuItemDeleteAttachments: TMenuItem;
    N1: TMenuItem;
    MenuItemSaveRemote: TMenuItem;
    MenuItemSaveLocal: TMenuItem;
    XPMenu1: TXPMenu;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    N2: TMenuItem;
    Closeviewer1: TMenuItem;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure PartsListViewDblClick(Sender: TObject);
    procedure PartsTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure PartsTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure PartsTreeFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure FormActivate(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure htmlLite1HotSpotClick(Sender: TObject; const SRC: String;
      var Handled: Boolean);
    procedure PartsTreeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SaveAttachment1Click(Sender: TObject);
    procedure ViewAttachment1Click(Sender: TObject);
    procedure MenuItemDeleteAttachmentsClick(Sender: TObject);
    procedure MenuItemSaveRemoteClick(Sender: TObject);
    procedure MenuItemSaveLocalClick(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure Closeviewer1Click(Sender: TObject);
  private
    mMboxIndex: Integer;        // Index of the mailbox of the displayed message
    mFullMailboxName: String;   // Full mailbox name of the displayed message
    mMsgUID: String;            // UID of the displayed message. Needed if attachments will be deleted
    mBodyStructure: String;     // Body structure as retreived from the server
    bs: TBodyStructure;         // Parsed body structure
    bsParsed: Boolean;          // True if the body structure of the current message was successfully parsed
    mMsgInfo: TMessageInfo;     // TMessageInfo of the displayed message
    mAppendMsg: TStrings;       // The message to append
    mBodyModified: Boolean;     // True if the body has been modified
    mInitNodeDataCnt: Integer;  // Internal counter for initializing parts tree data
    mBodyPartsFetch: TBodyPartsFetch; // Stores info for multiple body part fetching
    mSaveLocally: Boolean;      // True on saving if the message is to be saved locally
    mOriginalPartBoundaries: TStringList; // Stores MIME part boundaries of the original message (item at index 0 iz the "global" boundary)
    mHeaderFetched: Boolean;
    mRawMessageFetched: Boolean;
    mPrimaryHTMLFetched: Boolean;
    mOriginalMainTextPart: TStringList;
    mTreeX, mTreeY: Integer;     // coordinates of the last mouse click on the PartsTree (due to problems with getting the selected node)
    procedure CheckAllParts(check: Boolean);
    procedure CheckNode(Node: PVirtualNode; check: Boolean);
    procedure ToggleTreeView;
    procedure SaveMessage(locally: Boolean);
    procedure SaveMessageLocally;
    procedure SaveMessageRemotelly;
    function IsMessageModified: Boolean;
    function AnyPartsMarkedForDeletion: Boolean;
    function AreAllPartsMarkedForDeletion: Boolean;
    procedure GetUncheckedBodyPartIDs(var list: TStringList);
    procedure GetCheckedAttachmentNames(var list: TStringList);
    procedure QuickSmartDeleteAttachment;
    procedure LoadFetchedHeader(var fh: TStringList);
    function IsTypeSupported(mimeType, mimeSubtype: String): Boolean;
    function ValidateFilename(dir,filename: String; checkExistance: Boolean): String;
  public
    procedure ApplyFont(fn: TFont);
    procedure SetHeaderLabels;
    procedure ParseBodyStructure;
    procedure ClearPartsTree;
    procedure PopulatePartsTree;
    procedure FormMimeMessage;
    procedure InvokeAppendOperation;

    procedure PrepareContent(var requestPtr: PRequest);
    procedure GetMainBoundaryFromHeader;

    function  CreateRequestAndSetMainParams(threadType: TThreadType; requestType: TRequestType; imapOperation: TIMAPOperation; mailboxIndex: Integer; msgUID: String): PRequest;
    procedure PerformFetchBodyPart(mailboxIndex: Integer; msgUID: String; bodyPart: String; reqType: TRequestType);
    procedure PerformGetAttachment(mailboxIndex: Integer; msgUID: String; bodyPart,filename,encoding,mimeType,mimeSubtype: String; reqType: TRequestType);
    procedure PerformFetchWholeBody(mailboxIndex: Integer; msgUID: String; limitLength: Integer; reqType: TRequestType);
    procedure PerformFetchRawMessage(mailboxIndex: Integer; msgUID: String; limitLength: Integer; reqType: TRequestType);
    procedure PerformFetchHeader(mailboxIndex: Integer; msgUID: String; reqType: TRequestType);
    procedure PerformIMAPAppend(mailboxIndex: Integer; msgUID: String; wholeMessage: TStrings);

    procedure HandleRequestOver(var Message: TMessage); message WM_REQUEST_OVER;
  end;

var
  FMsgPeeker: TFMsgPeeker;

implementation

uses Main, ThreadManager, MyUtil, DownloadSizeQueryDlg, AttachmentStripper, ShellApi, ImageViewer,
  TextViewer, FileCtrl;

{$R *.DFM}

procedure TFMsgPeeker.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if Key=VK_ESCAPE then Close;
end;

procedure TFMsgPeeker.FormCreate(Sender: TObject);
begin
    //FMsgPeeker.Caption:=GetCaptionForListNodes;
    FMsgPeeker.Top:=settings.MessagePeeker.Top;
    FMsgPeeker.Left:=settings.MessagePeeker.Left;
    FMsgPeeker.Width:=settings.MessagePeeker.Width;
    FMsgPeeker.Height:=settings.MessagePeeker.Height;
    if settings.MessagePeeker.Maximized then FMsgPeeker.WindowState:=wsMaximized
    else FMsgPeeker.WindowState:=wsNormal;
    FMsgPeeker.XPMenu1.Active:=settings.MiscSettings.XPMenus;

    RichEditHeader.Font.Name:=settings.MessagePeeker.FontName;
    RichEditHeader.Font.Size:=settings.MessagePeeker.FontSize;
    RichEditBody.Font:=RichEditHeader.Font;
    RichEditRaw.Font:=RichEditHeader.Font;
    // set in OnActivate: RichEditHeader.ReadOnly:=not settings.IMAPOpSettings.EnableHeaderEditing;
    PartsTree.Header.Columns[0].Width:=settings.MessagePeeker.PartsViewNameWidth;
    PartsTree.Header.Columns[1].Width:=settings.MessagePeeker.PartsViewSizeWidth;
    mAppendMsg:=TStringList.Create;
    mOriginalMainTextPart := TStringList.Create;
    bs:=TBodyStructure.Create;
    mOriginalPartBoundaries:=TStringList.Create;
end;

procedure TFMsgPeeker.FormActivate(Sender: TObject);
begin
    // Set stuff that can be changed in Options
    RichEditHeader.ReadOnly:=not settings.IMAPOpSettings.EnableHeaderEditing;
end;

procedure TFMsgPeeker.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    if FMsgPeeker.WindowState = wsNormal then begin
        settings.MessagePeeker.Top:=FMsgPeeker.Top;
        settings.MessagePeeker.Left:=FMsgPeeker.Left;
        settings.MessagePeeker.Width:=FMsgPeeker.Width;
        settings.MessagePeeker.Height:=FMsgPeeker.Height;
        settings.MessagePeeker.Maximized:=false;
    end
    else if FMsgPeeker.WindowState=wsMaximized then begin
        settings.MessagePeeker.Maximized:=true;
    end;

    settings.MessagePeeker.PartsViewNameWidth:=PartsTree.Header.Columns[0].Width;
    settings.MessagePeeker.PartsViewSizeWidth:=PartsTree.Header.Columns[1].Width;
    settings.MessagePeeker.FontName:=RichEditHeader.Font.Name;
    settings.MessagePeeker.FontSize:=RichEditHeader.Font.Size;
end;

procedure TFMsgPeeker.FormDestroy(Sender: TObject);
begin
    mAppendMsg.Free;
    mOriginalMainTextPart.Free;
    if Assigned(mBodyPartsFetch) then mBodyPartsFetch.Free;
    bs.Free;
    mOriginalPartBoundaries.Free;
end;

{ Fills any needed information that has not already been set }
procedure TFMsgPeeker.PrepareContent(var requestPtr: PRequest);
var dummy: TMessageInfo; msgIndex, dRes: Integer;
begin

    mMsgUID:=requestPtr^.data.msgUID;
    mFullMailboxName:=requestPtr^.data.FullMailboxName;
    mMboxIndex:=requestPtr^.data.displayedMailboxIndex;

    // Set mailbox index and message info (stored in request^.data)
    CopyMessageInfo(requestPtr^.data.msgInfo,mMsgInfo);

    mBodyStructure := requestPtr^.data.stringResult;
    bs.Clear;
    ParseBodyStructure;  // if successfull bsParsed will be true

    if bsParsed then begin
        PopulatePartsTree;
        if bs.HasPrimaryTextPart then begin
            // Invoke fetching of the primary text part while we set up other stuff
            PerformFetchBodyPart(mMboxIndex, mMsgUID, bs.GetPrimaryTextPart, rtFetchPrimaryTextPart);
        end;
    end
    else begin
        // Parsing of the body structure failed. We can't determine the main text part.
        // Inform the user and get the whole body as primary text part
        MessageDlg('Parsing of the message body structure failed!'+
                   'The main text might not be displayed correctly.'+
                   'If the problem persists please report it to the author...',mtWarning,[mbOK],0);
        if (settings.IMAPOpSettings.LimitTextToFetch) and (mMsgInfo.mSize > settings.IMAPOpSettings.TextToFetchLength) then begin
            FDownloadSizeQueryDlg := TFDownloadSizeQueryDlg.Create(Self);
            FDownloadSizeQueryDlg.InitializeValues(mMsgInfo.mSize,settings.IMAPOpSettings.TextToFetchLength);
            dRes:=FDownloadSizeQueryDlg.ShowModal;
            if dRes=mrGetSpecified then begin
                PerformFetchWholeBody(mMboxIndex, mMsgUID, FDownloadSizeQueryDlg.GetSpecified, rtFetchWholeBodyAsPrimaryText);
            end
            else if dRes=mrGetFull then begin
                PerformFetchWholeBody(mMboxIndex, mMsgUID, -1, rtFetchWholeBodyAsPrimaryText);
            end;
            FDownloadSizeQueryDlg.Free;
        end
        else begin
            PerformFetchWholeBody(mMboxIndex, mMsgUID, -1, rtFetchWholeBodyAsPrimaryText);
        end;
    end;

    RichEditHeader.Clear;
    RichEditBody.Clear;
    RichEditRaw.Clear;
    RichEditHEader.Modified:=false;

    mBodyModified:=false;
    mHeaderFetched:=false;
    mRawMessageFetched:=false;
    mPrimaryHTMLFetched:=false;

    Caption:='Message Viewer';
    SetHeaderLabels;

    if bsParsed then begin
        MenuItemDeleteAttachments.Enabled := mMsgInfo.mHasAttachments;
        PageControl1.Pages[PC_IDX_ATTACH].TabVisible := mMsgInfo.mHasAttachments; // TSAttachments

        // Set HTML tab visible if the html part exists
        PageControl1.Pages[PC_IDX_HTML].TabVisible := bs.HasPrimaryHtmlPart;
        // Always open with the text tab as active
    end
    else begin
        MenuItemDeleteAttachments.Enabled := false;
        PageControl1.Pages[PC_IDX_ATTACH].TabVisible:=false;
        PageControl1.Pages[PC_IDX_HTML].TabVisible:=false;
    end;

    PageControl1.ActivePageIndex:=PC_IDX_TEXT;

    // Set message as seen in the list (if not already seen)
    if not mMsgInfo.mFlags.mSeen then begin
        msgIndex:=mMessageLists.GetMessageInfoByUID(dummy,mMboxIndex,mMsgInfo.mUID); // need this to get the msgIndex...
        mMsgInfo.mFlags.mSeen:=true;
        mMessageLists.UpdateMessageInfo(mMboxIndex,msgIndex,mMsgInfo);
    end;
end;

procedure TFMsgPeeker.ApplyFont(fn: TFont);
begin
    RichEditHeader.Font:=fn;
    RichEditBody.Font:=fn;
    RichEditRaw.Font:=fn;
end;

procedure TFMsgPeeker.SetHeaderLabels;
begin
    LabelFrom.Caption:='From: '+mMsgInfo.mFrom;
    LabelTo.Caption:='To: '+mMsgInfo.mTo;
    LabelSubject.Caption:='Subject: '+mMsgInfo.mSubject;
    LabelDate.Caption:='Date: '+mMsgInfo.mDateStr;
end;

procedure TFMsgPeeker.ParseBodyStructure;
var wholeBS: String; p: Integer;
begin
    p := Pos('BODY (',mBodyStructure);
    devLog.Debug('Found "BODY" substring at position: '+IntToStr(p));

    if p>0 then begin
        wholeBS := Copy(mBodyStructure,p,Length(mBodyStructure)-p);
        bsParsed := bs.Parse(wholeBS);
    end;
end;

{ Invokes attachment deletion (removes ALL attachments)
  This is the so-called quick&smart attachment deletion }


procedure TFMsgPeeker.QuickSmartDeleteAttachment;
var attStripper: TAttachmentStripper; strippedMsg: TStringList;
begin
    attStripper:=TAttachmentStripper.Create;
    attStripper.StripMessage(RichEditHeader.Lines,mOriginalMainTextPart,bs,RichEditHeader.Modified);
    attStripper.GetStrippedMessage(mAppendMsg);
    attStripper.Free;
    InvokeAppendOperation;
end;

{ Will form the mime compliant message ready for uploading.
  Message will be stored in the global mAppendMsg }
procedure TFMsgPeeker.FormMimeMessage;
var bpId, contentTypeStr, contentEncodingStr, boundary: String;
    bodyMimePart: TBodyMimePart;
    multipart: Boolean;
    partLines: TStringList; // no need for create and free, is retrieved from mBodyPartsFetch
    i,j, p1, p2: Integer;
    list: TStringList;
begin
    multipart:=false;
    // Get the first body part
    bpId:=mBodyPartsFetch.GetBodyPartID(0);  // Retrieve the parts ID
    bodyMimePart:=bs.GetBodyMimePart(bpId);  // Get the TBodyMimePart of this part
    // Determine if message is multipart. If yes, generate a boundary
    // Also generate a multipart/mixed if the only part is not a text
    if (mBodyPartsFetch.GetNumberOfParts > 1) or
       ((mBodyPartsFetch.GetNumberOfParts = 1) and (bodyMimePart.mType<>'text')) then begin
        multipart:=true;
        contentTypeStr:='Content-Type: multipart/mixed';   //@todo need to handle rfcmessage
        Randomize;
        boundary:='IMAPSize-=-Bnd-=-'+IntToStr(Random(1000000));
    end
    else begin
        // Only one part, use its contentType and contentEncoding
        contentTypeStr:='Content-Type: '+bodyMimePart.mType+'/'+bodyMimePart.mSubtype;
        if (LowerCase(bodyMimePart.mParameterList[0])='charset') then // @todo you'll probably need to expand this for other params
            contentTypeStr:=contentTypeStr+'; charset="'+bodyMimePart.mParameterList[1]+'"';
        contentEncodingStr:='Content-Transfer-Encoding: '+bodyMimePart.mEncoding;
    end;

    mAppendMsg.Clear;
    // Edit header
    // - Add X-IMAPSize headers
    // - Set correct content-type and boundary in case of a multipart message
    //Add the IMAPSize header field to the top of the header
    if AnyPartsMarkedForDeletion then mAppendMsg.Add(RFC822_HEADER_IMAPSIZE+'Some attachments deleted on '+Rfc822DateTime(Now));
    if RichEditHeader.Modified then mAppendMsg.Add(RFC822_HEADER_IMAPSIZE+'Message header manually modified on '+Rfc822DateTime(Now));
    // Add content type and transfer encoding
    if multipart then contentTypeStr:=contentTypeStr+'; boundary="'+boundary+'"';
    mAppendMsg.Add(contentTypeStr);
    if not multipart then mAppendMsg.Add(contentEncodingStr);
    // Add other header fields. Remove content-type (with boundary if there) and content-transfer-encoding lines

    i:=0;
    while i<RichEditHeader.Lines.Count do begin
        p1:=Pos('content-type',LowerCase(RichEditHeader.Lines[i]));
        p2:=Pos('content-transfer-encoding:',LowerCase(RichEditHeader.Lines[i]));
        if ((p1<>1) and (p2<>1)) then mAppendMsg.Add(RichEditHeader.Lines[i])
        else if p1>0 then begin // Content-Type line
            // Some multipart and other non-text/plain messages contain the boundary on the next line. Remove.
            if Pos('boundary',LowerCase(RichEditHeader.Lines[i+1])) > 0 then begin
                i:=i+1;  //@todo needs to be more robust (boundary could be elsewhere also)?
            end;
        end
        else if p2>0 then begin
            // Content-Transfer-Encoding line (skip)
        end
        else mAppendMsg.Add(RichEditHeader.Lines[i]);
        i:=i+1;
    end;

    // Add the parts from mBodyPartsFetch
    mAppendMsg.Add(''); // Blank line between header and body
    if multipart then begin
        mAppendMsg.Add('This is a multi-part message in MIME format.');
        // Add each body part (with its own contentType and encoding)
        for i:=0 to mBodyPartsFetch.GetNumberOfParts-1 do begin
            contentEncodingStr:='';
            bpId:=mBodyPartsFetch.GetBodyPartID(i);  // Retrieve the parts ID

            bodyMimePart:=bs.GetBodyMimePart(bpId);  // Get the TBodyMimePart of this part
            contentTypeStr:='Content-Type: '+bodyMimePart.mType+'/'+bodyMimePart.mSubtype;
            if bodyMimePart.mParameterList.Count>0 then begin
                contentTypeStr:=contentTypeStr+'; '+bodyMimePart.mParameterList[0]+'="'+bodyMimePart.mParameterList[1]+'"';
            end;
            if bodyMimePart.mEncoding<>'' then contentEncodingStr:='Content-Transfer-Encoding: '+bodyMimePart.mEncoding;

            mAppendMsg.Add('');
            mAppendMsg.Add('--'+boundary);
            mAppendMsg.Add(contentTypeStr);
            if contentEncodingStr<>'' then mAppendMsg.Add(contentEncodingStr);
            mAppendMsg.Add('');

            // Add names of deleted attachments to the body of the first part (if needed)
            if i=0 then begin
                if AnyPartsMarkedForDeletion then begin
                    if settings.IMAPOpSettings.MsgDelAddFilenamesToBody then begin
                        list:=TStringList.Create;
                        GetCheckedAttachmentNames(list);
                        if list.Count>0 then begin
                            mAppendMsg.Add('----'+CRLF+'IMAPSize: Following attachment(s) have been deleted on '+Rfc822DateTime(Now)+':');
                            for j:=0 to list.Count-1 do begin
                                mAppendMsg.Add('    '+list[j]);
                            end;
                            mAppendMsg.Add('----'+CRLF);
                        end;
                        list.Free;
                    end;
                end;
            end;

            partLines:=mBodyPartsFetch.GetBodyPartLines(i);
            for j:=0 to partLines.Count-1 do mAppendMsg.Add(partLines[j]);
        end;
        mAppendMsg.Add('');
        mAppendMsg.Add('--'+boundary+'--');  // the end of the MIME message (boundary + '--', see RFC 2046)
    end
    else begin
        // Single part message
        partLines:=mBodyPartsFetch.GetBodyPartLines(0);
        for i:=0 to partLines.Count-1 do mAppendMsg.Add(partLines[i]);
    end;

end;

{ Invokes the IMAP Append operation. Sending mAppendMsg as the message }
procedure TFMsgPeeker.InvokeAppendOperation;
var i: Integer;
begin
    // Debug contents of the new message
    //devLog.Trace('-------------START message to append------------');
    //for i:=0 to mAppendMsg.Count-1 do begin
    //    devLog.Trace(mAppendMsg[i]);
    //end;
    //devLog.Trace('-------------END message to append------------');
    PerformIMAPAppend(mMboxIndex, mMsgUID, mAppendMsg);
    if settings.MessagePeeker.AutoCloseMsgPeeker then Close;
end;

{ Will add the main MIME boundary of the message to mOriginalPartBoundaries }
procedure TFMsgPeeker.GetMainBoundaryFromHeader;
var i, j, bndPos: Integer; bnd: String;
begin
    mOriginalPartBoundaries.Clear;
    bnd:='';
    for i:=0 to RichEditHeader.Lines.Count-1 do begin
        bndPos:=Pos('boundary="',LowerCase(RichEditHeader.Lines[i]));
        if bndPos>0 then begin
            // This line contains the definition of the boundary. Extract the boundary
            j:=bndPos+10;
            while RichEditHeader.Lines[i][j]<>'"' do begin bnd:=bnd+RichEditHeader.Lines[i][j]; Inc(j); end;
            devLog.Trace('Main MIME boundary is: '+bnd);
        end;
    end;
    if bnd<>'' then mOriginalPartBoundaries.Add(bnd);
end;

function TFMsgPeeker.CreateRequestAndSetMainParams(threadType: TThreadType; requestType: TRequestType;
            imapOperation: TIMAPOperation; mailboxIndex: Integer; msgUID: String): PRequest;
var requestPtr: PRequest;
begin
    New(requestPtr);
    requestPtr^ := TRequest.Create(threadType);
    requestPtr^.requestType:=requestType;
    requestPtr^.imapOperation:=imapOperation;
    requestPtr^.requestComponent:=rcMsgPeeker;
    requestPtr^.accountInfoPtr:=FMain.pActiveAccount;
    requestPtr^.data.mailboxIndex := mailboxIndex;
    requestPtr^.data.msgUID:=msgUID;
    Result:=requestPtr;
end;


{ This invokes fetching of the specified body part.
  @param reqType determines what request type is actually being performed.
  All supported req types work the same, the difference is what happens when
  the request is over. See HandleRequestOver for more details:
  - rtFetchBodyPart: we are just getting the body part, no appending is being performed
  - rtFetchMultipleBodyParts: We are retrieving multiple body parts, need to consult
  the BodyPartsFetch object if more fetches are needed.
}
procedure TFMsgPeeker.PerformFetchBodyPart(mailboxIndex: Integer; msgUID: String; bodyPart: String; reqType: TRequestType);
var requestPtr: PRequest;
begin
    requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,reqType,iopFetchBodyPart,mailboxIndex,msgUID);
    requestPtr^.data.BodyPart:=bodyPart;
    requestMgr.InvokeRequest(requestPtr);
end;

{ Very similar to PerformFetchBodyPart, but requires some more specific information }
procedure TFMsgPeeker.PerformGetAttachment(mailboxIndex: Integer; msgUID: String; bodyPart,filename,encoding,mimeType,mimeSubtype: String; reqType: TRequestType);
var requestPtr: PRequest;
begin
    requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,reqType,iopFetchBodyPart,mailboxIndex,msgUID);
    requestPtr^.data.BodyPart:=bodyPart;
    requestPtr^.data.filename:=filename; 
    requestPtr^.data.encoding:=encoding;
    requestPtr^.data.partMimeType:=mimeType;
    requestPtr^.data.partMimeSubtype:=mimeSubtype;
    requestMgr.InvokeRequest(requestPtr);
end;


{ Invokes the whole message body (no header). Not limited to a number of characters }
procedure TFMsgPeeker.PerformFetchWholeBody(mailboxIndex: Integer; msgUID: String; limitLength: Integer; reqType: TRequestType);
var requestPtr: PRequest;
begin
    requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,reqType,iopFetchWholeBody,mailboxIndex,msgUID);
    requestPtr^.data.limitLength:=limitLength;
    requestMgr.InvokeRequest(requestPtr);
end;

procedure TFMsgPeeker.PerformFetchHeader(mailboxIndex: Integer; msgUID: String; reqType: TRequestType);
var requestPtr: PRequest;
begin
    requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,reqType,iopFetchHeader,mailboxIndex,msgUID);
    requestMgr.InvokeRequest(requestPtr);
end;

{ Invokes fetch of the raw (whole) message. Does use limitation:
  limitLength is the amount of bytes the user wants to get (-1 to get the whole message) }
procedure TFMsgPeeker.PerformFetchRawMessage(mailboxIndex: Integer; msgUID: String; limitLength: Integer; reqType: TRequestType);
var requestPtr: PRequest;
begin
    requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,reqType,iopFetchWholeMessage,mailboxIndex,msgUID);
    requestPtr^.data.limitLength:=limitLength;
    requestMgr.InvokeRequest(requestPtr);
end;


procedure TFMsgPeeker.PerformIMAPAppend(mailboxIndex: Integer; msgUID: String; wholeMessage: TStrings);
var requestPtr: PRequest; i: Integer; msg: TMessageInfo; flags: TMessageFlags;
begin
    requestPtr := CreateRequestAndSetMainParams(thtIMAPWorker,rtAppend,iopAppendMessage,mailboxIndex,msgUID);
    requestPtr^.SetMessageContents(wholeMessage);
    requestPtr^.data.msgUID:=msgUID;
    // Need to get flags of the original msg to set them to the new one
    if (mMessageLists.GetMessageInfoByUID(msg, mailboxIndex,msgUID) <> -1) then begin
        requestPtr^.setMsgFlags(msg.mFlags);
    end
    else begin
        // Just make sure no flags are set if the msg is not found (shouldn't happen though)
        flags.mSeen:=false;
        flags.mAnswered:=false;
        flags.mDeleted:=false;
        flags.mFlagged:=false;
        flags.mDraft:=false;
        requestPtr^.setMsgFlags(flags);
    end;
    requestMgr.InvokeRequest(requestPtr);
end;

procedure TFMsgPeeker.HandleRequestOver(var Message: TMessage);
var requestId, i: Integer; request: TRequest;
    nextPart, encoding, attachDir, savedFilename, subtype: String;
    decodedStr: TStringList; attType: TAttachmentType;
begin
    devLog.Trace('TFMsgPeeker.HandleRequestOver');
    requestId:=Message.WParam;
    try
        devLog.Trace('Request over: '+IntToStr(requestId));
        // If the request was to get the body part invoke the append operation
        request:=requestMgr.GetRequest(requestId);
        if request.requestType = rtFetchPrimaryTextPart then begin
            // First save the original result of the main text part. This will be used when saving messages
            // since the part might be displayed modified (decoded)
            mOriginalMainTextPart.Clear;
            for i:=0 to request.data.MsgContent.Count-1 do mOriginalMainTextPart.Add(request.data.MsgContent[i]);

            // Now decode the body part and display it
            encoding:=bs.GetBodyMimePart(request.data.bodyPart).mEncoding;
            decodedStr:=TStringList.Create;
            DecodeTextPart(request.data.MsgContent,decodedStr,encoding);
            RichEditBody.Lines:=decodedStr;
            RichEditBody.SelStart:=0;
            decodedStr.Free;
        end
        else if request.requestType = rtFetchWholeBodyAsPrimaryText then begin
            RichEditBody.Lines:=request.data.MsgContent;
            RichEditBody.SelStart:=0;
        end
        else if request.requestType = rtFetchPrimaryHTMLPart then begin
            encoding:=bs.GetBodyMimePart(request.data.bodyPart).mEncoding;
            decodedStr:=TStringList.Create;
            DecodeTextPart(request.data.MsgContent,decodedStr,encoding);
            htmlLite1.LoadStrings(decodedStr,'');
            mPrimaryHTMLFetched:=true;
            decodedStr.Free;
        end
        else if request.requestType = rtFetchRawMessage then begin
            RichEditRaw.Lines:=request.data.MsgContent;
            RichEditRaw.SelStart:=0;
            mRawMessageFetched:=true;
        end
        else if request.requestType = rtFetchHeader then begin
            LoadFetchedHeader(request.data.MsgContent);
        end
        else if request.requestType = rtFetchHeaderForQSDeletion then begin
            LoadFetchedHeader(request.data.MsgContent);
            QuickSmartDeleteAttachment;
        end
        else if request.requestType = rtFetchHeaderForRemoteSaving then begin
            LoadFetchedHeader(request.data.MsgContent);
            SaveMessageRemotelly;
        end
        else if request.requestType = rtFetchHeaderForLocalSaving then begin
            LoadFetchedHeader(request.data.MsgContent);
            SaveMessageLocally;
        end
        // if the whole body was being fetched, add the (modified) header from the richeditheader
        else if request.requestType = rtFetchWholeBody then begin
            mAppendMsg.Clear;
            mAppendMsg.Add(RFC822_HEADER_IMAPSIZE+'Message header manually modified on '+Rfc822DateTime(Now));
            for i:=0 to RichEditHeader.Lines.Count-1 do begin
                mAppendMsg.Add(RichEditHeader.Lines[i]);
            end;
            for i:=0 to request.data.msgContent.Count-1 do begin
                mAppendMsg.Add(request.data.msgContent[i]);
            end;
            if mSaveLocally then begin
                mAppendMsg.SaveToFile(SaveDialog.Filename);
            end
            else InvokeAppendOperation;
        end
        // If we are performing a fetch of multiple body parts (one by one that is)
        else if request.requestType = rtFetchMultipleBodyParts then begin
            // Add the body part to the BodyPartsFetch object (will mark it as fetched)
            mBodyPartsFetch.AddPart(request.data.MsgContent,mOriginalPartBoundaries,request.data.bodyPart);
            // Check if there are more body parts to be fetched
            nextPart:=mBodyPartsFetch.GetNextPartToFetch;
            devLog.Trace('NextPart='+nextPart);
            if nextPart<>'DONE' then begin
                // there are more parts to be fetched so do it
                PerformFetchBodyPart(mMboxIndex, mMsgUID, nextPart, rtFetchMultipleBodyParts);
            end
            else begin
                // if done form the MIME message and append to server
                FormMimeMessage;
                if mSaveLocally then begin
                    mAppendMsg.SaveToFile(SaveDialog.Filename);
                end
                else InvokeAppendOperation;
                // We don't need the object anymore
                mBodyPartsFetch.Free;
            end;
        end
        else if request.requestType = rtFetchBodyPart then begin
            // No such requests yet...
        end
        else if request.requestType = rtSaveAttachment then begin
            encoding:=request.data.encoding;
            decodedStr:=TStringList.Create;
            try
                attType:=DecodePart(request.data.MsgContent,decodedStr,encoding,request.data.partMimeType,request.data.partMimeSubtype);
                try
                    if attType=attTypeBinary then SaveFileFromString(request.data.Filename,decodedStr[0])
                    else decodedStr.SaveToFile(request.data.Filename);
                except
                    on E:EFCreateError do MessageDlg('Failed to save the file: '+E.Message,mtError,[mbOK],0);
                end;
            finally
                decodedStr.Free;
            end;
        end
        else if request.requestType = rtViewAttachment then begin
            // Save file to the attachments folder
            decodedStr:=TStringList.Create;
            try
                encoding:=request.data.encoding;
                attType:=DecodePart(request.data.MsgContent,decodedStr,encoding,request.data.partMimeType,request.data.partMimeSubtype);
                attachDir:=dataDir+'\attachments';
                if not DirectoryExists(attachDir) then CreateDir(attachDir);
                savedFilename:=ValidateFilename(attachDir,request.data.Filename,true);
                devLog.Trace('Saving file: '+savedFilename);
                try
                    if attType=attTypeBinary then SaveFileFromString(savedFilename,decodedStr[0])
                    else decodedStr.SaveToFile(savedFilename);
                    // Open the file based on its extension and mime type/subtype
                    if LowerCase(request.data.partMimeType)='image' then begin
                        subtype:=LowerCase(request.data.partMimeSubtype);
                        FImageViewer:=TFImageViewer.Create(Application);  // will free itself (OnClose)
                        if subtype='gif' then begin
                            FImageViewer.GIFImage1.LoadFromFile(savedFilename);
                            FImageViewer.PageControl1.ActivePageIndex:=GIF_PAGE_IDX;
                            FImageViewer.Show;
                        end
                        else if ((subtype='jpg') or (subtype='jpeg') or (subtype='bmp') or (subtype='ico') or (subtype='emf') or (subtype='wmf')) then begin
                            FImageViewer.Image1.Picture.LoadFromFile(savedFilename);
                            FImageViewer.PageControl1.ActivePageIndex:=JPG_PAGE_IDX;
                            FImageViewer.Show;
                        end
                        else begin
                            // Open external viewer
                            ShellExecute(Handle, 'open', PChar(savedFilename), nil, nil, SW_SHOW);
                        end;
                    end
                    else if (LowerCase(request.data.partMimeType)='text') or
                            (LowerCase(request.data.partMimeType)='message')
                    then begin
                        // invoke internal viewer/editor
                        subtype:=LowerCase(request.data.partMimeSubtype);
                        FTextViewer:=TFTextViewer.Create(Application);
                        if subtype='html' then begin
                            FTextViewer.htmlLite1.LoadFromFile(savedFilename);
                            FTextViewer.PageControl1.ActivePageIndex:=HTML_PAGE_IDX;
                            FTextViewer.Show;
                        end
                        else begin
                            FTextViewer.Memo1.Lines.LoadFromFile(savedFilename);
                            FTextViewer.PageControl1.ActivePageIndex:=TEXT_PAGE_IDX;
                            FTextViewer.Show;
                        end;
                    end
                    else begin
                        // Unknown mime type, try to open the file
                        devLog.Trace('Openning file with external application');
                        ShellExecute(Handle, 'open', PChar(savedFilename), nil, nil, SW_SHOW);
                    end;
                except
                    on E:EFCreateError do MessageDlg('Failed to save file for viewing: '+E.Message,mtError,[mbOK],0);
                end;
            finally
                decodedStr.Free;
            end;
        end;
    finally
        requestMgr.RemoveRequest(requestId,false);
    end;
end;

{ Private, loads the specified stringlist into the RichEditHeader component }
procedure TFMsgPeeker.LoadFetchedHeader(var fh: TStringList);
begin
    RichEditHeader.Lines:=fh;
    RichEditHeader.SelStart:=0;
    mHeaderFetched:=true;
    RichEditHeader.Modified:=false;
end;

procedure TFMsgPeeker.PartsListViewDblClick(Sender: TObject);
begin
    // Retrieve body part and save. Also include the html part (alternative).
end;

procedure TFMsgPeeker.ClearPartsTree;
begin
    PartsTree.Clear;
    PartsTree.Repaint;
    // Panel1.Repaint;  //@todo repaint holding panel
end;

{ simple mode - flat
  advanced mode - full hierarchy of the body structure }
procedure TFMsgPeeker.PopulatePartsTree;
var i: Integer;
    CurrNode: PVirtualNode;
    advancedMode: Boolean;
begin
    SpeedButton4.Down:=settings.MessagePeeker.PartsAdvanced;
    advancedMode:=settings.MessagePeeker.PartsAdvanced;
    // Populate the records to be displayed on the tree, depending on the mode selected...
    bs.PopulateBodyPartsForDisplay(advancedMode);
    // Clear tree
    ClearPartsTree;
    mInitNodeDataCnt:=0;
    // Reserve data in each node for the data
    PartsTree.NodeDataSize := SizeOf(TBodyPartForDisplay);

    if advancedMode then begin
        PartsTree.RootNodeCount:=1;
        CurrNode:=PartsTree.GetFirst;

        // Add nodes to the tree
        for i:=0 to Length(bs.mBodyPartsForDisplay)-1 do begin
            PartsTree.ChildCount[CurrNode] := bs.mBodyPartsForDisplay[i].mNumOfChildren;
            CurrNode:=PartsTree.GetNext(CurrNode);
        end;

        // All nodes expanded
        CurrNode:=PartsTree.GetFirst;
        for i:=0 to Length(bs.mBodyPartsForDisplay)-1 do begin
            PartsTree.Expanded[CurrNode]:=true;
            CurrNode:=PartsTree.GetNext(CurrNode);
        end;
    end
    else begin
        // Simple (flat) view
        PartsTree.RootNodeCount:=Length(bs.mBodyPartsForDisplay);
        CurrNode:=PartsTree.GetFirst;
        for i:=0 to Length(bs.mBodyPartsForDisplay)-1 do begin
            PartsTree.ChildCount[CurrNode] := 0;
            CurrNode:=PartsTree.GetNext(CurrNode);
        end;
    end;
end;

{ (Un)check all parts in the parts tree. }
procedure TFMsgPeeker.CheckAllParts(check: Boolean);
var i: Integer;
    CurrNode: PVirtualNode;
begin
    CurrNode:=PartsTree.GetFirst;
    for i:=0 to Length(bs.mBodyPartsForDisplay)-1 do begin
        CheckNode(CurrNode, check);
        CurrNode:=PartsTree.GetNext(CurrNode);
    end;
    PartsTree.Refresh;
end;

procedure TFMsgPeeker.GetUncheckedBodyPartIDs(var list: TStringList);
var i: Integer;
    CurrNode: PVirtualNode;
    Data: PBodyPartForDisplay;
begin
    CurrNode:=PartsTree.GetFirst;
    for i:=0 to Length(bs.mBodyPartsForDisplay)-1 do begin
        if CurrNode.CheckType = ctCheckBox then begin
            if CurrNode.CheckState=csUncheckedNormal then begin
                Data := PartsTree.GetNodeData(CurrNode);
                list.Add(Data.mID);
            end;
        end;
        CurrNode:=PartsTree.GetNext(CurrNode);
    end;
end;

procedure TFMsgPeeker.GetCheckedAttachmentNames(var list: TStringList);
var i: Integer;
    CurrNode: PVirtualNode;
    Data: PBodyPartForDisplay;
begin
    CurrNode:=PartsTree.GetFirst;
    for i:=0 to Length(bs.mBodyPartsForDisplay)-1 do begin
        if CurrNode.CheckType = ctCheckBox then begin
            if CurrNode.CheckState=csCheckedNormal then begin
                Data := PartsTree.GetNodeData(CurrNode);
                list.Add(Data.mName);
            end;
        end;
        CurrNode:=PartsTree.GetNext(CurrNode);
    end;
end;

{ (Un)check a single node (part) }
procedure TFMsgPeeker.CheckNode(Node: PVirtualNode; check: Boolean);
begin
    if Node.CheckType=ctCheckBox then begin
        if check then Node.CheckState:=csCheckedNormal
        else Node.CheckState:=csUncheckedNormal;
    end;
end;

procedure TFMsgPeeker.PartsTreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var Data: PBodyPartForDisplay;
begin
    try
        with Sender do begin
            Data := GetNodeData(Node);
            Data.mIsPart := bs.mBodyPartsForDisplay[mInitNodeDataCnt].mIsPart;
            Data.mID := bs.mBodyPartsForDisplay[mInitNodeDataCnt].mID;
            Data.mName := bs.mBodyPartsForDisplay[mInitNodeDataCnt].mName;
            Data.mSize:=bs.mBodyPartsForDisplay[mInitNodeDataCnt].mSize;
            Data.mEncoding:=bs.mBodyPartsForDisplay[mInitNodeDataCnt].mEncoding;
            //Data.mChecked:=bs.mBodyPartsForDisplay[mInitNodeDataCnt].mChecked;
            //if Pos('multipart',Data.mName)<>1 then begin
            if Data.mIsPart then begin
                Node.CheckType := ctCheckBox;
                CheckNode(Node,false);
            end;
        end;
        // Increment global counter for the array
        Inc(mInitNodeDataCnt);
    except
        devLog.Error('TFMsgPeeker.PartsTreeInitNode - Problem adding body part to the attachment list');
    end;
end;

procedure TFMsgPeeker.PartsTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var CurrNodeRec: PBodyPartForDisplay;
begin
    { Get a pointer to the data we reserved and initialized in OnInitNode }
    CurrNodeRec := PBodyPartForDisplay( Sender.GetNodeData(Node));
    if Assigned(CurrNodeRec) then begin
        case Column of
          0: CellText := CurrNodeRec.mName;
          1: CellText := IntToStr(CurrNodeRec.mSize);
        end;
    end;
end;

{ Toggles the tree view (simple/advanced) }
procedure TFMsgPeeker.ToggleTreeView;
begin
    settings.MessagePeeker.PartsAdvanced:=SpeedButton4.Down;
    PopulatePartsTree;
end;

{ Saves the message
  @param locally if true, will save the message locally, otherwise remotelly }
procedure TFMsgPeeker.SaveMessage(locally: Boolean);
var performSave: Boolean; uncheckedParts: TStringList; primaryPart, nextPart: String; i,p1,p2:Integer;
begin
    devLog.Trace('SaveMessage');
    mSaveLocally:=locally;
    performSave:=true;
    if not locally then begin
        // Check if the message has been modified and confirm
        if (not IsMessageModified) then begin
            MessageDlg('The message has not been modified and no parts have been selected for deletion. Save operation will not be performed',mtInformation,[mbOK],0);
            performSave:=false;
        end;
    end;

    uncheckedParts:=TStringList.Create;
    if performSave then begin
        // Simple view doesn't display the primary part, so we have to include it (if it exists)
        primaryPart:=bs.GetPrimaryTextPart;
        if not settings.MessagePeeker.PartsAdvanced then begin
            if (primaryPart<>'') then uncheckedParts.Add(primaryPart);
        end;

        // Get IDs of body parts checked in the tree
        GetUncheckedBodyPartIDs(uncheckedParts);
        if (uncheckedParts.Count=0) and (primaryPart='') then begin
            performSave:=false;
            MessageDlg('This message only contains attachments which you have checked for deletion. You can safely delete the complete message. Attachment deletion will not be performed.',mtInformation,[mbOK],0);
        end;
    end;

    if performSave and not locally then begin
        // Make sure the user confirms the save if saving remotelly
        performSave:=MessageDlg('Please confirm saving of the modified message to the server',mtConfirmation,mbYesNoCancel,0)=mrYes;
    end;

    if performSave then begin
        // If only the header has been modified
        if RichEditHeader.Modified and not AnyPartsMarkedForDeletion then begin
            // Download the whole message body
            PerformFetchWholeBody(mMboxIndex, mMsgUID, -1, rtFetchWholeBody);
        end
        else begin
            GetMainBoundaryFromHeader;
            mBodyPartsFetch:=TBodyPartsFetch.Create(uncheckedParts); // Freed from HandleRequest when all body parts have been fetched (or on Destroy if anything went wrong)

            // Invoke parts fetching
            nextPart:=mBodyPartsFetch.GetNextPartToFetch;
            if nextPart<>'DONE' then begin
                PerformFetchBodyPart(mMboxIndex, mMsgUID, nextPart, rtFetchMultipleBodyParts);
            end
        end;
        // Further control is managed in HandleRequestOver
    end;
    uncheckedParts.Free;
end;

{ Saves the (modified) message locally. }
procedure TFMsgPeeker.SaveMessageLocally;
begin
    if not IsMessageModified then begin
        MessageDlg('The message has not been modified. Use the Download to EML functionality to save the message locally',mtInformation,[mbOK],0);
    end
    else begin
        // Save Dialog sa settings.MiscSettings.EMLDir
        SaveDialog.InitialDir:=settings.MiscSettings.EMLDir;
        SaveDialog.Filter := '*.eml files|*.EML|All files (*.*)|*.*';
        if SaveDialog.Execute then begin
            settings.MiscSettings.EMLDir:=SaveDialog.Filename;
            SaveMessage(true);
        end;
    end;
end;

{ Saves the message remotelly to the currently selected folder }
procedure TFMsgPeeker.SaveMessageRemotelly;
begin
    if settings.MessagePeeker.PartsAdvanced and AreAllPartsMarkedForDeletion then
        MessageDlg('You have checked all parts for deletion. Please uncheck the part(s) you wish to keep.',mtInformation,[mbOK],0)
    else SaveMessage(false);
end;

{ Returns true if the header or body have been modified
  or if any body parts are marked for deletion }
function TFMsgPeeker.IsMessageModified: Boolean;
begin
    Result:=mBodyModified or RichEditHeader.Modified or AnyPartsMarkedForDeletion;
end;

{ Returns true if any of the parts are marked for deletion }
function TFMsgPeeker.AnyPartsMarkedForDeletion: Boolean;
var i: Integer; foundChecked: Boolean;
    CurrNode: PVirtualNode;
begin
    foundChecked:=false;
    CurrNode:=PartsTree.GetFirst;
    i:=0;
    while (not foundChecked) and (i<Length(bs.mBodyPartsForDisplay)) do begin
        if CurrNode.CheckState = csCheckedNormal then foundChecked:=true
        else CurrNode:=PartsTree.GetNext(CurrNode);
        Inc(i);
    end;
    Result:=foundChecked;
end;

function TFMsgPeeker.AreAllPartsMarkedForDeletion: Boolean;
var i: Integer; foundUnchecked: Boolean;
    CurrNode: PVirtualNode;
begin
    foundUnchecked:=false;  i:=0;
    CurrNode:=PartsTree.GetFirst;
    while (not foundUnchecked) and (i<Length(bs.mBodyPartsForDisplay)) do begin
        if CurrNode.CheckType = ctCheckBox then
            if CurrNode.CheckState = csUncheckedNormal then foundUnchecked:=true;
        if not foundUnchecked then CurrNode:=PartsTree.GetNext(CurrNode);
        Inc(i);
    end;
    Result:=not foundUnchecked;
end;

procedure TFMsgPeeker.PartsTreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var Data: PBodyPartForDisplay;
begin
    Data := Sender.GetNodeData(Node);
    if Assigned(Data) then Finalize(Data^);
end;

procedure TFMsgPeeker.SpeedButton2Click(Sender: TObject);
begin
    CheckAllParts(true);
end;

procedure TFMsgPeeker.SpeedButton3Click(Sender: TObject);
begin
    CheckAllParts(false);
end;

procedure TFMsgPeeker.SpeedButton4Click(Sender: TObject);
begin
    ToggleTreeView;
end;

procedure TFMsgPeeker.PageControl1Change(Sender: TObject);
var dRes: Integer;
begin
    if PageControl1.ActivePageIndex=PC_IDX_HTML then begin
        // HTML page selected, make sure the html part exists
        if not mPrimaryHTMLFetched then begin
            if bs.HasPrimaryHtmlPart then begin
                PerformFetchBodyPart(mMboxIndex, mMsgUID, bs.GetPrimaryHtmlPart, rtFetchPrimaryHtmlPart);
            end;
        end;
    end
    else if PageControl1.ActivePageIndex=PC_IDX_HEADER then begin
        if not mHeaderFetched then PerformFetchHeader(mMboxIndex, mMsgUID, rtFetchHeader);
    end
    else if PageControl1.ActivePageIndex=PC_IDX_RAW then begin
        if not mRawMessageFetched then begin
            // If the size of the message is bigger than the specified limit for
            // raw message retrieval then warn the user and present him with a dialog
            // where he can choose the ammount of bytes he wants to see...
            if (settings.IMAPOpSettings.LimitTextToFetch) and (mMsgInfo.mSize > settings.IMAPOpSettings.TextToFetchLength) then begin
                FDownloadSizeQueryDlg := TFDownloadSizeQueryDlg.Create(Self);
                FDownloadSizeQueryDlg.InitializeValues(mMsgInfo.mSize,settings.IMAPOpSettings.TextToFetchLength);
                dRes:=FDownloadSizeQueryDlg.ShowModal;
                if dRes=mrGetSpecified then begin
                    PerformFetchRawMessage(mMboxIndex, mMsgUID, FDownloadSizeQueryDlg.GetSpecified, rtFetchRawMessage);
                end
                else if dRes=mrGetFull then begin
                    PerformFetchRawMessage(mMboxIndex, mMsgUID, -1, rtFetchRawMessage);
                end;
                FDownloadSizeQueryDlg.Free;
            end
            else begin
                PerformFetchRawMessage(mMboxIndex, mMsgUID, -1, rtFetchRawMessage);
            end;
        end;
    end;
end;

procedure TFMsgPeeker.htmlLite1HotSpotClick(Sender: TObject;
  const SRC: String; var Handled: Boolean);
begin
    ShellExecute(Handle, 'open', PChar(SRC), nil, nil, SW_SHOW);
end;

procedure TFMsgPeeker.PartsTreeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var AnItem: PVirtualNode; p: TPoint; Data: PBodyPartForDisplay;
begin
    if Button=mbRight then begin
        AnItem := PartsTree.GetNodeAt(X,Y);
        if AnItem<>nil then begin
            PartsTree.Selected[AnItem]:=true;
            PartsTree.FocusedNode:=AnItem;
            p.x := X; p.y:= Y;
            p := PartsTree.ClientToScreen(p);
            Data := PartsTree.GetNodeData(AnItem);
            if (Data^.mIsPart) then begin
                mTreeX:=x;
                mTreeY:=y;
                PopupMenu1.Popup(p.X,p.Y);
            end;
        end;
    end
end;

procedure TFMsgPeeker.SaveAttachment1Click(Sender: TObject);
var Data: PBodyPartForDisplay; AnItem: PVirtualNode; mimeType, mimeSubtype: String;
begin
    SaveDialogAttach.InitialDir:=settings.MiscSettings.SaveAttachmentDir;
    if SaveDialogAttach.Execute then begin
        settings.MiscSettings.SaveAttachmentDir:=ExtractFilePath(SaveDialogAttach.Filename);
        AnItem := PartsTree.GetNodeAt(mTreeX,mTreeY);
        Data := PartsTree.GetNodeData(AnItem);
        mimeType:=LowerCase(bs.GetBodyMimePart(Data^.mId).mType);
        mimeSubtype:=bs.GetBodyMimePart(Data^.mId).mSubType;
        PerformGetAttachment(mMboxIndex, mMsgUID, Data^.mId,SaveDialogAttach.Filename,Data^.mEncoding, mimeType, mimeSubtype, rtSaveAttachment);
    end;
end;

procedure TFMsgPeeker.ViewAttachment1Click(Sender: TObject);
var Data: PBodyPartForDisplay; AnItem: PVirtualNode; mimeType, mimeSubtype: String; res: Integer;
begin
    AnItem := PartsTree.GetNodeAt(mTreeX,mTreeY);
    Data := PartsTree.GetNodeData(AnItem);
    mimeType:=LowerCase(bs.GetBodyMimePart(Data^.mId).mType);
    mimeSubtype:=bs.GetBodyMimePart(Data^.mId).mSubType;
    // Check if viewing of this attachment is supported by internal viewers
    if IsTypeSupported(mimeType,mimeSubtype) then
        PerformGetAttachment(mMboxIndex, mMsgUID, Data^.mId,Data^.mName,Data^.mEncoding, mimeType, mimeSubtype, rtViewAttachment)
    else begin
        res := MyMessageDialog('This attachment can not be viewed with internal viewers.'+CRLF+
            'You can choose to (1) Save the attachment and view it manually, (2) Try to open the attachment with an external application '+
            'or (3) cancel the action',mtInformation,['Save...', 'View Anyway','Cancel']);
        case res of
            1: SaveAttachment1Click(Self);
            2: PerformGetAttachment(mMboxIndex, mMsgUID, Data^.mId,Data^.mName,Data^.mEncoding, mimeType, mimeSubtype, rtViewAttachment);
        else
            // Do nothing (don't download), action canceled
        end;
    end;
end;

{ Returns true if the mime type/subtype is supported for internal viewing }
function TFMsgPeeker.IsTypeSupported(mimeType, mimeSubtype: String): Boolean;
var s: String;
begin
    Result:=false;
    if LowerCase(mimeType)='text' then begin
        s:=LowerCase(mimeSubtype);
        if ((s='plain') or (s='html')) then Result:=true;
    end
    else if LowerCase(mimeType)='image' then begin
        s:=LowerCase(mimeSubtype);
        if ((s='gif') or (s='jpg') or (s='jpeg') or (s='bmp') or (s='ico') or (s='emf') or (s='wmf')) then Result:=true;
    end
    else if LowerCase(mimeType)='message' then Result:=true;
end;

{ Validates the filename for any invalid characters and replaces them with a '_'
  If checkExistance is true, will check if the file exists and add a unique integer.
  Returns the full valid file path }
function TFMsgPeeker.ValidateFilename(dir,filename: String; checkExistance: Boolean): String;
var fName, fNameOrg: String; invCharRegEx: TRegExpr; i:Integer;
begin
    invCharRegEx:=TRegExpr.Create;
    invCharRegEx.Expression:=invCharRE;
    fName:=invCharRegEx.Replace(filename,'_');
    fNameOrg:=fName;
    if checkExistance then begin
        i:=0;
        while FileExists(dir+'\'+fName) do begin
            fName:=Format('%d%s',[i,fNameOrg]);
            Inc(i);
        end;
    end;
    Result:=dir+'\'+fName;
end;

procedure TFMsgPeeker.MenuItemDeleteAttachmentsClick(Sender: TObject);
begin
    if bsParsed then begin
        // Get the header if not yet fetched
        if not mHeaderFetched then PerformFetchHeader(mMboxIndex, mMsgUID, rtFetchHeaderForQSDeletion)
        else QuickSmartDeleteAttachment;
    end
    else begin
        devLog.Error('Body structure parsing failed. Can''t procede with QS attachment deletion');
        MessageDlg('Can''t perform attachment deletion. Body structure parsing failed.',mtError,[mbOK],0);
    end;
end;

procedure TFMsgPeeker.MenuItemSaveRemoteClick(Sender: TObject);
begin
    if not mHeaderFetched then PerformFetchHeader(mMboxIndex, mMsgUID, rtFetchHeaderForRemoteSaving)
    else SaveMessageRemotelly;
end;

procedure TFMsgPeeker.MenuItemSaveLocalClick(Sender: TObject);
begin
    if not mHeaderFetched then PerformFetchHeader(mMboxIndex, mMsgUID, rtFetchHeaderForLocalSaving)
    else SaveMessageLocally;
end;

procedure TFMsgPeeker.Closeviewer1Click(Sender: TObject);
begin
    Close;
end;

end.
