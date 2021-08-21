{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: MessageLists.pas,v 1.12 2004/03/31 23:27:34 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit MessageLists;

interface

uses Log, VecLog, sysutils, windows, mytypes, myutil;

const SEARCH_LIST_INDEX = 0;

type

  TMsgSortField = (msfFrom, msfTo, msfSubject, msfDate, msfSize, msfSpamScore, msfUID, msfNone);
  // msfNone will sort by UID ascending
  TMsgSortType = (mstString, mstFloatString, mstInteger, mstDate);
  TMsgSortDir = (msdAsc, msdDsc);

  TMessageFlags = record
    mSeen: Boolean;
    mAnswered: Boolean;
    mDeleted: Boolean;
    mFlagged: Boolean;
    mDraft: Boolean;
  end;
  PMessageFlags = ^TMessageFlags;

  TMessageInfo = record
    mUID: String;
    mSize: Integer;
    mSizeDisplay: String;
    mDate: TDateTime;
    mDateStr: String;
    mSubject: String;
    mFrom: String;
    mTo: String;
    mImageIndex: Integer;
    mFlags: TMessageFlags;
    mSpamScore: String;
    mIsSpam: Boolean;
    mHasAttachments: Boolean;
  end;
  TListOfMessageInfos = array of TMessageInfo;

  { A record which stores information about the messages
    that should be added to the message list in the GUI.
    This is used for WM_UPDATE_MESSAGE_LIST messages only.
    This solution is unsafe and temporary, will need to implement as
    a FIFO queue of these records }
  TPageDisplayInfo = record
    mailboxIndex: Integer;  // Mailbox which needs updating
    fromMessage: Integer;   // Index in the messageList of the first message to be added to the gui list
    toMessage: Integer;     // Index in the messageList of the last message to be added to the gui list
    clear: Boolean;         // True if the gui message list should be cleared
  end;

  TMessageLists = class
    ItemsCS: TRTLCriticalSection;
    // mboxLists is a list of message lists
    // List at index 0 is used for searches (since it is an Account node in mboxTree.Items)
    mboxLists: array of TListOfMessageInfos;
    searchDisplayedIndex: Integer;   // index of the mbox whose search results are displayed. -1 if none
    searchOriginalSortField: TMsgSortField;  // sort field of the original search
    searchOriginalSortDir: TMsgSortDir;
    hasOriginal: Boolean;   // true when a search is displayed without the original existing (happens on global search where
                            // searched messages are retrieved without retrieving all messages
    pageDisplayInfo: TPageDisplayInfo;
    constructor Create;
    procedure Clear;
    procedure createMailboxes(num: Integer);
    procedure AddMessageList(msgList: TListOfMessageInfos; mboxIndex: Integer);
    procedure AddSearchResultsToMessageLists(msgList: TListOfMessageInfos; mboxIndex: Integer);
    procedure AddGlobalSearchResultsToMessageLists(msgList: TListOfMessageInfos; mboxIndex: Integer);
    procedure AddMessageInfoToMessageList(mboxIndex: Integer; msgInfo: TMessageInfo);
    function GetMessageInfo(mboxIndex, msgIndex: Integer):TMessageInfo;
    function GetMessageInfoByUID(var msgInfo: TMessageInfo; mboxIndex: Integer; uid: String): Integer;
    procedure UpdateMessageInfo(mboxIndex, msgIndex: Integer; msgInfo: TMessageInfo);
    function GetMailboxLength(mboxIndex: Integer): Integer;
    function GetMailboxSize(mboxIndex: Integer): Integer;
    procedure ClearMailbox(mboxIndex: Integer);
    procedure InsertMailbox(mboxIndex: Integer);
    procedure RemoveMailbox(mboxIndex: Integer);
    function RecoverOriginalSearched: Integer;
    function SimulateExpunge(mboxIndex: Integer): Integer;
    procedure ReassignImageIndexes(mboxIndex: Integer);
    procedure ReformatSizeDisplay(mboxIndex: Integer);
    procedure SortMsgsInMailbox(mboxIndex: Integer; sortField: TMsgSortField; direction: TMsgSortDir);
    procedure Quicksort(var A: TListOfMessageInfos; l, r: Integer; sortField: TMsgSortField; direction: TMsgSortDir);
    function HasDataForRecover: Boolean;
    procedure SetPageDisplayInfo(mboxIndex, msgFrom, msgTo: Integer; clear: Boolean);
    procedure EnterCS;
    procedure LeaveCS;
    destructor Destroy; override;
  end;

  function formatSizeDisplay(size: Integer): String;
  function intToMsgSortField(n: Integer): TMsgSortField;
  function intToMsgSortDir(n: Integer): TMsgSortDir;
  function msfToInt(msf: TMsgSortField): Integer;
  function msdToInt(msd: TMsgSortDir): Integer;
  function GetMessageImageIndex(size: Integer; hasAttachments: Boolean): Integer;
  procedure CopyMessageInfo(var fromNode, toNode: TMessageInfo);

implementation

uses Main, SpamHandles;

    constructor TMessageLists.Create;
    begin
        SetLength(mboxLists,0);
        searchDisplayedIndex := -1;
        hasOriginal:=false;
        InitializeCriticalSection(ItemsCS);
    end;

    { Clears the contents of all lists }
    procedure TMessageLists.Clear;
    var i: Integer;
    begin
        for i:=0 to Length(mboxLists)-1 do begin
            SetLength(mboxLists[i],0);
        end;
        SetLength(mboxLists,0);
        searchDisplayedIndex := -1;
        hasOriginal:=false;
    end;

    { Prepares the big list for accepting small lists }
    procedure TMessageLists.createMailboxes(num: Integer);
    var i: Integer;
    begin
        SetLength(mboxLists,num);
        for i:=0 to Length(mboxLists)-1 do begin
            SetLength(mboxLists[i],0);
        end;
    end;

    procedure TMessageLists.AddMessageList(msgList: TListOfMessageInfos; mboxIndex: Integer);
    var i: Integer;
    begin
        EnterCS;
        try
            try
                SetLength(mboxLists[mboxIndex],Length(msgList));
                for i:=0 to Length(msgList)-1 do begin
                    CopyMessageInfo(msgList[i],mboxLists[mboxIndex][i]);
                end;

                if (mMsgSortField <> msfNone) then
                    if (Length(msgList) > 1) then
                        SortMsgsInMailbox(mboxIndex,mMsgSortField,mMsgSortDir);
            except
                devLog.Error('Unable to add message to message list');
            end;
         finally
            LeaveCS;
         end;
    end;

    procedure TMessageLists.AddSearchResultsToMessageLists(msgList: TListOfMessageInfos; mboxIndex: Integer);
    var i: Integer;
    begin
        EnterCS;
        try
            searchDisplayedIndex := mboxIndex;
            SetLength(mboxLists[SEARCH_LIST_INDEX],Length(msgList));
            for i:=0 to Length(msgList)-1 do begin
                CopyMessageInfo(msgList[i],mboxLists[SEARCH_LIST_INDEX][i]);
            end;
            if (mMsgSortField <> msfNone) then
                if (Length(msgList) > 1) then
                    SortMsgsInMailbox(SEARCH_LIST_INDEX,mMsgSortField,mMsgSortDir);
            searchOriginalSortField:= mMsgSortField;  // sort field of the original search
            searchOriginalSortDir:= mMsgSortDir;
            hasOriginal := true;
        finally
            LeaveCS;
        end;
    end;

    procedure TMessageLists.AddGlobalSearchResultsToMessageLists(msgList: TListOfMessageInfos; mboxIndex: Integer);
    var i: Integer;
    begin
        if mDisplayedNodeIndex = mboxIndex then begin
            if searchDisplayedIndex = mboxIndex then begin
                // do nothing, previous search results of this mailbox are displayed
                // searchDisplayedIndex and hasOriginal (true) have proper values
            end
            else begin
                hasOriginal:=false;
                searchDisplayedIndex := mboxIndex;
            end;
        end
        else begin
            hasOriginal:=false;
            searchDisplayedIndex := mboxIndex;
        end;

        EnterCS;
        try
            SetLength(mboxLists[SEARCH_LIST_INDEX],Length(msgList));
            for i:=0 to Length(msgList)-1 do begin
                CopyMessageInfo(msgList[i],mboxLists[SEARCH_LIST_INDEX][i]);
            end;
            if (mMsgSortField <> msfNone) then
                if (Length(msgList) > 1) then
                    SortMsgsInMailbox(SEARCH_LIST_INDEX,mMsgSortField,mMsgSortDir);
        finally
            LeaveCS;
        end;
    end;

    { Adds a message to the message list. Will automatically update the search
      list if the search is displayed for this mailbox }
    procedure TMessageLists.AddMessageInfoToMessageList(mboxIndex: Integer; msgInfo: TMessageInfo);
    var lastIndex: Integer;
    begin
        EnterCS;
        try
            //@todo test this
            // We basically don't add a message only if a global search without original data
            if HasDataForRecover then begin
                lastIndex:=Length(mboxLists[mboxIndex]);
                SetLength(mboxLists[mboxIndex],lastIndex+1);
                CopyMessageInfo(msgInfo,mboxLists[mboxIndex][lastIndex]);
            end;

            // Update the search list if search is displayed
            // @todo take care here, maybe the new message (usually will get here on attch deletion)
            // doesn't contain the search string, but will work for now...
            if searchDisplayedIndex = mboxIndex then begin
                // This way both lists are always up to date.
                lastIndex:=Length(mboxLists[SEARCH_LIST_INDEX]);
                SetLength(mboxLists[SEARCH_LIST_INDEX], lastIndex+1);
                CopyMessageInfo(msgInfo, mboxLists[SEARCH_LIST_INDEX][lastIndex]);
            end;
        finally
            LeaveCS;
        end;
    end;

    function TMessageLists.GetMessageInfo(mboxIndex, msgIndex: Integer):TMessageInfo;
    begin
        if searchDisplayedIndex = mboxIndex then begin
            //devLog.Trace('in getMsgInfo, mboxIndex=0; msgIndex: '+IntToStr(msgIndex));
            //devLog.Trace('length of 0: '+inttostr(Length(mboxLists[0])));
            Result:=mboxLists[SEARCH_LIST_INDEX][msgIndex]
        end
        else
            Result:=mboxLists[mboxIndex][msgIndex];
    end;

    { Retrieves the MessageInfo with the specified uid from the specified mailbox
      This is returned as a var param. Function returns the index of the message in the list,
      -1 if it is not found}
    function TMessageLists.GetMessageInfoByUID(var msgInfo: TMessageInfo; mboxIndex: Integer; uid: String): Integer;
    var found: Boolean; i,lt: Integer;
    begin
        if searchDisplayedIndex = mboxIndex then mboxIndex := SEARCH_LIST_INDEX;
        found:=false;
        i:=0;
        lt:=Length(mboxLists[mboxIndex]);
        while ((not found) and (i<lt)) do begin
            msgInfo:=mboxLists[mboxIndex][i];
            if (msgInfo.mUID = uid) then found:=true
            else Inc(i);
        end;
        if found then Result:=i
        else Result:=-1;
    end;

    procedure TMessageLists.UpdateMessageInfo(mboxIndex, msgIndex: Integer; msgInfo: TMessageInfo);
    var found: Boolean; i,lt: Integer; msgInfoOld: TMessageInfo;
    begin
        // Update original list (if exists)
        if HasDataForRecover then
            mboxLists[mboxIndex][msgIndex]:=msgInfo;
        if searchDisplayedIndex = mboxIndex then begin
            // Search results are displayed. Have to update the message in search list too
            // This way both lists are always up to date.
            found:=false;
            i:=0;
            lt:=Length(mboxLists[SEARCH_LIST_INDEX]);
            while ((not found) and (i<lt)) do begin
                msgInfoOld:=mboxLists[SEARCH_LIST_INDEX][i];
                if (msgInfoOld.mUID = msgInfo.mUID) then begin
                    mboxLists[SEARCH_LIST_INDEX][i]:=msgInfo;
                    found:=true
                end
                else Inc(i);
            end;
        end;
    end;

    { Calculates the number of messages in displayed/searched mailbox }
    function TMessageLists.GetMailboxLength(mboxIndex: Integer): Integer;
    begin
        if searchDisplayedIndex = mboxIndex then mboxIndex := SEARCH_LIST_INDEX;
        Result:=Length(mboxLists[mboxIndex]);
    end;

    { Calculates the size of the mailbox or size of searched messages if search is displayed }
    function TMessageLists.GetMailboxSize(mboxIndex: Integer): Integer;
    var i, sum: Integer;
    begin
        sum:=0;
        if searchDisplayedIndex = mboxIndex then mboxIndex:=SEARCH_LIST_INDEX;
        for i:=0 to Length(mboxLists[mboxIndex])-1 do begin
            sum:=sum + mboxLists[mboxIndex][i].mSize;
        end;
        Result:=sum;
    end;

    procedure TMessageLists.ClearMailbox(mboxIndex: Integer);
    begin
        if searchDisplayedIndex = mboxIndex then mboxIndex := SEARCH_LIST_INDEX;
        if mboxIndex<Length(mboxLists) then 
            SetLength(mboxLists[mboxIndex],0);
    end;

    { Inserts a new mailbox at position mboxIndex. All subsequent mailboxes will
      be moved by one place up. This is used for situations when a new mailbox is
      created. The inserted mailbox is empty }
    procedure TMessageLists.InsertMailbox(mboxIndex: Integer);
    var i,j: Integer;
    begin
        devLog.Debug(Format('*** new index=%d, length=%d',[mboxIndex,Length(mboxLists)]));
        if mboxIndex<=Length(mboxLists) then begin
            // First create space at the end of the list
            SetLength(mboxLists,Length(mboxLists)+1);
            SetLength(mboxLists[Length(mboxLists)-1],0);
            // From the last mailbox backwards, copy mailbox lists one space up
            for i:=Length(mboxLists)-2 downto mboxIndex do begin
                SetLength(mboxLists[i+1],Length(mboxLists[i]));
                for j:=0 to Length(mboxLists[i])-1 do begin
                    CopyMessageInfo(mboxLists[i][j],mboxLists[i+1][j]);
                end;
                // Clear the original box
                SetLength(mboxLists[i],0);
            end;
        end
        else begin
            devLog.Warn('TMessageLists.InsertMailbox: Invalid mboxIndex');
        end;
    end;

    { Removes a mailbox from the message list. To perform on mailbox deletion }
    procedure TMessageLists.RemoveMailbox(mboxIndex: Integer);
    var i,j: Integer;
    begin
        //dbg('*** mboxIndex='+inttostr(mboxIndex)+', length='+inttostr(length(mboxLists)));
        if mboxIndex<Length(mboxLists) then begin
            SetLength(mboxLists[mboxIndex],0);
            // Move mailboxes down
            for i:=mboxIndex+1 to Length(mboxLists)-1 do begin
                SetLength(mboxLists[i-1],Length(mboxLists[i]));
                //dbg('*** '+Format('mbox %d new length %d',[i-1,Length(mboxLists[i])]));
                for j:=0 to Length(mboxLists[i])-1 do begin
                    CopyMessageInfo(mboxLists[i][j],mboxLists[i-1][j]);
                end;
                // Clear the original box
                SetLength(mboxLists[i],0);
            end;
            SetLength(mboxLists,Length(mboxLists)-1);
        end
        else begin
            devLog.Warn('TMessageLists.InsertMailbox: Invalid mboxIndex');
        end;
    end;

    { This method checks if there is data to recover to after a search.
      Will return FALSE only if a search is displayed and there is no list of messages to display,
      should happen on global search only }
    function TMessageLists.HasDataForRecover: Boolean;
    var value: Boolean;
    begin
        // If there is no search displayed then we have data for recover (the data itself)
        if (searchDisplayedIndex = -1) then value:=true
        // else it depends if we have the original (non-searched) messages
        else value := hasOriginal;

        Result:=value;
    end;

    { Returns the index of the recovered mailbox }
    function TMessageLists.RecoverOriginalSearched: Integer;
    var ind: Integer;
    begin
        ind:=searchDisplayedIndex;
        EnterCS;
        try
            if searchDisplayedIndex > -1 then begin
                SetLength(mboxLists[SEARCH_LIST_INDEX],0);
                searchDisplayedIndex := -1;
                // Resort if needed
                if (searchOriginalSortField<>mMsgSortField) or (searchOriginalSortDir<>mMsgSortDir) then begin
                    if (Length(mboxLists[ind]) > 1) then
                        SortMsgsInMailbox(ind,mMsgSortField,mMsgSortDir);
                end;
            end;
            hasOriginal:=false;
            Result:=ind;
        finally
            LeaveCS;
        end;
    end;

    { Simulates an expunge: removes all messages marked as deleted from the list }
    { Returns the new size of the folder }
    function TMessageLists.SimulateExpunge(mboxIndex: Integer): Integer;
    var i, j: Integer; msg: TMessageInfo; size: Integer;
    begin
        // In case a search is displayed, recover the original
        // (we want to display the contents of the whole folder when an expunge is performed)

        //!!! Should see what to do here if doesn't have original. A little tricky...
        RecoverOriginalSearched;

        EnterCS;
        try
            j:=0;
            size:=0;
            for i:=0 to Length(mboxLists[mboxIndex])-1 do begin
                msg:=mboxLists[mboxIndex][i];
                if (not msg.mFlags.mDeleted) then begin
                    size:=size+msg.mSize;
                    CopyMessageInfo(mboxLists[mboxIndex][i],mboxLists[mboxIndex][j]);
                    Inc(j);
                end;
            end;
            SetLength(mboxLists[mboxIndex],j);
            Result:=size;
        finally
            LeaveCS;
        end;
    end;

    { Goes through the currently displayed mailbox and reassigns the image indexes
      based on the settings in 'settings'. If a search is displayed, will do the
      same with the backup message array
      @param mboxIndex: the currently displayed mailbox
    }
    procedure TMessageLists.ReassignImageIndexes(mboxIndex: Integer);
    var i, size: Integer;
    begin
        EnterCS;
        try
            for i:=0 to Length(mboxLists[mboxIndex])-1 do begin
                size:=mboxLists[mboxIndex][i].mSize;
                mboxLists[mboxIndex][i].mImageIndex:=GetMessageImageIndex(size,mboxLists[mboxIndex][i].mHasAttachments);
            end;
            if searchDisplayedIndex = mboxIndex then begin
                for i:=0 to Length(mboxLists[SEARCH_LIST_INDEX])-1 do begin
                    size:=mboxLists[SEARCH_LIST_INDEX][i].mSize;
                    mboxLists[SEARCH_LIST_INDEX][i].mImageIndex:=GetMessageImageIndex(size, mboxLists[SEARCH_LIST_INDEX][i].mHasAttachments);
                end;
            end;
        finally
            LeaveCS;
        end;
    end;

    procedure TMessageLists.ReformatSizeDisplay(mboxIndex: Integer);
    var i: Integer;
    begin
        EnterCS;
        try
            for i:=0 to Length(mboxLists[mboxIndex])-1 do
                mboxLists[mboxIndex][i].mSizeDisplay := formatSizeDisplay(mboxLists[mboxIndex][i].mSize);
            if searchDisplayedIndex = mboxIndex then begin
                for i:=0 to Length(mboxLists[SEARCH_LIST_INDEX])-1 do
                    mboxLists[SEARCH_LIST_INDEX][i].mSizeDisplay:=formatSizeDisplay(mboxLists[SEARCH_LIST_INDEX][i].mSize);
            end;
        finally
            LeaveCS;
        end;
    end;

    { This method is not synchronized with a critical section, so that should be done outside }
    procedure TMessageLists.SortMsgsInMailbox(mboxIndex: Integer; sortField: TMsgSortField; direction: TMsgSortDir);
    begin
        if searchDisplayedIndex = mboxIndex then mboxIndex := SEARCH_LIST_INDEX;
        Quicksort(mboxLists[mboxIndex],0,Length(mboxLists[mboxIndex])-1, sortField, direction);
    end;

    { Sets values used for adding messages to the message list gui }
    procedure TMessageLists.SetPageDisplayInfo(mboxIndex, msgFrom, msgTo: Integer; clear: Boolean);
    begin
        pageDisplayInfo.mailboxIndex:=mboxIndex;
        pageDisplayInfo.fromMessage:=msgFrom;
        pageDisplayInfo.toMessage:=msgTo;
        pageDisplayInfo.clear:=clear;
    end;

    procedure TMessageLists.Quicksort(var A: TListOfMessageInfos; l, r: Integer; sortField: TMsgSortField; direction: TMsgSortDir);
    var
      i, j: Integer;
      Help: TMessageInfo;
      sortType: TMsgSortType;
      vInt: Integer;
      vStr: String;
      vDate: TDateTime;
      vFloat: Float;

      function getStringField(i: Integer; sortField: TMsgSortField): String;
      begin
          case sortField of
            msfFrom: Result := A[i].mFrom;
            msfTo: Result := A[i].mTo;
            msfSubject: Result := A[i].mSubject;
            msfSpamScore: Result := A[i].mSpamScore;  // this doesn't correctly sort 19.9 and 9.8
          end;
      end;

      function getFloatField(i: Integer; sortField: TMsgSortField): Float;
      begin
          case sortField of
            msfSpamScore: Result:=StrToFloatSafe(A[i].mSpamScore);
          end;
      end;

      function getIntField(i: Integer; sortField: TMsgSortField): Integer;
      begin
          case sortField of
            msfSize: Result := A[i].mSize;
            msfUID, msfNone: Result := StrToInt(A[i].mUID);
          end;
      end;

    begin

      case sortField of
        msfFrom: begin vStr := A[(l+r) div 2].mFrom; sortType:= mstString; end;
        msfTo: begin vStr := A[(l+r) div 2].mTo; sortType:= mstString; end;
        msfSubject: begin vStr := A[(l+r) div 2].mSubject; sortType:= mstString; end;
        msfDate: begin vDate := A[(l+r) div 2].mDate; sortType:= mstDate; end;
        msfUID, msfNone: begin vInt := StrToInt(A[(l+r) div 2].mUID); sortType:= mstInteger; end;
        msfSpamScore:
            begin
                if (FMain.pActiveAccount.SpamHandle <> 0) and (settings.SpamHandles.GetSpamHandle(FMain.pActiveAccount.SpamHandle-1).SpamComparedBy = 0) then
                    begin vFloat := StrToFloatSafe(A[(l+r) div 2].mSpamScore); sortType:= mstFloatString; end
                else
                    begin vStr := A[(l+r) div 2].mSpamScore; sortType:= mstString; end;
            end;
        msfSize: begin vInt := A[(l+r) div 2].mSize; sortType:= mstInteger; end;
      end;

      i := l; j := r;
      repeat
        case sortType of
            mstString: begin
                if direction = msdAsc then begin
                    while getStringField(i,sortField) < vStr do inc(i);
                    while vStr < getStringField(j,sortField) do dec(j);
                end
                else begin
                    while getStringField(i,sortField) > vStr do inc(i);
                    while vStr > getStringField(j,sortField) do dec(j);
                end;
            end;
            mstFloatString: begin
                if direction = msdAsc then begin
                    while getFloatField(i,sortField) < vFloat do inc(i);
                    while vFloat < getFloatField(j,sortField) do dec(j);
                end
                else begin
                    while getFloatField(i,sortField) > vFloat do inc(i);
                    while vFloat > getFloatField(j,sortField) do dec(j);
                end;
            end;
            mstInteger: begin
                if direction = msdAsc then begin
                    while getIntField(i,sortField) < vInt do inc(i);
                    while vInt < getIntField(j,sortField) do dec(j);
                end
                else begin
                    while getIntField(i,sortField) > vInt do inc(i);
                    while vInt > getIntField(j,sortField) do dec(j);
                end;
            end;
            mstDate: begin
                if direction = msdAsc then begin
                    while A[i].mDate < vDate do inc(i);
                    while vDate < A[j].mDate do dec(j);
                end
                else begin
                    while A[i].mDate > vDate do inc(i);
                    while vDate > A[j].mDate do dec(j);
                end;
            end;
        end;

        if i <= j then begin
          Help := A[i]; A[i]:= A[j]; A[j]:= Help;
          inc(i); dec(j);
        end;
      until i > j;
      if l < j then Quicksort(A, l, j, sortField, direction);
      if i < r then Quicksort(A, i, r, sortField, direction);
    end;

    { Enters critical section for the node array (Items) }
    procedure TMessageLists.EnterCS;
    begin
        EnterCriticalSection(ItemsCS);
    end;

    { Leaves critical section for the node array (Items) }
    procedure TMessageLists.LeaveCS;
    begin
        LeaveCriticalSection(ItemsCS);
    end;

    destructor TMessageLists.Destroy;
    begin
        Clear;
        DeleteCriticalSection(ItemsCS);
    end;

{ Formats the display of the size (individual size) }
function formatSizeDisplay(size: Integer): String;
var str: String;
    function convertByte(size: Integer): String;
    begin Result := FloatToStrF(size,ffNumber,12,0); end;
begin
    str:=IntToStr(size);  // Default (for 0 (byte))
    case settings.MessageDisplay.SizeFormat of
       -1,0: str:=convertByte(size);
       1: str := FloatToStrF(size/1024,ffNumber,6,1) + 'K'; // KB
    else str:=convertByte(size);
    end;
    Result:=str;
end;

{ Converts an integer into a TMsgSortField }
function intToMsgSortField(n: Integer): TMsgSortField;
var r: TMsgSortField;
begin
    case n of
        0: r:=msfFrom;
        1: r:=msfTo;
        2: r:=msfSubject;
        3: r:=msfDate;
        4: r:=msfSize;
        5: r:=msfSpamScore;
        6: r:=msfUID;
    else
        r := msfNone;
    end;
    Result:=r;
end;

function msfToInt(msf: TMsgSortField): Integer;
begin
    Result:=ord(msf);
end;

function intToMsgSortDir(n: Integer): TMsgSortDir;
begin
    if n=0 then Result:=msdAsc
    else Result:=msdDsc;
end;

function msdToInt(msd: TMsgSortDir): Integer;
begin
    Result:=ord(msd);
end;

{ Calculates the image to be displayed beside the message based on the size and attachement state }
function GetMessageImageIndex(size: Integer; hasAttachments: Boolean): Integer;
var index: Integer;
begin
    if size < (settings.MessageDisplay.Lower * 1024) then begin
        if hasAttachments then index:=3 else index:=0;
    end
    else if size < (settings.MessageDisplay.Upper * 1024) then begin
        if hasAttachments then index:=4 else index:=1;
    end
    else begin
        if hasAttachments then index:=5 else index:=2;
    end;
    Result:=index;
end;

procedure CopyMessageInfo(var fromNode, toNode: TMessageInfo);
begin
    toNode.mUID:=fromNode.mUID;
    toNode.mSize:=fromNode.mSize;
    toNode.mSizeDisplay:=fromNode.mSizeDisplay;
    toNode.mDate:=fromNode.mDate;
    toNode.mDateStr:=fromNode.mDateStr;
    toNode.mSubject:=fromNode.mSubject;
    toNode.mFrom:=fromNode.mFrom;
    toNode.mTo:=fromNode.mTo;
    toNode.mImageIndex:=fromNode.mImageIndex;
    toNode.mFlags.mSeen:=fromNode.mFlags.mSeen;
    toNode.mFlags.mAnswered:=fromNode.mFlags.mAnswered;
    toNode.mFlags.mDeleted:=fromNode.mFlags.mDeleted;
    toNode.mFlags.mFlagged:=fromNode.mFlags.mFlagged;
    toNode.mFlags.mDraft:=fromNode.mFlags.mDraft;
    toNode.mSpamScore:=fromNode.mSpamScore;
    toNode.mIsSpam:=fromNode.mIsSpam;
    toNode.mHasAttachments:=fromNode.mHasAttachments;
end;



end.