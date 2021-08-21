{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id$
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit BodyStructure;

interface

uses classes, sysutils, VecLog, Log, splitfns, Stacks, MyUtil;

const
    PARTS_DEPTH = 20;

    STATE_START = 0;
    STATE_PART_COULD_START = 1;  // A body part should start (might get another "(" as the next symbol)
    STATE_PART_ENDED = 2;
    STATE_PART_STARTING = 3;
    STATE_TYPE_STARTED = 4;
    STATE_TYPE_ENDED = 5;
    STATE_SUBTYPE_STARTED = 6;
    STATE_SUBTYPE_ENDED = 7;
    PARAMS_STARTED = 8;
    PARAM_STARTED = 9;
    PARAM_ENDED = 10;
    PARAMS_ENDED = 11;
    BODY_ID_STARTED = 12;
    BODY_ID_ENDED = 13;
    DESCRIPTION_STARTED = 14;
    DESCRIPTION_ENDED = 15;
    ENCODING_STARTED = 16;
    ENCODING_ENDED = 17;
    SIZE_STARTED = 18;
    SIZE_ENDED = 19;
    MULTIPART_DESC_STARTED = 20;
    MULTIPART_DESC_ENDED = 21;
    // MESSAGE/RFC822 requires some special action after the size
    RFC_ENVELOPE = 22;
    RFC_BODYSTRUCTURE_SIMPLE = 23;
    RFC_BODYSTRUCTURE_COMPLEX = 24;


    STATE_DONE = 99;
    STATE_ERROR = 999;

type

    // Record for passing information about the stripped message to other components.
    // A stripped message is the message that remains when the attachments are deleted.
    TStrippedMessageInfo = record
        mBodyPartID: String;
        mContentType: String;  // includes subtype
        mCharset: String;
        mEncoding: String;
    end;

    TBodyMimePart = class
      private
        function ToString: String;
      public
        mID: String;    // ID of the part, as determined by the server (or by us ;)
        mType: String;
        mSubtype: String;
        mParameterList: TStringList;
        mBodyId: String;
        mDescription: String;
        mEncoding: String;
        mSize: Integer;
        //mLinesSize: Integer;  // only for TEXT and MESSAGE/RFC822
        constructor Create;
        destructor Destroy; override;
    end;

    { Add subclasses as necessary, eg:
    // Special case of this class, contains the BODY and ENVELOPE of the enclosing message
    TBodyMimePartMessageRFC822 = class(TBodyMimePart)
        // Following used for type/subtype MESSAGE/RFC822 only
        mBody: ^TBodyMimePart;
        mEnvelope: ^TEnvelope;
    end;
    }

    // A compact version of a body part, used for displaying in the
    // list (or tree) of body parts...
    TBodyPartForDisplay = record
        mIsPart: Boolean;   // Defines if this node is a real part or just a multipart holder
        mID: String;        // ID of the part (eg 1.2.1)
        mName: String;      // String to display in the tree (depends on what the body part is)
        mType: String;
        mSubtype: String;
        mEncoding: String;
        mSize: Integer;
        // mChecked: Boolean;  // True if the body part is checked for deletion
        // Tree related
        mNumOfChildren: Integer;
        mParentID: Integer;
    end;
    PBodyPartForDisplay = ^TBodyPartForDisplay;
    TListOfBodyPartsForDisplay = array of TBodyPartForDisplay;

    TBodyStructure = class
    private
        mBodyStructureString: String;
        mMultipartStack: TIntegerStack;
        mRFC822Stack: TIntegerStack;     // Used just for indicating whether we're in a complex message/rfc822 bodystructure. Stores original number of open braces.
        procedure DumpContents;
        function GetName(bodyPart: TBodyMimePart; attachmentOnly: Boolean): String;
        function GetLevel(bodyPartId: String): Integer;
        function GetParentIndex(index: Integer): Integer;
    public
        mBodyParts: TList;
        mBodyPartsForDisplay: TListOfBodyPartsForDisplay;
        mPrimaryTextPart: String; // ID of the 'primary' part (main text of the message)
        mPrimaryHTMLPart: String; // ID of the primary HTML part (Alternative HTML) if it exists
        constructor Create;
        destructor Destroy; override;
        function Parse(bs: String): Boolean;
        procedure Clear;
        function GetStrippedMessageInfo: TStrippedMessageInfo;
        procedure PopulateBodyPartsForDisplay(hierarchy: Boolean);
        function GetBodyMimePart(id: String): TBodyMimePart;
        function GetPrimaryTextPart: String;
        function HasPrimaryTextPart: Boolean;
        function GetPrimaryHtmlPart: String;
        function HasPrimaryHtmlPart: Boolean;
        function CalculatePrimaryTextPart: String;
        function CalculatePrimaryHtmlPart: String;
        procedure GetAttachmentList(var list: TStringList);
        property bodyStructureString: String read mBodyStructureString;
    end;

function IsDigit(c: char): Boolean;

implementation

uses Main;

{ Will trim any unnecessary stuff from the end. }
constructor TBodyStructure.Create;
begin
    // Currently doesn't trim unnecessary stuff...
    mBodyParts:=TList.Create;
    mMultipartStack:=TIntegerStack.Create;
    mRFC822Stack:=TIntegerStack.Create;
    SetLength(mBodyPartsForDisplay,0);
end;

procedure TBodyStructure.Clear;
begin
    mBodyStructureString:='';
    mBodyParts.Clear;
end;

destructor TBodyStructure.Destroy;
var i: Integer;
begin
    for i:=0 to mBodyParts.Count-1 do
        TBodyMimePart(mBodyParts[i]).Free;
    mBodyParts.Free;
    mMultipartStack.Free;
    mRFC822Stack.Free;
    SetLength(mBodyPartsForDisplay,0);
    Inherited Destroy;
end;

function TBodyStructure.Parse(bs: String): Boolean;
var bsr: String;
    thisPartOpenBraceCount, openBraceCount: Integer;
    i, j, state, tmp: Integer;
    thisBodyPart: TBodyMimePart;
    token: String;
    partCnts: array [1..PARTS_DEPTH] of Integer; // array that stores counters for each part...
    envOpenBrace, envCloseBrace: Integer;

    function GetBodyPartID: String;
    var j: Integer; res: String;
    begin
        // Form the part ID of the starting body part
        res := Format('%d',[partCnts[1]]);
        j:=2;
        while (j<=PARTS_DEPTH) and (partCnts[j]>0) do begin
            res := res + '.' + Format('%d',[partCnts[j]]);
            j:=j+1;
        end;
        Result:=res;
    end;

    // Removes the last positive value from partCnts (e.g. 1 2 2 0 -> 1 2 0 0)
    procedure ClearLastPositivePartID;
    var done: Boolean; k: Integer;
    begin
        // Skip if second part is 0
        done:=false;
        if partCnts[2]<>0 then begin
            k:=2;
            while (k<PARTS_DEPTH) and (not done) do begin
                if (partCnts[k]>0) and (partCnts[k+1]=0) then begin
                    partCnts[k]:=0;
                    done:=true;
                end;
                k:=k+1;
            end;
            if not done then begin
                partCnts[PARTS_DEPTH]:=0;
                devLog.Warn('Maximum MIME part depth reached. Errors possible. Please contact the author.');
            end;
        end;
    end;

begin
    Clear; // clear previous contents
    mBodyStructureString:=bs;
    devLog.Trace('BODY Parser starting to parse: '+mBodyStructureString);

    bsr := mBodyStructureString;
    openBraceCount := 0;
    for j:=1 to PARTS_DEPTH do partCnts[j]:=0;
    state:=STATE_START;
    token:='';
    i:=5;  // Skip BODY
    while ((i<=Length(bsr)) and (state<STATE_DONE)) do begin

        if bsr[i]='(' then Inc(openBraceCount);
        if bsr[i]=')' then Dec(openBraceCount);

        // devLog.Trace(Format('-- bsr[%3d]=%s, state=%d, openBraceCount=%d',[i,bsr[i],state,openBraceCount]));

        case state of
            STATE_START:
                if bsr[i]='(' then begin
                    // Adjust body part counters
                    if (openBraceCount=1) and (bsr[i+1]='"') then begin
                        // ("TEXT/PLAIN... case - only one part
                        partCnts[1]:=1;
                    end
                    else begin
                        if (openBraceCount=1) then begin
                           // do nothing
                        end
                        else begin
                            Inc(partCnts[openBraceCount-1]);
                        end;

                        if bsr[i+1]='(' then begin
                            // Create a body part for this multipart, add it to the list of parts
                            // and store the ID in the list on the stack. Will be used when
                            // all children have been processed and we get to the multipart subtype
                            thisBodyPart:=TBodyMimePart.Create;
                            thisBodyPart.mID:=GetBodyPartID;
                            thisBodyPart.mType:='multipart';
                            mBodyParts.Add(thisBodyPart);
                            mMultipartStack.Push(mBodyParts.Count-1);
                        end;
                    end;

                    // Lookahead for a ". Don't increment i.
                    if bsr[i+1]='"' then state := STATE_PART_STARTING;
                end
                else if bsr[i]=')' then begin
                    if openBraceCount=0 then state:=STATE_DONE
                    // we are exiting a level (e.g. from depth 3 to depth 2). clear last positive...
                    else begin
                        if mRFC822Stack.IsEmpty then ClearLastPositivePartID;
                    end;
                end;
            STATE_PART_STARTING: begin
                    thisPartOpenBraceCount:=openBraceCount;
                    thisBodyPart := TBodyMimePart.Create;

                    // Form the part ID of the starting body part
                    thisBodyPart.mID := GetBodyPartID;

                    if bsr[i]='"' then state := STATE_TYPE_STARTED;
                end;
            STATE_TYPE_STARTED:
                if bsr[i]<>'"' then token:=token+bsr[i]
                else begin
                    // Store type, clear token
                    thisBodyPart.mType := token;
                    token := '';
                    Inc(state);
                end;
            STATE_TYPE_ENDED:
                if bsr[i]='"' then state := STATE_SUBTYPE_STARTED;
            STATE_SUBTYPE_STARTED:
                if bsr[i]<>'"' then token:=token+bsr[i]
                else begin
                    // Store sybtype, clear token
                    thisBodyPart.mSubType := token;
                    token := '';
                    Inc(state);
                end;
            STATE_SUBTYPE_ENDED:
                if bsr[i]='(' then state := PARAMS_STARTED
                else if Copy(bsr,i,3)='NIL' then begin
                    // Jump to next field, param list will stay empty
                    state:=PARAMS_ENDED;
                end;

            PARAMS_STARTED:
                if bsr[i]='"' then state:=PARAM_STARTED;
            PARAM_STARTED:
                if bsr[i]<>'"' then token:=token+bsr[i]
                else begin
                    // Store param name or value... (add to list of strings)
                    thisBodyPart.mParameterList.Add(token);
                    token:='';
                    state:=PARAM_ENDED;
                end;
            PARAM_ENDED:
                if bsr[i]='"' then state:=PARAM_STARTED
                else if bsr[i]=')' then state:=PARAMS_ENDED;
            PARAMS_ENDED:
                if bsr[i]='"' then state := BODY_ID_STARTED
                else if Copy(bsr,i,3)='NIL' then begin
                    // Leave NIL ('') as body id
                    state:=BODY_ID_ENDED;
                end;
            BODY_ID_STARTED:
                if bsr[i]<>'"' then token := token + bsr[i]
                else begin
                    // Store body id
                    thisBodyPart.mBodyId:=token;
                    token:='';
                    Inc(state);
                end;
            BODY_ID_ENDED:
                if bsr[i]='"' then state := DESCRIPTION_STARTED
                else if Copy(bsr,i,3)='NIL' then begin
                    // Leave NIL as description
                    state:=DESCRIPTION_ENDED;
                end;
            DESCRIPTION_STARTED:
                if bsr[i]<>'"' then token := token + bsr[i]
                else begin
                    // Store description
                    thisBodyPart.mDescription:=token;
                    token:='';
                    Inc(state);
                end;
            DESCRIPTION_ENDED:
                if bsr[i]='"' then state := ENCODING_STARTED
                else if Copy(bsr,i,3)='NIL' then begin
                    // Leave NIL as encoding
                    state:=ENCODING_ENDED;
                end;
            ENCODING_STARTED:
                if bsr[i]<>'"' then token := token + bsr[i]
                else begin
                    // Store encoding
                    thisBodyPart.mEncoding:=token;
                    token:='';
                    Inc(state);
                end;
            ENCODING_ENDED:
                if isDigit(bsr[i+1]) then state:=SIZE_STARTED;
            SIZE_STARTED: begin
                    if isDigit(bsr[i]) then token:=token+bsr[i]
                    else begin
                        // Store size (StrToInt(token))
                        thisBodyPart.mSize:=StrToInt(token);
                        token:='';
                        Inc(state);

                        // Manage various specific cases
                        if (LowerCase(thisBodyPart.mType)='message') and (LowerCase(thisBodyPart.mSubtype)='rfc822') then begin
                            // RFC Envelope follows
                            state:=RFC_ENVELOPE;
                        end
                        else if bsr[i+1]='(' then begin
                            // Immediate start of next part on the same level
                            mBodyParts.Add(thisBodyPart);
                            state := STATE_START;
                        end
                        else if (bsr[i]=')') then begin
                            mBodyParts.Add(thisBodyPart);
                            if (bsr[i+1]=' ') and (bsr[i+2]='"') and (not mMultipartStack.IsEmpty) then begin
                                i:=i+2;
                                state:=MULTIPART_DESC_STARTED;
                            end
                            else state := STATE_START;
                        end
                    end;
                end;
            SIZE_ENDED: begin
                    // We are basically ignoring everything except brackets
                    // When ")" comes we check if openBraceCount is < thisPartOpenBraceCount
                    // If it is then we have reached the end of this body part

                    // Need to check if there is anything on the mRFC822Stack. If there is, increment openBraceCount
                    if (not mRFC822Stack.IsEmpty) and (openBraceCount=mRFC822Stack.Peek-1) then begin
                        mRFC822Stack.Pop;
                        Inc(openBraceCount);
                    end;

                    if bsr[i]=')' then begin
                        // devLog.Trace(inttostr(openBraceCount)+' - '+inttostr(thisPartOpenBraceCount));
                        if openBraceCount<thisPartOpenBraceCount then begin
                            // Store this part
                            mBodyParts.Add(thisBodyPart);
                            // state = PART_STATE_DONE; this state equivalent to STATE_START
                            if (bsr[i+1]=' ') and (bsr[i+2]='"') and (not mMultipartStack.IsEmpty) then begin
                                state:=MULTIPART_DESC_STARTED;
                                i:=i+2;
                            end
                            else state := STATE_START;
                        end;
                    end;
                end;
            MULTIPART_DESC_STARTED: begin
                    if bsr[i]<>'"' then token := token + bsr[i]
                    else begin

                        tmp:=mMultipartStack.Pop;
                        // devLog.Trace('FOUND - '+token+', id in list: '+inttostr(tmp));
                        TBodyMimePart(mBodyParts.Items[tmp]).mSubtype:=token;
                        token:='';
                        state:=STATE_START;
                        //@warn I'm skipping the MULTIPART_DESC_ENDED state here. Might need it...
                    end;
                end;
            RFC_ENVELOPE: begin
                    // Not processing currently, just skip it all...
                    if bsr[i]='(' then begin // just make sure the next char is '('
                        envOpenBrace:=1; envCloseBrace:=0;
                        repeat
                            if bsr[i+1]='(' then Inc(envOpenBrace)
                            else if bsr[i+1]=')' then Dec(envOpenBrace);
                            Inc(i);
                        until envOpenBrace=0;
                        Dec(openBraceCount);
                    end;
                    if bsr[i+1]=' ' then Inc(i);  // skip blank space if it exists
                    // Check if the message body structure is simple or complex
                    if (bsr[i+1]='(') and (bsr[i+2]<>'(') then begin
                        // Only one bracket - simple bodystructure
                        state:=RFC_BODYSTRUCTURE_SIMPLE;
                    end
                    else begin
                        // More than one brackets, complex body structure
                        state:=RFC_BODYSTRUCTURE_COMPLEX;
                    end;
                end;
            RFC_BODYSTRUCTURE_SIMPLE: begin
                    // Skip the body structure of the simple message...
                    envOpenBrace:=1; envCloseBrace:=0;
                    repeat
                        if bsr[i+1]='(' then Inc(envOpenBrace)
                        else if bsr[i+1]=')' then Dec(envOpenBrace);
                        Inc(i);
                    until envOpenBrace=0;
                    Dec(openBraceCount);
                    while bsr[i+1]<>')' do begin Inc(i); end;
                    // Store this part
                    mBodyParts.Add(thisBodyPart);
                    state:=STATE_START;
                end;
            RFC_BODYSTRUCTURE_COMPLEX: begin
                    mBodyParts.Add(thisBodyPart);
                    // We need to temporarily decrement the open brace count while in a complex body structure
                    // This is because an open brace is "skipped" in this case (the one that starts the rfc822s' body structure
                    mRFC822Stack.Push(thisPartOpenBraceCount);
                    Dec(openBraceCount);
                    state:=STATE_START;
                end;
            else begin
                devLog.Error('Unknown body parser state: '+IntToStr(state));
                state := STATE_ERROR;
            end;
        end;
        Inc(i);
    end;
    if state = STATE_START then state:=STATE_DONE;
    if (state <> STATE_DONE) then devLog.Error('Error parsing BODY response at state: '+IntToStr(state))
    else DumpContents;
    Result:=(state = STATE_DONE);
end;

{ Retrieves information about the body part that should make up the
  message after the deletion of the attachment }
function TBodyStructure.GetStrippedMessageInfo: TStrippedMessageInfo;
var smInfo: TStrippedMessageInfo; found: Boolean; i: Integer;
begin
    // Set default values
    smInfo.mBodyPartID := '-1';   // Default if a text part is not found
    smInfo.mContentType:='text/plain';
    smInfo.mCharset:='us-ascii';
    smInfo.mEncoding:='7bit';

    // Find the first body part in the list of type 'text'
    found:=false;
    //devLog.Trace('Number of body parts: '+inttostr(mbodyparts.count));
    for i:=0 to mBodyParts.Count-1 do begin
        if (LowerCase(TBodyMimePart(mBodyParts[i]).mType)='text') then begin
            found:=true;
            break;
        end;
    end;

    if found then begin
        smInfo.mBodyPartID := TBodyMimePart(mBodyParts[i]).mID;
        smInfo.mContentType := TBodyMimePart(mBodyParts[i]).mType+'/'+TBodyMimePart(mBodyParts[i]).mSubtype;
        if (TBodyMimePart(mBodyParts[i]).mParameterList.Count>=2) then
            if LowerCase(TBodyMimePart(mBodyParts[i]).mParameterList[0])='charset' then
                smInfo.mCharset := TBodyMimePart(mBodyParts[i]).mParameterList[1];
        smInfo.mEncoding := TBodyMimePart(mBodyParts[i]).mEncoding;
    end;

    Result:=smInfo;
end;

procedure TBodyStructure.DumpContents;
var i: Integer;
begin
    devLog.Trace('========= START BodyStructure contents dump =========');
    devLog.Trace('Parsed string: '+mBodyStructureString);
    devLog.Trace('-- Body parts:');
    for i:=0 to mBodyParts.Count-1 do begin
        devLog.Trace(TBodyMimePart(mBodyParts[i]).ToString);
    end;
    devLog.Trace('========= END   BodyStructure contents dump =========');
end;


{ ====== TBodyPartForDisplay related ====== }

{ Populates the mBodyPartsForDisplay List based on values in
  mBodyParts. This structure is used for displaying body part
  information in the MsgPeeker. Records in this list will also
  be filled with hierarchy information (number of children, parents
  index, etc...
  @param hierarchy is true if the tree should be displayed in advanced mode.
  This is a persisted setting which can be modified in the MessagePeeker.
  Otherwise a plain list is being used...
}
procedure TBodyStructure.PopulateBodyPartsForDisplay(hierarchy: Boolean);
var viewCnt, i: Integer; alterLevel: Integer;
begin
    SetLength(mBodyPartsForDisplay,0);  // Clear list
    SetLength(mBodyPartsForDisplay,mBodyParts.Count);

    mPrimaryTextPart:=CalculatePrimaryTextPart;  // Set the primary body part
    mPrimaryHtmlPart:=CalculatePrimaryHtmlPart;

    if hierarchy then begin
        // advanced view
        for i:=0 to mBodyParts.Count-1 do begin
            mBodyPartsForDisplay[i].mIsPart := (LowerCase(TBodyMimePart(mBodyParts[i]).mType) <> 'multipart');
            mBodyPartsForDisplay[i].mID := TBodyMimePart(mBodyParts[i]).mID;
            mBodyPartsForDisplay[i].mType := LowerCase(TBodyMimePart(mBodyParts[i]).mType);
            mBodyPartsForDisplay[i].mSubtype := LowerCase(TBodyMimePart(mBodyParts[i]).mSubtype);
            mBodyPartsForDisplay[i].mEncoding := TBodyMimePart(mBodyParts[i]).mEncoding;
            //mBodyPartsForDisplay[i].mChecked := false;
            if (mBodyPartsForDisplay[i].mIsPart) then begin
                mBodyPartsForDisplay[i].mName:=GetName(TBodyMimePart(mBodyParts[i]),false);
                mBodyPartsForDisplay[i].mSize:=TBodyMimePart(mBodyParts[i]).mSize;
            end
            else begin
                mBodyPartsForDisplay[i].mName:=LowerCase(TBodyMimePart(mBodyParts[i]).mType)+'/'+LowerCase(TBodyMimePart(mBodyParts[i]).mSubtype);
                mBodyPartsForDisplay[i].mSize:=0;
            end;
            mBodyPartsForDisplay[i].mParentID:=GetParentIndex(i);
            // Increment parents number of children
            if i>0 then Inc(mBodyPartsForDisplay[mBodyPartsForDisplay[i].mParentID].mNumOfChildren);
        end;
    end
    else begin
        // simple view
        viewCnt:=0;  // Counts actual nodes in the tree
        alterLevel:=-1; // alter level is >-1 when alter parts are being processed
        for i:=0 to mBodyParts.Count-1 do begin
            if (alterLevel>-1) and (GetLevel(TBodyMimePart(mBodyParts[i]).mId)<=alterLevel) then alterLevel:=-1; // we got out of the alternative part
            if (TBodyMimePart(mBodyParts[i]).mId<>mPrimaryTextPart) then begin
                if (LowerCase(TBodyMimePart(mBodyParts[i]).mType) <> 'multipart') then begin
                    if (alterLevel=-1) or ((alterLevel>-1) and (LowerCase(TBodyMimePart(mBodyParts[i]).mSubtype)<>'plain')) then begin
                        mBodyPartsForDisplay[viewCnt].mIsPart := true;
                        mBodyPartsForDisplay[viewCnt].mID := TBodyMimePart(mBodyParts[i]).mID;
                        if (alterLevel>-1) then
                            mBodyPartsForDisplay[viewCnt].mName := 'Alternative '+ TBodyMimePart(mBodyParts[i]).mSubtype
                        else
                            mBodyPartsForDisplay[viewCnt].mName := GetName(TBodyMimePart(mBodyParts[i]),false);
                        mBodyPartsForDisplay[viewCnt].mType := LowerCase(TBodyMimePart(mBodyParts[i]).mType);
                        mBodyPartsForDisplay[viewCnt].mSubtype := LowerCase(TBodyMimePart(mBodyParts[i]).mSubtype);
                        mBodyPartsForDisplay[viewCnt].mEncoding := TBodyMimePart(mBodyParts[i]).mEncoding;
                        mBodyPartsForDisplay[viewCnt].mSize:=TBodyMimePart(mBodyParts[i]).mSize;
                        //mBodyPartsForDisplay[viewCnt].mChecked := false;
                        Inc(viewCnt);
                    end;
                end
                else begin
                    if (LowerCase(TBodyMimePart(mBodyParts[i]).mSubtype) = 'alternative')
                    then alterLevel:=GetLevel(TBodyMimePart(mBodyParts[i]).mId);
                end;
            end;
        end;
        // Adjust the size to the proper length
        SetLength(mBodyPartsForDisplay,viewCnt);
    end;
end;

{ Get the name of the body part.
  If attachmentOnly is true, will return an empty string if the part is not a "real" attachment }
function TBodyStructure.GetName(bodyPart: TBodyMimePart; attachmentOnly: Boolean): String;
var name: String; j: Integer;
begin
    name:='';

    for j:=0 to bodyPart.mParameterList.Count-1 do begin
        if (j mod 2 = 0) then begin
            if LowerCase(bodyPart.mParameterList[j]) = 'name' then name := bodyPart.mParameterList[j+1];
        end;
    end;
    if name='' then begin
        // The attachment doesn't have the NAME property, get the description
        name:=bodyPart.mDescription;
    end;
    if not attachmentOnly then
        if name='' then name:=LowerCase(bodyPart.mType)+'/'+LowerCase(bodyPart.mSubtype);

    // If this is a message/rfc822 and has a description set that as the name
    // Some servers return this as the name property, some as the description...
    if (LowerCase(bodyPart.mType)='message') and (LowerCase(bodyPart.mSubtype)='rfc822') then
        if bodyPart.mDescription<>'' then name:='message: '+bodyPart.mDescription
        else name:='message: '+name;

    Result:=name;
end;

// Get the body part level based on the body part id
// 0 - level 0
// 1, 2, 3, etc - level 1
// 1.1, 1.2, 2.1, etc - level 2
// 1.1.2, etc level 3...
function TBodyStructure.GetLevel(bodyPartId: String): Integer;
var level: Integer;
begin
    level:=0;
    if bodyPartId<>'0' then
        level:=CharOcurrenceInString(bodyPartId,'.') + 1;
    Result:=level;
end;

function TBodyStructure.GetParentIndex(index: Integer): Integer;
var k: Integer; thisLevel: Integer; found: Boolean;
begin
    if index=0 then Result:=-1
    else begin
        thisLevel:=GetLevel(mBodyPartsForDisplay[index].mID);
        k:=index-1;
        found:=false;
        while not found do begin
            if GetLevel(mBodyPartsForDisplay[k].mID)<thisLevel
                then found:=true
                else k:=k-1;
        end;
        Result:=k;
    end;
end;

{ Tries to calculate the 'primary' body part. This is simplified here to be the
  first text part of the message. If it is not present, an empty string is returned.
  This value should be stored in mPrimaryTextPart }
function TBodyStructure.CalculatePrimaryTextPart: String;
var found: Boolean; i: Integer;
begin
    Result:='';
    found:=false;
    for i:=0 to mBodyParts.Count-1 do begin
        if (LowerCase(TBodyMimePart(mBodyParts[i]).mType)='text') then begin
            found:=true;
            break;
        end;
    end;
    if found then begin
        mPrimaryTextPart := TBodyMimePart(mBodyParts[i]).mID;
        Result:=mPrimaryTextPart;
        devLog.Trace('Primary text part found: '+Result);
    end
    else devLog.Trace('Primary text part not found');
end;

{ Tries to calculate the 'primary' html part. This is simplified here to be the
  first text/html part of the message. If it is not present, an empty string is returned.
  This value should be stored in mPrimaryHtmlPart }
function TBodyStructure.CalculatePrimaryHtmlPart: String;
var found: Boolean; i: Integer;
begin
    Result:='';
    found:=false;
    for i:=0 to mBodyParts.Count-1 do begin
        if (LowerCase(TBodyMimePart(mBodyParts[i]).mType)='text') then begin
            if (LowerCase(TBodyMimePart(mBodyParts[i]).mSubType)='html') then begin
                found:=true;
                break;
            end;
        end;
    end;
    if found then begin
        Result := TBodyMimePart(mBodyParts[i]).mID;
        devLog.Trace('Primary html part found: '+Result);
    end
    else devLog.Trace('Primary html part not found');
end;

{ Returns the body part which was determined to be the 'primary text' one }
function TBodyStructure.GetPrimaryTextPart: String;
begin
    Result:=mPrimaryTextPart;
end;

function TBodyStructure.HasPrimaryTextPart: Boolean;
begin
    Result:=(mPrimaryTextPart<>'');
end;

{ Returns the body part which was determined to be the 'primary html' one
  Returns an empty string if no part was found }
function TBodyStructure.GetPrimaryHtmlPart: String;
begin
    Result:=mPrimaryHtmlPart;
end;

function TBodyStructure.HasPrimaryHtmlPart: Boolean;
begin
    Result:=(mPrimaryHtmlPart<>'');
end;

function TBodyStructure.GetBodyMimePart(id: String): TBodyMimePart;
var i: Integer; found: Boolean;
begin
    found:=false; i:=0;
    while (not found) and (i<mBodyParts.Count) do begin
        if TBodyMimePart(mBodyParts.Items[i]).mID=id then found:=true
        else Inc(i);
    end;
    if found then Result:=mBodyParts[i]
    else begin
        devLog.Error('TBodyStructure.GetBodyMimePart - unable to find body part');
        raise Exception.Create('Internal Error - TBodyStructure.GetBodyMimePart - unable to find body part');
    end;
end;

{ Returns a simplified (flat) list of attachments. Doesn't recurse into attachments! }
procedure TBodyStructure.GetAttachmentList(var list: TStringList);
var i: Integer; a: String;
begin
    list.Clear;
    for i:=0 to mBodyParts.Count-1 do begin
        a:=GetName(TBodyMimePart(mBodyParts[i]),true);
        if a<>'' then list.Add(a);
    end;
end;

{ ----------- TBodyMimePart ---------------}

constructor TBodyMimePart.Create;
begin
    // Set all parameters to empty (NIL) values.
    // these will be used if they are not defined in the body response
    mID:='';
    mType:='';
    mSubtype:='';
    mParameterList := TStringList.Create;
    mBodyId:='';
    mDescription:='';
    mEncoding:='';
    mSize:=0;
end;

function TBodyMimePart.ToString: String;
var s, pls: String; i: Integer;
begin
    pls:='(';
    for i:=0 to mParameterList.Count-1 do begin
        pls := pls+mParameterList[i];
        if (i mod 2 = 0) then pls:=pls+'--'
        else pls:=pls+' ';
    end;
    pls := pls+')';
    s := Format('ID=%s, Type=%s, Subtype=%s, Parameters: (%s), BodyID=%s, Description=%s, Encoding=%s, Size=%d',
                [mId,mType,mSubtype,pls,mBodyId,mDescription,mEncoding,mSize]);
    Result:=s;
end;

destructor TBodyMimePart.Destroy;
begin
    mParameterList.Free;
end;



{ ----------- GLOBAL FUNCTIONS ------------}

function IsDigit(c: char): Boolean;
begin
    Result := ((ord(c)>=48) and (ord(c)<=57));
end;






end.