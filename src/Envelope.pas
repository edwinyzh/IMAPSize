{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: Envelope.pas,v 1.6 2004/03/31 23:27:38 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit Envelope;

{ Unit that performs conversion routines for RFC 822 Envelopes }

interface

uses VecLog, GlobalConstants, mimeinln, synachar;

const
    STATE_START = 0;
    STATE_DONE = 22;

    ADDR_STATE_START = 100;
    ADDR_STATE_PADDR_STARTED = 101;
    ADDR_STATE_PADDR_ENDED = 109;
    ADDR_STATE_DONE = 110;

    ERROR_STATE = 200;

type

{ An address structure is a parenthesized list that describes an electronic mail address.
  The fields of an address structure are in the following order:
  personal name, [SMTP] at-domain-list (source route), mailbox name, and host name. }
TAddressStructure = record
    mName: String;
    mSourceRoute: String;
    mMailboxName: String;
    mHostName: String;
end;
TAddresses = array of TAddressStructure;

TEnvelope = class
    mDate: String;
    mSubject: String;
    mFrom: TAddresses;
    mSender: TAddresses;
    mReplyTo: TAddresses;
    mTo: TAddresses;
    mCc: TAddresses;
    mBcc: TAddresses;
    mInReplyTo: String;
    mMessageId: String;
    mFromCnt, mSenderCnt, mReplyToCnt, mToCnt, mCcCnt,mBccCnt: Integer;
    constructor Create;
    function ParseEnvelope(fetchEnvelopeResponse: String): boolean;
    function GetFromField: String;
    function GetToField: String;
    destructor Destroy; override;
end;

function getLiteralLength(var s: String; var i: Integer): Integer;
function getStringLiteral(var s: String; var i: Integer; literalLength: Integer): String;

implementation

uses Main, sysutils;  //@todo remove

constructor TEnvelope.Create;
begin
    mFromCnt:=0;
    mSenderCnt:=0;
    mReplyToCnt:=0;
    mToCnt:=0;
    mCcCnt:=0;
    mBccCnt:=0;
    SetLength(mFrom,mFromCnt);
    SetLength(mSender,mSenderCnt);
    SetLength(mReplyTo,mReplyToCnt);
    SetLength(mTo,mToCnt);
    SetLength(mCc,0);
    SetLength(mBcc,mBccCnt);

    // Set default values
    mDate:= '';
    mSubject:='';
    mInReplyTo:='';
    mMessageId:='';
end;

{ States:
  0 - Starting state
  1 - Found oppening brace of ENVELOPE
  2,3 - Date token started/ended
  4,5 - Subject started/ended
  6,7 - From started/ended
  8,9 - Sender started/ended
  10,11 - Reply-to started/ended
  12,13 - To started/ended
  14/15 - CC started/ended
  16/17 - BCC started/ended
  18/19 - In-reply-to started/ended
  20/21 - message-id started/ended
}
{
  Parses a response to a fetch command containing ENVELOPE. You can drop
  any fetch response if it contains the ENVELOPE part, no need for specific parsing.
  So, raw response for all of the following will work:
  FETCH x (ENVELOPE)
  FETCH x (UID ENVELOPE)
  FETCH x (UID ENVELOPE RFC822.SIZE)

  Parsed values can be obtained through published read-only properties

  @param stringType should be either STRING_FORM_QUOTED or STRING_FORM_LITERAL
         otherwise the envelope will not be parsed...
}
function TEnvelope.ParseEnvelope(fetchEnvelopeResponse: String): boolean;
var state,i: Integer;
    token: String;
    fer: String;
    literal: Boolean;
    literalLength: Integer;

    function getAddresses: Boolean;
    var addrState: Integer; // State of the address part
        address: TAddressStructure;
    begin
        addrState:=ADDR_STATE_START;
        token:='';
        literal:=false;
        // Currently just get the raw address (parenthesized)
        while ((i<Length(fer)) and (addrState<ADDR_STATE_DONE)) do begin
            // devLog.Trace('++++ i='+inttostr(i)+', char=['+fer[i]+'], state='+inttostr(addrState));
            case addrState of
               // 100 - Expecting a new parenthesized address or end of the field
               ADDR_STATE_START: begin
                        if fer[i]='(' then addrState:=ADDR_STATE_PADDR_STARTED
                        else if fer[i]=')' then addrState:=ADDR_STATE_DONE;
                    end;
               // 101 - Expecting start of one of the fields in parenthesized list
               101,103,105,107: if fer[i]='"' then begin
                        Inc(addrState);
                        literal:=false;
                    end
                    else if fer[i]='{' then begin  // found the begining of a string literal - get length
                        Inc(i);
                        literalLength := getLiteralLength(fer,i);
                        Inc(addrState);
                        literal:=true;
                    end
                    else if Copy(fer,i,3)='NIL' then begin
                        addrState:=addrState+2;
                        i:=i+2;
                    end;
               // 102 - 108 (even) One of the fields in parenthesized list started.
               102,104,106,108:
                    if (not literal) then begin
                        if (fer[i]='"') and (fer[i-1]<>BACKSLASH) then begin
                            case addrState of
                               102: address.mName:=InlineDecode(token, GetCurCP);
                               104: address.mSourceRoute:=token;
                               106: address.mMailboxName:=token;
                               108: address.mHostname:=token;
                            end;
                            token:='';
                            Inc(addrState);
                        end
                        else if fer[i-1]=BACKSLASH then
                            // skip, don't add " to token
                        else if fer[i]=BACKSLASH then
                            // skip, don't add \ to token
                        else token:=token+fer[i];
                    end
                    else begin
                        case addrState of
                           102: begin
                                    address.mName:=getStringLiteral(fer,i,literalLength);
                                    address.mName:=InlineDecode(address.mName,GetCurCP);
                                end;
                           104: address.mSourceRoute:=getStringLiteral(fer,i,literalLength);
                           106: address.mMailboxName:=getStringLiteral(fer,i,literalLength);
                           108: address.mHostname:=getStringLiteral(fer,i,literalLength);
                        end;
                        Inc(addrState);
                    end;
               // 109 - Parenthesized address ended (might start another one)
               ADDR_STATE_PADDR_ENDED: begin
                        // Add this address to the list of appropriate addresses
                        case state of
                           6: begin Inc(mFromCnt); SetLength(mFrom,mFromCnt); mFrom[mFromCnt-1]:=address; end;
                           8: begin Inc(mSenderCnt); SetLength(mSender,mSenderCnt); mSender[mSenderCnt-1]:=address; end;
                           10: begin Inc(mReplyToCnt); SetLength(mReplyTo,mReplyToCnt); mReplyTo[mReplyToCnt-1]:=address; end;
                           12: begin Inc(mToCnt); SetLength(mTo,mToCnt); mTo[mToCnt-1]:=address; end;
                           14: begin Inc(mCcCnt); SetLength(mCc,mCcCnt); mCc[mCcCnt-1]:=address; end;
                           16: begin Inc(mBccCnt); SetLength(mBcc,mBccCnt); mBcc[mBccCnt-1]:=address; end;
                        end;

                        // Clear address fields for next address
                        address.mName:='NIL';
                        address.mSourceRoute:='NIL';
                        address.mMailboxName:='NIL';
                        address.mHostname:='NIL';

                        if fer[i]=')' then addrState:=ADDR_STATE_DONE
                        else if fer[i]='(' then addrState:=ADDR_STATE_PADDR_STARTED;
                    end;

               // 110 - End of addresses field reached
               ADDR_STATE_DONE: {done} ;
            end; // case
            Inc(i);
        end; // while
        if (addrState <> ADDR_STATE_DONE) then devLog.Warn('Failed to parse address');

        Result := (addrState = ADDR_STATE_DONE);
    end;

begin
    state:=STATE_START;
    literal:=false;
    fer := fetchEnvelopeResponse;
    while ((i<=Length(fer)) and (state<STATE_DONE)) do begin
        // devLog.Trace('**** i='+inttostr(i)+', char=['+fer[i]+'], state='+inttostr(state));
        case state of
          STATE_START: if fer[i]='(' then state:=1;
          1,3,17,19: if fer[i]='"' then begin // found the begining of a quoted string
                        token:='';
                        Inc(state);
                        literal:=false;
                     end
                     else if fer[i]='{' then begin  // found the begining of a string literal - get length
                        Inc(i);
                        literalLength := getLiteralLength(fer,i);
                        Inc(state);
                        literal:=true; token:='';
                     end
                     else if Copy(fer,i,3)='NIL' then begin
                        state:=state+2;
                        i:=i+2;
                     end;
          2: if (not literal) then begin
                if fer[i]='"' then begin mDate:=token; Inc(state); end
                else token:=token+fer[i];
             end
             else begin
                mDate := getStringLiteral(fer, i, literalLength);
                mDate := InlineDecode(mDate,GetCurCP);
                Inc(state);
             end;
          4: // Subject started
             if (not literal) then begin
                // quoted string
                if fer[i]='"' then begin mSubject:=InlineDecode(token, GetCurCP); Inc(state); end
                else token:=token+fer[i];
             end
             else begin
                // string literal
                mSubject := getStringLiteral(fer, i, literalLength);
                mSubject := InlineDecode(mSubject,GetCurCP);
                Inc(state);
             end;
          5,7,9,11,13,15:  if fer[i]='(' then begin token:=''; Inc(state); end
                           else if Copy(fer,i,3)='NIL' then begin
                              state:=state+2;
                              i:=i+2;
                           end;
          6,8,10,12,14,16: begin
                               if GetAddresses then Inc(state)
                               else state:=ERROR_STATE;
                           end;
          18: if (not literal) then begin
                if fer[i]='"' then begin mInReplyTo:=token; Inc(state); end
                else token:=token+fer[i];
              end
              else begin
                mInReplyTo := getStringLiteral(fer, i, literalLength);
                mInReplyTo := InlineDecode(mInReplyTo,GetCurCP);
                Inc(state);
              end;
          20: if (not literal) then begin
                if fer[i]='"' then begin mMessageId:=token; Inc(state); end
                else token:=token+fer[i];
             end
             else begin
                mMessageId := getStringLiteral(fer, i, literalLength);
                mMessageId := InlineDecode(mMessageId,GetCurCP);
                Inc(state);
             end;
          21: if fer[i]=')' then begin
                    state:=STATE_DONE;
              end;
          22: {done} ;

        end; //case
        Inc(i);
    end;
    if (state <> STATE_DONE) then devLog.Error('ERROR PARSING ENVELOPE. State='+inttostr(state));

    Result:=(state = STATE_DONE);
end;

function TEnvelope.GetFromField: String;
var res: String;
begin
    if mFromCnt = 0 then res := ''
    else begin

        case settings.MessageDisplay.AddressStructure of
            0:  // display both the name and address (if possible)
                begin
                    if Length(mFrom[0].mName)<>0 then res:=mFrom[0].mName;
                    res:=res + ' <' + mFrom[0].mMailboxName+'@'+mFrom[0].mHostName + '>';
                end;

            1:  // display only the name (if exists)
                begin
                    if Length(mFrom[0].mName)=0 then begin
                        res:=mFrom[0].mMailboxName+'@'+mFrom[0].mHostName;
                    end
                    else begin
                        res:=mFrom[0].mName;
                    end
                end;

        else    // display only the address
                begin
                    res:=mFrom[0].mMailboxName+'@'+mFrom[0].mHostName;
                end;
        end;
    end;
    if mFromCnt > 1 then res:=res+' and others';
    Result:=res;
end;

function TEnvelope.GetToField: String;
var res: String;
begin
    if mToCnt = 0 then res := ''
    else begin
        case settings.MessageDisplay.AddressStructure of
            0:  // display both the name and address (if possible)
                begin
                    if Length(mTo[0].mName)<>0 then res:=mTo[0].mName;
                    res:=res + ' <' + mTo[0].mMailboxName+'@'+mTo[0].mHostName + '>';
                end;

            1:  // display only the name (if exists)
                begin
                    if Length(mTo[0].mName)=0 then begin
                        res:=mTo[0].mMailboxName+'@'+mTo[0].mHostName;
                    end
                    else begin
                        res:=mTo[0].mName;
                    end
                end;

        else    // display only the address
                begin
                    res:=mTo[0].mMailboxName+'@'+mTo[0].mHostName;
                end;
        end;
    end;
    if mToCnt > 1 then res:=res+' and others';
    Result:=res;
end;

destructor TEnvelope.Destroy;
begin
    SetLength(mFrom,0);
    SetLength(mSender,0);
    SetLength(mReplyTo,0);
    SetLength(mTo,0);
    SetLength(mCc,0);
    SetLength(mBcc,0);
    Inherited Destroy;
end;

{ Returns the length of a literal
  @param s the whole string that is being processed (might contain several literals)
  @param i position of the openning brace from where the count begins }
function getLiteralLength(var s: String; var i: Integer): Integer;
var literalLengthStr: String;
begin
    while s[i]<>'}' do begin literalLengthStr := literalLengthStr + s[i]; Inc(i); end;
    Result := StrToInt(literalLengthStr);
    //@todo ako ne nadje baci exception
end;

{ Extracts the string literal string from 's' starting at 'i' of length literalLength }
//@todo use Copy, faster
function getStringLiteral(var s: String; var i: Integer; literalLength: Integer): String;
var j: Integer; token: String;
begin
    j := i + literalLength;
    while (i<j) do begin
        token := token + s[i];
        Inc(i);
    end;
    Result := token;
end;

end.