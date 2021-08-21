{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id$
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

{ Unit used for performing the quick & smart attachment deletion.
  The body structure has to already be retreived and parsed.
  Input: header, the main (text) body part and the TStrippedMessageInfo (see BodyStructure.pas)
  Output: whole message without attachments.
  Basically, just modifies the header to comply to the }

unit AttachmentStripper;

interface

uses BodyStructure, classes, sysutils, VecLog, Log, SynaUtil, GlobalConstants;

const
    NO_TEXT_MSG=CRLF+'IMAPSize: This message only contained attachments.'+CRLF+'There is no text left after the attachment deletion';

type
    TAttachmentStripper = class
        mHeader: TStringList;
        mBody: TStringList;
        mStrippedInfo: TStrippedMessageInfo;
        mStrippedHeader: TStringList;
        mStrippedMessage: TStringList;
        mHeaderModified: Boolean;
        procedure FormStrippedHeader(hdr: TStrings; strippedInfo: TStrippedMessageInfo);
        procedure FormStrippedMessage;
        procedure InsertDeletedAttachmentInfo(var bs: TBodyStructure);
    public
        constructor Create;
        destructor Destroy; override;
        procedure StripMessage(header, body: TStrings; var bs: TBodyStructure; headerModified: Boolean);
        procedure GetStrippedMessage(var StrippedMessage: TStrings);
    end;

implementation

uses Main;

constructor TAttachmentStripper.Create;
begin
    mHeader:=TStringList.Create;
    mBody:=TStringList.Create;
    mStrippedHeader:=TStringList.Create;
    mStrippedMessage:=TStringList.Create;
end;

destructor TAttachmentStripper.Destroy;
begin
    mHeader.Free;
    mBody.Free;
    mStrippedHeader.Free;
    mStrippedMessage.Free;
end;

{ main method }
procedure TAttachmentStripper.StripMessage(header, body: TStrings; var bs: TBodyStructure; headerModified: Boolean);
var i: Integer; var strippedInfo: TStrippedMessageInfo;
begin
    strippedInfo:= bs.GetStrippedMessageInfo;

    // Set input values
    for i:=0 to header.Count-1 do mHeader.Add(header[i]);
    for i:=0 to body.Count-1 do mBody.Add(body[i]);
    mStrippedInfo:=strippedInfo;
    mHeaderModified:=headerModified;

    // Clear working lists
    mStrippedHeader.Clear;
    mStrippedMessage.Clear;

    // Form the header of the stripped message
    FormStrippedHeader(mHeader, mStrippedInfo);

    // If the main body part is empty, add a note to the body
    if mBody.Count=0 then mBody.Add(NO_TEXT_MSG);

    if settings.IMAPOpSettings.MsgDelAddFilenamesToBody then begin
        InsertDeletedAttachmentInfo(bs);
    end;

    // Form the stripped message
    FormStrippedMessage;
end;

{ This old method is used only on quick&smart deletion }
procedure TAttachmentStripper.FormStrippedHeader(hdr: TStrings; strippedInfo: TStrippedMessageInfo);
var i,p1,p2: Integer; dateStr: String;
begin
    devLog.Trace('Modifying header for new message');

    //Add the IMAPSize header field to the top of the header
    mStrippedHeader.Add(RFC822_HEADER_IMAPSIZE+'Attachment(s) deleted on '+Rfc822DateTime(Now));
    if mHeaderModified then mStrippedHeader.Add(RFC822_HEADER_IMAPSIZE+'Message header manually modified on '+Rfc822DateTime(Now));

    // Add content type and transfer encoding
    mStrippedHeader.Add('Content-Type: '+strippedInfo.mContentType+'; charset="'+strippedInfo.mCharset+'"');
    mStrippedHeader.Add('Content-Transfer-Encoding: '+strippedInfo.mEncoding);

    i:=0;
    while i<hdr.Count do begin
        // Check if the line is Content-Type. If it is, we have to change it to the new one
        p1:=Pos('content-type',LowerCase(mHeader[i]));
        p2:=Pos('content-transfer-encoding:',LowerCase(mHeader[i]));
        if ((p1<>1) and (p2<>1)) then begin
            mStrippedHeader.Add(hdr[i]);
        end
        else if p1>0 then begin
            // Content-Type line
            // Some multipart and other non-text/plain messages contain the boundary on the next line. Remove.
            if Pos('boundary',LowerCase(hdr[i+1])) > 0 then begin
                devLog.Debug('Removing boundary line from header');    //@todo ovde malo vise provera, moze i negde drugde da bude boundary
                i:=i+1;
            end;
        end
        else if p2>0 then begin
            // Content-Transfer-Encoding line
        end;
        i:=i+1;
    end;
    devLog.Trace('Header created');
end;

{ Will populate mAppendMsg with stripeed header/body }
procedure TAttachmentStripper.FormStrippedMessage;
var i: Integer;
begin
    mStrippedMessage.Clear;

    // Copy stripped header and body to mAppendMsg
    for i:=0 to mStrippedHeader.Count-1 do begin
        mStrippedMessage.Add(mStrippedHeader[i]);
    end;
    for i:=0 to mBody.Count-1 do begin
        mStrippedMessage.Add(mBody[i]);
    end;
end;

{ Get the resulting message. The StringList has to be initialized (and freed) by the caller }
procedure TAttachmentStripper.GetStrippedMessage(var StrippedMessage: TStrings);
var i: Integer;
begin
    StrippedMessage.Clear;
    for i:=0 to mStrippedMessage.Count-1 do StrippedMessage.Add(mStrippedMessage[i]);
end;

{ Inserts information about the deleted files to the begining of the body }
procedure TAttachmentStripper.InsertDeletedAttachmentInfo(var bs: TBodyStructure);
var list: TStringList; i: Integer;
begin
    list:=TStringList.Create;
    try
        bs.GetAttachmentList(list);
        if list.Count>0 then begin
            // Insert from last to first, makes it simpler
            mBody.Insert(0,'----'+CRLF);
            for i:=list.Count-1 downto 0 do begin
                mBody.Insert(0,'    '+list[i]);
            end;
            mBody.Insert(0,'----'+CRLF+'IMAPSize: Following attachment(s) have been deleted on '+Rfc822DateTime(Now)+':');
        end;
    finally
        list.Free;
    end;
end;


end.
