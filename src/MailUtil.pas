{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id$
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

{ Unit with various helper functions related to messages }

unit MailUtil;

interface

uses classes, sysutils, veclog;

type TAttachmentType = (attTypeText, attTypeBinary);

// Meta Information about an attachment, needed for saving it
TAttachmentSavingInfo = class
public
  fullMailboxName: String;
  msgUID: String;
  bodyPartID: String;
  encoding: String;
  partMimeType: String;
  partMimeSubtype: String;
  filename: String;         // Base filename where the attachment will be saved. Subject to change if needed.
  size: Integer;
end;

procedure GetEMLFilesFromFolder(var emlFiles: TStringList; emlFolder: String);
procedure GetMessageIdsFromEMLFiles(var messageIds: TStringList; emlFolder: String);
procedure GetMessageIdsFromMbox(var messageIds: TStringList; mboxFile: String);

function DecodePart(var encoded, decoded: TStringList; encoding, mimeType, mimeSubtype: String): TAttachmentType;
procedure DecodeTextPart(var encoded, decoded: TStringList; encoding: String);
procedure DecodeBinaryPart(var encoded: TStringList; var decoded: String; encoding: String);

implementation

uses synacode;

{ Extracts filenames (full path) of eml files from the specified folder }
procedure GetEMLFilesFromFolder(var emlFiles: TStringList; emlFolder: String);
var SearchRec: TSearchRec;
begin
    if FindFirst(emlFolder+'\*.eml', faAnyFile and not (faVolumeID or faDirectory), SearchRec) = 0 then begin
        repeat
            emlFiles.Add(emlFolder+'\'+SearchRec.Name);
        until FindNext(SearchRec) <> 0;
        FindClose(SearchRec);
    end;
end;

{ Will retrieve message ids of all eml files found in the specified folder.
  Calling method should create and destroy the messageIds list }
procedure GetMessageIdsFromEMLFiles(var messageIds: TStringList; emlFolder: String);
var files, msgContents: TStringList; i,j: Integer; found: Boolean;
    openPos, closePos: Integer;
begin
    // Get eml files from the folder
    files:=TStringList.Create;
    msgContents:=TStringList.Create;
    try
        GetEMLFilesFromFolder(files,emlFolder);
        // Now get the message id of each file
        for i:=0 to files.Count-1 do begin
            msgContents.Clear;
            msgContents.LoadFromFile(files[i]);
            found:=false; j:=0;
            while (not found) and (j<msgContents.Count) do begin
                if Length(msgContents[j])>10 then
                    if Copy(LowerCase(msgContents[j]),1,10)='message-id' then begin
                        openPos:=Pos('<',msgContents[j]);
                        closePos:=Pos('>',msgContents[j]);
                        if (openPos>0) and (closePos>0) then begin
                            messageIds.Add(Copy(msgContents[j],openPos+1,closePos-openPos-1));
                            found:=true;
                        end
                        else break;  // If the message-id is not formatted ok, skip the message (could lead to errors otherwise (e.g. nested messages))
                    end;
                Inc(j);
            end;
        end;
    finally
        msgContents.Free;
        files.Free;
    end;
end;

{ Will retrieve message ids of all messages in the mbox file.
  @todo need to use file streams for better performance.
  Calling method should create and destroy the messageIds list }
procedure GetMessageIdsFromMbox(var messageIds: TStringList; mboxFile: String);
var mboxContents: TStringList; lineCnt, openPos, closePos: Integer; foundMsgId: Boolean;

    { Moves lineCnt to the beginning of the next message.
      Returns false if not found }
    function FindNextMessageStart: Boolean;
    begin
        Result:=true;
        if lineCnt<mboxContents.Count-1 then begin
            while (Copy(mboxContents[lineCnt],1,5)<>'From ') do begin
                Inc(lineCnt);
                if lineCnt>=mboxContents.Count then begin
                    Result:=false;
                    break;
                end;
            end;
        end
        else Result:=false;
    end;

begin
    mboxContents:=TStringList.Create;
    try
        if FileExists(mboxFile) then begin
            mboxContents.LoadFromFile(mboxFile);
            // Find the first message-id in each message (should ignore message-ids of nested messages)
            lineCnt:=0;
            while FindNextMessageStart do begin
                foundMsgId:=false;
                while (not foundMsgId) and (lineCnt<mboxContents.Count) do begin
                    if Length(mboxContents[lineCnt])>10 then
                        if Copy(LowerCase(mboxContents[lineCnt]),1,10)='message-id' then begin
                            openPos:=Pos('<',mboxContents[lineCnt]);
                            closePos:=Pos('>',mboxContents[lineCnt]);
                            if (openPos>0) and (closePos>0) then begin
                                messageIds.Add(Copy(mboxContents[lineCnt],openPos+1,closePos-openPos-1));
                                foundMsgId:=true;
                            end
                            else break;  // If the message-id is not formatted ok, skip the message (could lead to errors otherwise (e.g. nested messages))
                        end;
                    Inc(lineCnt);
                end;
            end;
        end;
    finally
        mboxContents.Free;
    end;
end;


{ Will determine the attachment type based on the mimetype/subtype and the encoding
  and based on it will decode either as text or binary. In case of a binary decode,
  the result is stored in the first string of the 'decoded' string list.
  @return the attachment type }
function DecodePart(var encoded, decoded: TStringList; encoding, mimeType, mimeSubtype: String): TAttachmentType;
var attType: TAttachmentType; dc: String;
begin
    if encoding='' then attType:=attTypeText
    // Following is a hack (possibly real solution) for situations where mime is application/octet-stream but
    // the encoding is quoted-printable (such as *jsp, *cpp, etc sent by some mailers)
    // Here we assume that if encoding is quoted-printable, the content is text.
    else if LowerCase(encoding)='quoted-printable' then attType:=attTypeText
    else begin
        if (mimeType='text') or (mimeSubtype='text') or (mimeSubtype='rtf') or (mimeType='message') then attType:=attTypeText
        else attType:=attTypeBinary;
    end;
    if attType=attTypeBinary then begin
        dc:='';
        DecodeBinaryPart(encoded,dc,encoding);
        decoded.Add(dc);
    end
    else begin
        DecodeTextPart(encoded,decoded,encoding);
    end;
    Result:=attType;
end;

{ Decodes a text body part }
procedure DecodeTextPart(var encoded, decoded: TStringList; encoding: String);
var i: Integer;
begin
    decoded.Clear;
    if LowerCase(encoding)='base64' then decoded.Text:=DecodeBase64(encoded.Text)
    else if LowerCase(encoding)='quoted-printable' then decoded.Text:=DecodeQuotedPrintable(encoded.Text)
    else begin
        // Encoding not recognized
        devLog.Warn('Encoding not recognized/supported: '+encoding);
        //dbg('*** Encoded length: '+IntToStr(encoded.Count));
        for i:=0 to encoded.Count-1 do decoded.Add(encoded[i]);
    end;
end;

{ Decodes a binary body part. Differs from DecodeTextPart in that the text passed
  for decoding is free from CRLFs }
procedure DecodeBinaryPart(var encoded: TStringList; var decoded: String; encoding: String);
var i: Integer; encodedStr: String;
begin
    decoded:=''; encodedStr:='';
    for i:=0 to encoded.Count-1 do encodedStr:=encodedStr+encoded[i];
    if LowerCase(encoding)='base64' then decoded:=DecodeBase64(encodedStr)
    else if LowerCase(encoding)='quoted-printable' then decoded:=DecodeQuotedPrintable(encodedStr)
    else if Pos('x-uu',LowerCase(encoding)) = 1 then decoded:=DecodeUU(encodedStr)
    else if Pos('x-xx',LowerCase(encoding)) = 1 then decoded:=DecodeXX(encodedStr)
    else begin
        // Encoding not recognized
        devLog.Warn('Encoding not recognized/supported: '+encoding);
        decoded:=encodedStr;
    end;
end;



end.
