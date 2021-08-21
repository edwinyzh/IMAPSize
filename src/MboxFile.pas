{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: MboxFile.pas 31 2006-03-11 21:43:09Z Ivan $
|==============================================================================|
| Copyright 2003-2006 Ivan Vecanski                                            |
| =============================================================================}


unit MboxFile;

interface

uses CommonTypes, MyUtil, classes, sysutils;

type


TMboxFile = class
    mFilename: String;
    mFile: File;
    mLineEnding: String;  // CR, LF or CRLF
  public
    constructor Create(filename: String);
    destructor Destroy; override;
    procedure GetMboxMsgInfos(var mboxMsgInfos: TListOfMboxMessageInfo);
end;

implementation

constructor TMboxFile.Create(filename: String);
begin
    mFilename:=filename;
end;

destructor TMboxFile.Destroy;
begin

end;

// Forms an array of
procedure TMboxFile.GetMboxMsgInfos(var mboxMsgInfos: TListOfMboxMessageInfo);
const BLOCK_SIZE = 1024;
var
    charCnt,mboxSize: Int64;
    i,msgNumber : Integer;
    prevCharCR, foundFrom, firstLine, done: boolean;
    line, statusBarMsg: String;
    F : File; ch : char;
    byteArray : array[1..BLOCK_SIZE] of byte;
    byteCount: Integer;

    {mboxFile StrList, oneMail: TStringList; i,e,partResultCnt,

    progBarLinesStep: Integer;
    firstLine: boolean;
    foundFrom: boolean;
    warn: boolean;
    progressBarValue: Real;
    progressBarIncrement: Real;
    mboxFile, emlFolder: String;
    F : File; ch : char;
    statusBarMsg: String;

    oneByte   : byte;
    byteCount  : Integer;
    done: Boolean;
    }

begin
{
    try

            // Get file size
            mboxSize:=GetFileSize(mFilename);
            // progBarLinesStep := Round(mboxSize/100);

            // Initialize counters, etc
            charCnt:=0;
            msgNumber:=0;
            prevCharCR:=false;
            line:='';
            foundFrom:=false;
            statusBarMsg:='Processing file: '+mFilename;

            if mboxSize=0 then begin
                // requestMgr.LogError(thread.ThreadInfo.RequestID,'mbox file is empty: '+mFilename);
            end
            else begin

                AssignFile(f, mFilename);
                FileMode := fmOpenRead;
                Reset(f, 1);  // define one record as 1 byte


                try

                    //PostMessage(requestMgr.Handle, WM_MAIN_PROGRESS_BAR_VISIBILITY,thread.ThreadInfo.requestID, 1);
                    //progressBarValue:=options.progressStart;
                    //progressBarIncrement := (options.progressEnd-options.progressStart)/(mboxSize/progBarLinesStep);
                    //PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, thread.ThreadInfo.requestID, Trunc(progressBarValue));

                    //UpdateStatusBar(statusBarMsg+' - Message: '+IntToStr(msgNumber+1),STATUSBAR_MSGS);
                    firstLine:=true;
                    done:=false;
                    repeat
                        // if not thread.ThreadInfo.WasTerminated then begin  @TODO threaded!!!
                            BlockRead(f, byteArray, BLOCK_SIZE, byteCount); // byteCount stores actual bytes read

                            for i:=1 to byteCount do begin
                                ch:=Chr(byteArray[i]);
                                Inc(charCnt);
                                if not (ch = #26) then begin    // 0x1A = 26 is the EOF character, will skip
                                    if (not ((ch = #13) or (ch = #10))) then line:=line+ch;
                                    if (not (prevCharCR and (ch=#10))) then begin
                                        if (ch = #13) or (ch = #10) then begin
                                            // End of line, if char is CR, make note of it
                                            prevCharCR:=(ch=#13);
                                            // Process line
                                            if Copy(line,1,5)='From ' then begin
                                                if foundFrom then begin
                                                    // Contents of oneMail represent a message (located before this from line). Send
                                                    try
                                                        SaveAsEML(oneMail,emlFolder,options,msgNumber);
                                                    except
                                                        on E:EFCreateError do
                                                            requestMgr.LogError(thread.ThreadInfo.RequestID,E.Message + ' Please check if you have permissions to write to '+emlFolder);
                                                    end;
                                                    UpdateStatusBar(statusBarMsg+' - Message: '+IntToStr(msgNumber+1),STATUSBAR_MSGS);
                                                    Inc(msgNumber);
                                                end;
                                                foundFrom:=true;
                                            end
                                            else begin
                                                // Not a from line, just add to oneMail (if foundFrom - if a mail message has started)
                                                if firstLine then begin
                                                    warn:=true;
                                                    requestMgr.LogWarning(thread.ThreadInfo.RequestID,'File '+mFilename+' does not seem to be a valid mbox file!');
                                                end;
                                                oneMail.add(line);
                                            end;
                                            line:='';
                                            if firstLine then firstLine:=false;
                                        end
                                    end;
                                    // else don't process LF as new line for CRLF (already processed for CR)
                                    if options.exportProgress then
                                        if (charCnt mod progBarLinesStep = 0) then begin
                                            progressBarValue:=progressBarValue+progressBarIncrement;
                                            PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, thread.ThreadInfo.requestID, Trunc(progressBarValue));
                                        end;
                                end
                                else begin
                                    requestMgr.LogWarning(thread.ThreadInfo.RequestID,'Skipped invalid EOF character (0x1A) at position '+IntToStr(charCnt)+' in '+mFilename);
                                end;
                            end;
                            // If we've reached the end of the file there will be less bytes read than expected
                            if byteCount<BLOCK_SIZE then done:=true;
                        end
                        // else break; // terminated
                    until done;
                except
                    devLog.Warn('Failed to open mbox file. Probable cause: Sharing violation');
                    requestMgr.LogError(thread.ThreadInfo.RequestID,'Failed to convert mbox file: '+mFilename+'. Make sure the file is not locked by another application.');
                end;
                CloseFile(f);

                if foundFrom then begin
                    try
                        SaveAsEML(oneMail,emlFolder,options,msgNumber);
                    except
                        on E:EFCreateError do
                            requestMgr.LogError(thread.ThreadInfo.RequestID,E.Message + ' Please check if you have permissions to write to '+emlFolder);
                    end;
                    Inc(msgNumber);
                end;
                if options.exportProgress and not thread.ThreadInfo.WasTerminated then
                    PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, thread.ThreadInfo.requestID, options.progressEnd);
                Inc(partResultCnt);
                requestMgr.Log(thread.ThreadInfo.RequestID,'Converted mbox file: '+mFilename+' into ' + IntToStr(msgNumber) + ' emls in '+emlFolder);
            end;
        end;
        if (partResultCnt = mboxFiles.Count) then begin
            if warn then Result:=1
            else Result:=0;
        end
        else
            Result:=2; // Errors
    finally
        UpdateStatusBar('',STATUSBAR_MSGS);
        PostMessage(requestMgr.Handle, WM_MAIN_PROGRESS_BAR_VISIBILITY,thread.ThreadInfo.requestID, 0);
        oneMail.Free;
    end;
    }

end;

end.
