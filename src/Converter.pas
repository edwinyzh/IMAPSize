{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: Converter.pas,v 1.3 2004/04/04 20:17:12 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit Converter;

interface

uses classes, sysutils, windows, messages,
     RegExpr, synautil, CustomThread, GlobalConstants,
     VecLog, Log, MyUtil;

type

TConvertOperation = (cnvOpNone, cnvOpEml2Mbox, cnvOpEml2Mboxes, cnvOpMbox2Eml);

{ Record that stores conversion options }
TConvertOptions = record
    UnixLF: Boolean;        // true if the resulting file should have unix file ending (Dos otherwise - default)
    yzOrder: Boolean;       // true if year zone order should be used in the From line (default false)
    overwrite: Boolean;     // true if an existing file should be overwritten (defaul false - append)
    exportProgress: Boolean; // true if messages should be posted about the progress
    progressStart: Integer; // starting position of the progress (usually 0)
    progressEnd: Integer;   // final position of the progress (usually 100)
end;

{ Use this class when you want the conversions to take place in YOUR thread }
TConverter = class
  private
    procedure SetFromLine(var oneMail: TStringList);
    procedure AddLineToFile(var theFile: TFileStream; theLine: String);
    procedure SaveAsEML(var oneMail: TStringList; emlFolder: String; options: TConvertOptions; msgNumber: Integer);
    procedure UpdateStatusBar(newText: String; panelIndex: Integer);
  public
    function eml2mbox(var emlFiles: TStringList; mboxFile: String; options: TConvertOptions; var thread: TCustomThread): Boolean;
    function mbox2eml(mboxFiles: TStringList; emlFolders: TStringList; options: TConvertOptions; var thread: TCustomThread): Integer;
end;

{ Use this class when you don't want to bother with your threads }
TConverterThread = class(TCustomThread)
private
    mConverter: TConverter;
    mFileList: TStringList;
    mFilename: String;
    mFolders: TStringList;
    mOptions: TConvertOptions;
    mOperation: TConvertOperation;
    mEmlDir: String;
    mMboxDir: String;
public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure Execute; override;
    procedure SetFileList(fileList: TStrings);
    procedure SetFile(filename: String);
    procedure SetFolders(folders: TStringList);
    procedure SetOptions(options: TConvertOptions);
    procedure SetOperation(op: TConvertOperation);
    procedure SetEmlDir(emlDir: String);
    procedure SetMboxDir(mboxDir: String); 
end;

function GetDefaultConversionOptions: TConvertOptions;

implementation

uses Main;

{ Converts eml2mbox. Will not touch eml files. }
{ @param thread is the thread in which this method is running. }
function TConverter.eml2mbox(var emlFiles: TStringList; mboxFile: String;
    options: TConvertOptions; var thread: TCustomThread): Boolean;
var
    oneMail: TStringList;
    theFile: TFileStream;
    i,j: Integer;
    progressBarValue: Real;
    progressBarIncrement: Real;
    lineEnding: String;
begin
    devLog.Trace('eml2mbox');
    result:=false;
    oneMail := TStringList.Create;
    try
        if options.exportProgress then begin
            PostMessage(requestMgr.Handle, WM_MAIN_PROGRESS_BAR_VISIBILITY,thread.ThreadInfo.requestID, 1);
            progressBarValue:=options.progressStart;
            if emlFiles.Count>0 then progressBarIncrement := (options.progressEnd-options.progressStart)/emlFiles.Count
            else progressBarIncrement:=options.progressEnd;
            PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, thread.ThreadInfo.requestID, Trunc(progressBarValue));
        end;

        // Prepare the mbox file
        if not FileExists(mboxFile) then begin
            theFile := TFileStream.Create(mboxFile, fmCreate);
            theFile.Destroy;
        end;
        if options.overwrite then begin
            theFile := TFileStream.Create(mboxFile, fmOpenWrite);
            theFile.Size:=0;
            theFile.Seek(0, soFromBeginning);
        end
        else begin
            theFile := TFileStream.Create(mboxFile, fmOpenReadWrite);
            theFile.Seek(0, soFromEnd);
        end;


        if options.UnixLF then lineEnding:=LF
        else lineEnding:=CRLF;

        for i:=0 to emlFiles.Count-1 do begin
            if not thread.ThreadInfo.WasTerminated then begin
                oneMail.Clear;
                try
                    oneMail.LoadFromFile(emlFiles[i]);
                    oneMail.Insert(0, 'From ');  // add fake From line to the beginning
                    SetFromLine(oneMail);
                    // oneMail now contains the message in the mbox format. Add to the file
                    for j:=0 to oneMail.Count-1 do begin
                        if Copy(oneMail[j],1,4)='From' then
                            if oneMail[j][5]<>':' then begin
                                // The From header starts with From:
                                if (j>0) then
                                    // Fake from line, prefix with '>'
                                    oneMail[j]:='>'+oneMail[j];
                            end;
                        AddLineToFile(theFile,oneMail[j]+lineEnding);
                    end;
                    AddLineToFile(theFile,lineEnding);
                    requestMgr.Log(thread.ThreadInfo.RequestID,'Successfully added to mbox: '+emlFiles[i]);
                except
                    on EFOpenError do
                        requestMgr.LogError(thread.ThreadInfo.RequestID,'ERROR: Can''t open file '+emlFiles[i]+'. Message NOT added to mbox!');
                    else
                        requestMgr.LogError(thread.ThreadInfo.RequestID,'ERROR: Unable to process '+emlFiles[i]+'. Message NOT added to mbox!');
                end;
                if options.exportProgress then begin
                    progressBarValue:=progressBarValue+progressBarIncrement;
                    PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, thread.ThreadInfo.requestID, Trunc(progressBarValue));
                end;
            end
            else begin
                break;
            end;
        end;
        if options.exportProgress and not thread.ThreadInfo.WasTerminated then
            PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, thread.ThreadInfo.requestID, options.progressEnd);
        result:=true;
    finally
        if (theFile <> nil) then theFile.Destroy();
        theFile := nil;
        oneMail.Free;
        PostMessage(requestMgr.Handle, WM_MAIN_PROGRESS_BAR_VISIBILITY,thread.ThreadInfo.requestID, 0);
    end;
end;

{ Returns:
      0: All OK
      1: Warning
      2: Errors }
function TConverter.mbox2eml(mboxFiles: TStringList; emlFolders: TStringList;
    options: TConvertOptions; var thread: TCustomThread): Integer;

const BLOCK_SIZE = 1024;
var mboxFileStrList, oneMail: TStringList; i,e,partResultCnt, msgNumber: Integer; line: String;
    charCnt, mboxSize: Int64;
    progBarLinesStep: Integer;
    firstLine: boolean;
    foundFrom: boolean;
    warn: boolean;
    progressBarValue: Real;
    progressBarIncrement: Real;
    mboxFile, emlFolder: String;
    F : File; ch : char; prevCharCR: boolean;
    statusBarMsg: String;
    byteArray : array[1..BLOCK_SIZE] of byte;
    oneByte   : byte;
    byteCount, lineCount  : Integer;
    done: Boolean;
    ordCh: Integer;
begin
    oneMail:=TStringList.Create;
    warn:=false;
    try
        Result:=0;
        partResultCnt:=0;
        for e:=0 to mboxFiles.Count-1 do begin

            mboxFile:=mboxFiles[e];
            emlFolder:=emlFolders[e];

            // Get file size
            mboxSize:=GetFileSize(mboxFile);
            progBarLinesStep := Round(mboxSize/100);

            // Initialize counters, etc
            charCnt:=0; lineCount:= 0;
            msgNumber:=0;
            prevCharCR:=false;
            line:='';
            foundFrom:=false;
            oneMail.Clear;
            statusBarMsg:='Processing file: '+mboxFile;

            if mboxSize=0 then begin
                requestMgr.LogError(thread.ThreadInfo.RequestID,'mbox file is empty: '+mboxFile);
            end
            else begin

                AssignFile(f, mboxFile);
                FileMode := fmOpenRead;
                Reset(f, 1);  // define one record as 1 byte

                try
                    if options.exportProgress then begin
                        PostMessage(requestMgr.Handle, WM_MAIN_PROGRESS_BAR_VISIBILITY,thread.ThreadInfo.requestID, 1);
                        progressBarValue:=options.progressStart;
                        progressBarIncrement := (options.progressEnd-options.progressStart)/(mboxSize/progBarLinesStep);
                        PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, thread.ThreadInfo.requestID, Trunc(progressBarValue));
                    end;

                    UpdateStatusBar(statusBarMsg+' - Message: '+IntToStr(msgNumber+1),STATUSBAR_MSGS);
                    firstLine:=true;
                    done:=false;
                    repeat
                        if not thread.ThreadInfo.WasTerminated then begin
                            BlockRead(f, byteArray, BLOCK_SIZE, byteCount); // byteCount stores actual bytes read

                            i:=1;
                            while i<=byteCount do begin

                                ch:=Chr(byteArray[i]);
                                ordCh:=Ord(ch);
                                Inc(charCnt);
                                if not (ordCh = 26) then begin    // 0x1A = 26 is the EOF character, will skip
                                    if (not ((ordCh = 13) or (ordCh = 10))) then line:=line+ch;
                                    if (ordCh = 13) or (ordCh = 10) then begin
                                        // Consume any remaining LFs
                                        if ((ordCh=13) and (i<byteCount) and (ord(chr(byteArray[i+1]))=10)) then Inc(i);
                                        Inc(lineCount);
                                        // End of line, if char is CR, make note of it
                                        prevCharCR:=(ordCh=13);
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
                                            oneMail.Clear;
                                        end
                                        else begin
                                            // Not a from line, just add to oneMail (if foundFrom - if a mail message has started)
                                            if firstLine then begin
                                                warn:=true;
                                                requestMgr.LogWarning(thread.ThreadInfo.RequestID,'File '+mboxFile+' does not seem to be a valid mbox file!');
                                            end;
                                            oneMail.add(line);
                                        end;
                                        line:='';
                                        if firstLine then firstLine:=false;
                                    end;

                                    if options.exportProgress then
                                        if (charCnt mod progBarLinesStep = 0) then begin
                                            progressBarValue:=progressBarValue+progressBarIncrement;
                                            PostMessage(requestMgr.Handle, WM_PROGRESS_BAR_UPDATE, thread.ThreadInfo.requestID, Trunc(progressBarValue));
                                        end;
                                end
                                else begin
                                    requestMgr.LogWarning(thread.ThreadInfo.RequestID,'Skipped invalid EOF character (0x1A) at position '+IntToStr(charCnt)+' in '+mboxFile);
                                end;
                                Inc(i);
                            end;
                            // If we've reached the end of the file there will be less bytes read than expected
                            if byteCount<BLOCK_SIZE then done:=true;
                        end
                        else break; // terminated
                    until done;
                except
                    devLog.Warn('Failed to open mbox file. Probable cause: Sharing violation');
                    requestMgr.LogError(thread.ThreadInfo.RequestID,'Failed to convert mbox file: '+mboxFile+'. Make sure the file is not locked by another application.');
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
                requestMgr.Log(thread.ThreadInfo.RequestID,'Converted mbox file: '+mboxFile+' into ' + IntToStr(msgNumber) + ' emls in '+emlFolder);
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

end;

{ Saves contents of the TStringList into an eml file in the
  specified folder. Name of the file is formed in the procedure }
procedure TConverter.SaveAsEML(var oneMail: TStringList; emlFolder: String; options: TConvertOptions; msgNumber: Integer);
var nowStr, subjectStr, emlFilename: String; theFile: TFileStream;
    invCharRegEx: TRegExpr; i:Integer;
begin
    invCharRegEx:=TRegExpr.Create;
    invCharRegEx.Expression:=invCharRE;
    try
        // Create file with temporary name
        subjectStr:='';
        DateTimeToString(nowStr,'yyyymmdd_hhnnss_zzz',Now);
        emlFilename:=emlFolder+'\'+IntToStr(msgNumber)+'_'+nowStr;
        theFile := TFileStream.Create(emlFilename,fmCreate);
        theFile.Seek(0, soFromBeginning);
        for i:=0 to oneMail.Count-1 do begin
            // Strip > from fake From line
            if Copy(oneMail[i],1,6)='>From ' then oneMail[i]:=Copy(oneMail[i],2,length(oneMail[i])-1);
            // Set subject
            if Copy(oneMail[i],1,9)='Subject: ' then subjectStr:=Copy(oneMail[i],10,Length(oneMail[i])-1);
            AddLineToFile(theFile,oneMail[i]+CRLF);
        end;
    finally
        if (theFile <> nil) then theFile.Destroy();
    end;
    if FileExists(emlFilename) then begin
        if subjectStr='' then subjectStr:='NoSubject';
        subjectStr:=invCharRegEx.Replace(subjectStr,'_');
        if not RenameFile(emlFilename,emlFolder+'\'+subjectStr+'_'+IntToStr(msgNumber)+'_'+nowStr+'.eml') then
            devLog.Warn('Cant rename to: '+subjectStr);
    end
    else devLog.Error('File does not exist: '+emlFilename);
    invCharRegEx.Free;
end;

{ Private method used to add a line to a file.
  @NOTE: The file stream HAS to be open!!! }
procedure TConverter.AddLineToFile(var theFile: TFileStream; theLine: String);
var buffer: PChar;
begin
    buffer := PChar(theLine);
    theFile.Write(buffer^, Length(theLine));
end;


{ Sets the from line (at index 0) based on the contents
  of the email. list should be created/destroyed in the
  calling procedure. First line needs to be reserved for
  the From line - this proc will not insert it, just modify it }
procedure TConverter.SetFromLine(var oneMail: TStringList);
var fromFound, dateFound: Boolean;
    fromStr, dateStr: String;
    i, openIndex, closeIndex: Integer;
    dt: TDateTime;
begin
    // Find from or date lines:
    i:=1;
    fromStr:='unknown@unknown.unk';
    dateStr:=DateTimeToStr(Now);
    fromFound:=false;
    dateFound:=false;
    while ((not fromFound) or (not dateFound)) and (i<oneMail.Count) do begin
        if Copy(oneMail[i],1,6)='From: ' then begin
            fromStr:=Copy(oneMail[i],7,Length(oneMail[i])-6);
            fromFound:=true;
        end
        else if Copy(oneMail[i],1,6)='Date: ' then begin
            dateStr:=Copy(oneMail[i],7,Length(oneMail[i])-6);
            dateFound:=true;
        end;
        Inc(i);
    end;

    // Standardize From to mbox format
    openIndex := Pos('<',fromStr);
    closeIndex := Pos('>',fromStr);
    if (openIndex>0) and (closeIndex>0) then
        fromStr := Copy(fromStr,openIndex+1,closeIndex-openIndex-1);

    // Standardize Date to mbox format
    //dt:=EnhStr2Date(dateStr);
    dt:=DecodeRfcDateTime(dateStr);
    DateTimeToString(dateStr,'ddd mmm dd hh:nn:ss yyyy',dt);

    oneMail[0]:='From '+fromStr+' '+dateStr;
    devLog.Trace('From line: '+oneMail[0]);
end;

procedure TConverter.UpdateStatusBar(newText: String; panelIndex: Integer);
begin
    FMain.ShowMessageOnStatusBar(newText, panelIndex);
    FMain.StatusBar1.Repaint;
end;

{ ======== TConverterThread ========= }

constructor TConverterThread.Create(CreateSuspended: Boolean);
begin
    Inherited Create(CreateSuspended, thtConverter);
    mFileList:=TStringList.Create;
    mFolders:=TStringList.Create;
    mConverter := TConverter.Create;
end;

destructor TConverterThread.Destroy;
begin
    mFileList.Free;
    mFolders.Free;
    mConverter.Free;
end;

procedure TConverterThread.SetFileList(fileList: TStrings);
var i: Integer;
begin
    mFileList.Clear;
    for i:=0 to fileList.Count-1 do mFileList.Add(fileList[i]);
end;

procedure TConverterThread.SetFolders(folders: TStringList);
var i: Integer;
begin
    mFolders.Clear;
    for i:=0 to folders.Count-1 do mFolders.Add(folders[i]);
end;

procedure TConverterThread.SetFile(filename: String); begin mFilename := filename; end;
procedure TConverterThread.SetOptions(options: TConvertOptions); begin mOptions:=options; end;
procedure TConverterThread.SetOperation(op: TConvertOperation); begin mOperation:=op; end;
procedure TConverterThread.SetEmlDir(emlDir: String); begin mEmlDir:=IncludeTrailingBackslash(emlDir); end;
procedure TConverterThread.SetMboxDir(mboxDir: String); begin mMboxDir:=IncludeTrailingBackslash(mboxDir); end;

procedure TConverterThread.Execute;
var dirs, files: TStringList; i: Integer; mboxFilename: String; errorsOccurred: Boolean;

    { Forms the mbox dir based on the baseDir and the fullDirName.
      The baseDir is the root emlDir, the fullDirName is the name of a
      subdirectory of emlDir. If baseDir is C:\work\emlfiles,
      then the fullDirName of C:\work\emlfiles\test\one will return the
      following mbox name: emlfiles.test.one.mbox
      Note that the last dir in the baseDir is needed in order to
      assign a useful name (emlfiles in the above example) to the mbox
      file for emls in the root folder }
    function FormMboxName(baseDir, fullDirName: String):String;
    var p: Integer; mboxName: String;
    begin
        baseDir:=ExcludeTrailingBackslash(baseDir);
        // Remove the last dir in paths
        baseDir:=ExtractFilePath(baseDir);
        fullDirName:=ExtractFilePath(fullDirName);
        p:=Pos(baseDir,fullDirName);
        if (p>0) then begin
            mboxName:=Copy(fullDirName,Length(baseDir)+1,Length(fullDirName)-Length(baseDir)-1);
            mboxName:=StringReplace(mboxName, '\', '.',[rfReplaceAll, rfIgnoreCase])+'.mbox';
            Result:=mboxName;
        end
        else begin
            raise Exception.Create('Error creating mbox name! baseDir='+baseDir+', fullDirName='+fullDirName);
        end;
    end;

begin
    try
        // Adapt options
        mOptions.exportProgress:=true;  // is false by default

        case mOperation of
            cnvOpEml2Mbox:
                begin
                    if mConverter.eml2mbox(mFileList,mFilename,mOptions,TCustomThread(self)) then
                        requestMgr.SetSummary(ThreadInfo.RequestID,'Messages successfully saved to '+mFilename,mlvInfo)
                    else
                        requestMgr.SetSummary(ThreadInfo.RequestID,'Error converting emls to mbox',mlvError);

                    if terminated then begin
                        Sysutils.DeleteFile(mFilename);
                    end;
                end;
            cnvOpEml2Mboxes:
                begin
                    // Get list of folders in emldir
                    dirs:=TStringList.Create;
                    files:=TStringList.Create;
                    dirs.Add(IncludeTrailingBackslash(mEMLDir));
                    GetSubdirList(dirs, mEMLDir, true);
                    errorsOccurred:=false;
                    for i:=0 to dirs.Count-1 do begin
                        mboxFilename:=FormMboxName(mEMLDir,dirs[i]);
                        devLog.Debug('Converting eml files from directory: '+dirs[i]+' to mbox: '+(mMboxDir+mboxFilename));
                        // Form the list of eml files to convert in this dir
                        files.Clear;
                        LoadFilesByMask(files, dirs[i], '*.eml');

                        if mConverter.eml2mbox(files,mMboxDir+mboxFilename,mOptions,TCustomThread(self)) then begin
                            requestMgr.Log(ThreadInfo.RequestID,'Successfully converted eml files from directory: '+dirs[i]+' to mbox: '+(mMboxDir+mboxFilename));
                        end
                        else begin
                            errorsOccurred:=true;
                            requestMgr.LogError(ThreadInfo.RequestID,'Error occurred while converting eml files from directory: '+dirs[i]+' to mbox: '+(mMboxDir+mboxFilename));
                        end;

                        if terminated then begin
                            devLog.Info('Conversion terminated by user');
                            break;
                        end;

                    end;
                    if errorsOccurred then
                        requestMgr.SetSummary(ThreadInfo.RequestID,'Some errors occurred during the conversion. Please check the details.',mlvError)
                    else
                        requestMgr.SetSummary(ThreadInfo.RequestID,'Conversion was successful',mlvInfo);
                    dirs.Free;
                    files.Free;
                end;
            cnvOpMbox2Eml:
                begin
                    case mConverter.mbox2eml(mFileList,mFolders,mOptions,TCustomThread(self)) of
                        0: requestMgr.SetSummary(ThreadInfo.RequestID,'Messages successfully converted',mlvInfo);
                        1: requestMgr.SetSummary(ThreadInfo.RequestID,'Warnings issued. Click the "Details" button for more details.',mlvWarn);
                        2: requestMgr.SetSummary(ThreadInfo.RequestID,'Error converting mbox to emls. Click the "Details" button for more details.',mlvError);
                    end;

                    if terminated then begin
                        // @todo delete any created files? Not really needed, leave whatever is created for now...
                    end;
                end;
        else devLog.Error('Unknown convert operation');
        end;
        devLog.Trace('ConverterThread done, sending message');
    finally
        // Inform the request manager that the request is over
        PostMessage(requestMgr.Handle,WM_CONVERTER_OVER,ThreadInfo.ID,0);
    end;
end;


{ ============= Global ============= }

{ Just creates a record with the default conversion options }
function GetDefaultConversionOptions: TConvertOptions;
var opt: TConvertOptions;
begin
    opt.UnixLF:=false;
    opt.yzOrder:=false;
    opt.exportProgress:=false;
    opt.progressStart:=0;
    opt.progressEnd:=100;
    Result:=opt;
end;
end.