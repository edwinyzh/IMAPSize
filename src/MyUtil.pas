{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: MyUtil.pas,v 1.6 2004/03/31 23:27:33 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit MyUtil;

interface

uses Classes, ShellAPI, sysutils, Forms, Dialogs, StdCtrls, windows, mytypes;

procedure CopyString(var toStr, fromStr: String);
procedure CopyStringList(var toList, fromList: TStrings);
function BoolToStr(val: Boolean): String;
function CharOcurrenceInString(s: String; c: String): Integer;
function IsKeyDown(Key: Word; c: Char): Boolean;
function DelTree(DirName : string): Boolean;  // taken from http://delphi.about.com/cs/adptips1999/a/bltip1199_2.htm
function MyMessageDialog(const Msg: string; DlgType: TMsgDlgType; Captions: array of string): Integer;
function LoadFileToString(const FileName: TFileName): string;
procedure SaveFileFromString(const FileName: TFileName; const content: string);
function GetParameterValue(const ParamName: string; SwitchChars: TSysCharSet; Seperator: Char; var Value: string): Boolean;
function GetFileSize(const FileName: String): Int64;
function CopyDir(const fromDir, toDir: string): Boolean;
function MoveDir(const fromDir, toDir: string): Boolean;
procedure LoadFilesByMask(lst: TStringList; const SpecDir, WildCard: string);
procedure GetSubdirList(List: TStrings; Directory: String; const Recursive: Boolean);
function HasSubDirs(Dir: string): Boolean;


function CharPos(const S: AnsiString; const C: AnsiChar; const Index: Integer = 1): Integer;
function StrCharCount(const S: AnsiString; C: AnsiChar): Integer;
function StrKeepChars(const S: AnsiString; const Chars: TSysCharSet): AnsiString;
function StrToFloatSafe(const S: AnsiString): Float;


implementation

{ Performs a safe copy of strings. toStr:=fromStr can lead to intermitent @LStrAsg errors }
procedure CopyString(var toStr, fromStr: String);
var i: Integer;
begin
    // SetLength(toStr, lStrLen(PChar(fromStr)));
    SetLength(toStr, Length(fromStr));
    for i:=1 to Length(fromStr) do begin
        toStr[i]:=fromStr[i];
    end;
end;

{ Copies contents of one string list to another one }
procedure CopyStringList(var toList, fromList: TStrings);
var i: Integer;
begin
    toList.Clear;
    for i:=0 to fromList.Count-1 do begin
        toList.Add(fromList[i]);
    end;
end;

function BoolToStr(val: Boolean): String;
begin
    if val then Result:='True' else Result:='False';
end;

{ Gets the number of ocurrences of the specified char in the
  specified string }
function CharOcurrenceInString(s: String; c: String): Integer;
var cnt: Integer;
begin
    cnt:=0;
    while Pos(c, s) > 0 do begin
        s[Pos(c, s)] := 'x'; // replace with dummy char
        cnt:=cnt+1;
    end;
    Result:=cnt;
end;

{ Returns true if the key corresponds to the specified char (case insensitive) }
function IsKeyDown(Key: Word; c: Char): Boolean;
var w: Word;
begin
    // Convert key to uppercase range
    if ((Key>=97) and (Key<=122)) then Key:=Key-32;
    // Convert char to uppercase range
    w:=ord(c);
    if ((w>=97) and (w<=122)) then w:=w-32;
    // Find result
    if (Key=w) then Result:=true else Result:=false;
end;

Function DelTree(DirName : string): Boolean;
var
  SHFileOpStruct : TSHFileOpStruct;
  DirBuf : array [0..255] of char;
begin
  try
   Fillchar(SHFileOpStruct,Sizeof(SHFileOpStruct),0) ;
   FillChar(DirBuf, Sizeof(DirBuf), 0 ) ;
   StrPCopy(DirBuf, DirName) ;
   with SHFileOpStruct do begin
    Wnd := 0;
    pFrom := @DirBuf;
    wFunc := FO_DELETE;
    fFlags := FOF_ALLOWUNDO;
    fFlags := fFlags or FOF_NOCONFIRMATION;
    fFlags := fFlags or FOF_SILENT;
   end;
    Result := (SHFileOperation(SHFileOpStruct) = 0) ;
   except
    Result := False;
  end;
end;


{ Creates a MessageDlg with custom button captions.
  Usage: MyMessageDialog('Some text here', mtInformation, ['First button', 'Second', 'Third'])
  If the first button is pressed 1 is returned, second button 2, etc.
  If the dialog is closed, 0 is returned }
function MyMessageDialog(const Msg: string; DlgType: TMsgDlgType; Captions: array of string): Integer;
var
  aMsgDlg: TForm;
  i: Integer; 
  dlgButton: TButton; 
  CaptionIndex: Integer;
  Buttons: TMsgDlgButtons;
  res: Integer;
begin
  { Create the Dialog }
  case Length(Captions) of
    0: raise Exception.Create('MyMessageDialog needs at least one button!');
    1: Buttons:=[mbOK];
    2: Buttons:=[mbOK,mbAbort];
    3: Buttons:=[mbOK,mbAbort,mbRetry];
    4: Buttons:=[mbOK,mbAbort,mbRetry,mbIgnore];
    5: Buttons:=[mbOK,mbAbort,mbRetry,mbIgnore,mbYes];
  else
    raise Exception.Create('MyMessageDialog only supports up to 5 buttons!');
  end;

  aMsgDlg := CreateMessageDialog(Msg, DlgType, Buttons);
  captionIndex := 0;
  { Loop through Objects in Dialog }
  for i := 0 to aMsgDlg.ComponentCount - 1 do
  begin
   { If the object is of type TButton, then }
    if (aMsgDlg.Components[i] is TButton) then
    begin
      dlgButton := TButton(aMsgDlg.Components[i]);
      if CaptionIndex > High(Captions) then Break;
      { Give a new caption from our Captions array}
      dlgButton.Caption := Captions[CaptionIndex];
      Inc(CaptionIndex); 
    end; 
  end;
  res := aMsgDlg.ShowModal;
  // If the user clicked the close icon return 0
  if res = 2 then res:=0
  // Else if the user clicked a button greater than 2 (mbCancel) then decrement by one
  else if res>2 then res:=res-1;
  Result:=res;
end;

{ Loads contents of a file to a string. File can be binary }
function LoadFileToString(const FileName: TFileName): string;
begin
  with TFileStream.Create(FileName,
      fmOpenRead or fmShareDenyWrite) do begin
    try
      SetLength(Result, Size);
      Read(Pointer(Result)^, Size);
    except
      Result := '';  // Deallocates memory
      Free;
      raise;
    end;
    Free;
  end;
end;

{Saves contents of a string to a file. String can have binary content }
procedure SaveFileFromString(const FileName: TFileName; const content: string);
begin
  with TFileStream.Create(FileName, fmCreate) do
    try
      Write(Pointer(content)^, Length(content));
    finally
      Free;
    end;
end;

{ 
Taken from: http://www.howtodothings.com/showarticle.asp?article=341

GetParameterValue will return the value associated with a parameter name in the form of

/paramname:paramvalue
-paramname:paramvalue

and

/paramname
-paramname

ParamName - Name of the parameter (paramname)
SwitchChars - Parameter switch identifiers (/ or -)
Seperator - The char that sits between paramname and paramvalue (:)
Value - The value of the parameter (paramvalue) if it exists

Returns - Boolean, true if the parameter was found, false if parameter does not exist

typical usage

Parameter
-P=c:\temp\
-S

GetParameterValue('p', ['/', '-'], '=', sValue);

sValue will contain c:\temp\
}
function GetParameterValue(const ParamName: string; SwitchChars: TSysCharSet;
  Seperator: Char; var Value: string): Boolean;
var
  I, Sep: Longint;
  S: string;
begin
  Result := False;
  Value := '';

  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    if Length(S) > 0 then
      if S[1] in SwitchChars then
      begin
        Sep := Pos(Seperator, S);

        case Sep of
          0:
            begin
              if CompareText(Copy(S, 2, Length(S) -1), ParamName) = 0 then
              begin
                Result := True;
                Break;
              end;
            end;
          1..MaxInt:
            begin
              if CompareText(Copy(S, 2, Sep -2), ParamName) = 0 then
              begin
                Value := Copy(S, Sep + 1, Length(S));
                Result := True;
                Break;
              end;
            end;
        end; //case
      end;
  end;
end;

{ Determines the size of a file in bytes, the size can be more than 2 GB }
function GetFileSize(const FileName: String): Int64;
var
  myFile: THandle;
  myFindData: TWin32FindData;
begin
  // set default value
  Result := 0;
  // get the file handle.
  myFile := FindFirstFile(PChar(FileName), myFindData);
  if (myFile <> INVALID_HANDLE_VALUE) then
  begin
    Windows.FindClose(myFile);
    Int64Rec(Result).Lo := myFindData.nFileSizeLow;
    Int64Rec(Result).Hi := myFindData.nFileSizeHigh;
  end;
end;

{ Copy directory. Uses ShellAPI }
function CopyDir(const fromDir, toDir: string): Boolean;
var
  fos: TSHFileOpStruct;
begin
  ZeroMemory(@fos, SizeOf(fos));
  with fos do
  begin
    wFunc  := FO_COPY;
    fFlags := FOF_FILESONLY;
    pFrom  := PChar(fromDir + #0);
    pTo    := PChar(toDir)
  end;
  Result := (0 = ShFileOperation(fos));
end;

{ Move directory. Uses ShellAPI }
function MoveDir(const fromDir, toDir: string): Boolean;
var
  fos: TSHFileOpStruct;
begin
  ZeroMemory(@fos, SizeOf(fos));
  with fos do
  begin
    wFunc  := FO_MOVE;
    fFlags := FOF_FILESONLY;
    pFrom  := PChar(fromDir + #0);
    pTo    := PChar(toDir)
  end;
  Result := (0 = ShFileOperation(fos));
end;

{ For example, to load bmp-files from application folder:
  LoadFilesByMask(lbFiles.Items, ExtractFilePath(Application.ExeName), '*.bmp') }
procedure LoadFilesByMask(lst: TStringList; const SpecDir, WildCard: string);
var
  intFound: Integer;
  SearchRec: TSearchRec;
begin
  lst.Clear;
  intFound := FindFirst(SpecDir + WildCard, faAnyFile, SearchRec);
  while intFound = 0 do
  begin
    lst.Add(SpecDir + SearchRec.Name);
    intFound := FindNext(SearchRec);
  end;
  sysutils.FindClose(SearchRec);
end;

// Recursive procedure to build a list of subdirectories
procedure GetSubdirList(List: TStrings; Directory: String; const Recursive: Boolean);
var
  bFoundFile: Boolean;
  mySearchRec: TSearchRec;
  sFileName: String;
begin
  Directory := IncludeTrailingBackslash(Directory);
  bFoundFile := FindFirst(Directory + '*.*', faAnyFile, mySearchRec) = 0;
  while bFoundFile do
  begin
    // skip "." and ".."
    if (mySearchRec.Name[1] <> '.') then
    begin
      sFileName := Directory + mySearchRec.Name;
      if ((mySearchRec.Attr and faDirectory) > 0) then
      begin // found a directory
        sFileName := IncludeTrailingBackslash(sFileName);
        List.Add(sFileName);
        // list the subdirectory
        if Recursive then GetSubdirList(List, sFileName, Recursive);
      end;
    end;
    // find next file
    bFoundFile := FindNext(mySearchRec) = 0;
  end;
  sysutils.FindClose(mySearchRec);
end;

function HasSubDirs(Dir: string): Boolean;
var
  sr: TSearchRec;
begin
  result := false;
  dir := IncludeTrailingBackslash(dir);
  if FindFirst(dir + '*.*', faAnyfile, sr) = 0 then
  begin
    repeat
      result := (sr.attr and faDirectory <> 0) and (sr.name <> '.') and (sr.name <>'..');
    until
      result or (FindNext(sr) <> 0);
    sysutils.FindClose(sr);
  end;
end;

{ Following methods taken from JclStrings }

function CharPos(const S: AnsiString; const C: AnsiChar; const Index: Integer): Integer;
var
  P: PAnsiChar;
begin
  Result := 0;
  if (Index > 0) and (Index <= Length(S)) then
  begin
    P := PAnsiChar(S);
    Result := Index - 1;
    Inc(P, Result);
    while P^ <> #0 do
    begin
      Inc(Result);
      if P^ = C then
        Break;
      Inc(P);
    end;
    if P^ = #0 then
      Result := 0;
  end;
end;

function StrCharCount(const S: AnsiString; C: AnsiChar): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if S[I] = C then
      Inc(Result);
end;

function StrKeepChars(const S: AnsiString; const Chars: TSysCharSet): AnsiString;
var
  Source, Dest: PChar;
begin
  SetLength(Result, Length(S));
  UniqueString(Result);
  Source := PChar(S);
  Dest := PChar(Result);
  while (Source <> nil) and (Source^ <> #0) do
  begin
    if Source^ in Chars then
    begin
      Dest^ := Source^;
      Inc(Dest);
    end;
    Inc(Source);
  end;
  SetLength(Result, (Longint(Dest) - Longint(PChar(Result))) div SizeOf(AnsiChar));
end;


function StrToFloatSafe(const S: AnsiString): Float;
var
  Temp: AnsiString;
  I, J, K: Integer;
  SwapSeparators, IsNegative: Boolean;
begin
  Temp := S;
  SwapSeparators := False;

  IsNegative := False;
  J := 0;
  for I := 1 to Length(Temp) do
  begin
    if Temp[I] = '-' then
      IsNegative := not IsNegative
    else
      if not (Temp[I] in [' ', '(', '+']) then
      begin
        // if it appears prior to any digit, it has to be a decimal separator
        SwapSeparators := Temp[I] = ThousandSeparator;
        J := I;
        Break;
      end;
  end;

  if not SwapSeparators then
  begin
    K := CharPos(Temp, DecimalSeparator);
    SwapSeparators :=
      // if it appears prior to any digit, it has to be a decimal separator
      (K > J) and
      // if it appears multiple times, it has to be a thousand separator
      ((StrCharCount(Temp, DecimalSeparator) > 1) or
      // we assume (consistent with Windows Platform SDK documentation),
      // that thousand separators appear only to the left of the decimal
      (K < CharPos(Temp, ThousandSeparator)));
  end;

  if SwapSeparators then
  begin
    // assume a numerical string from a different locale,
    // where DecimalSeparator and ThousandSeparator are exchanged
    for I := 1 to Length(Temp) do
      if Temp[I] = DecimalSeparator then
        Temp[I] := ThousandSeparator
      else
        if Temp[I] = ThousandSeparator then
          Temp[I] := DecimalSeparator;
  end;

  Temp := StrKeepChars(Temp, ['0'..'9', DecimalSeparator]);

  if Length(Temp) > 0 then
  begin
    if Temp[1] = DecimalSeparator then
      Temp := '0' + Temp;
    if Temp[length(Temp)] = DecimalSeparator then
      Temp := Temp + '0';
    Result := StrToFloat(Temp);
    if IsNegative then
      Result := -Result;
  end
  else
    Result := 0.0;
end;


end.