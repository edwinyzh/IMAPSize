unit GUIDs;

 

interface
Uses classes,sysutils;
 

 function  CreateObjectID :String;

 type




PGUID = ^TGUID;
  TGUID = record
    D1: LongWord;
    D2: Word;
    D3: Word;
    D4: array[0..7] of Byte;
  end;
  TCLSID = TGUID;
  EOleError = class(Exception);
 EOleSysError = class(EOleError)
  private
    FErrorCode: HRESULT;
  public
    constructor Create(const Message: string; ErrorCode: HRESULT;
      HelpContext: Integer);
    property ErrorCode: HRESULT read FErrorCode write FErrorCode;
  end;


{$EXTERNALSYM CoCreateGuid}
function CoCreateGuid(var guid: TGUID): HResult; stdcall;
{$EXTERNALSYM CoTaskMemFree}
procedure CoTaskMemFree(pv: Pointer); stdcall;
{$EXTERNALSYM StringFromCLSID}
function StringFromCLSID(const clsid: TCLSID; out psz: PWideChar): HResult; stdcall;




implementation


const
  ole32    = 'ole32.dll';
  oleaut32 = 'oleaut32.dll';
function CoCreateGuid;                  external ole32 name 'CoCreateGuid';
function StringFromCLSID;               external ole32 name 'StringFromCLSID';
procedure CoTaskMemFree;                external ole32 name 'CoTaskMemFree';

function Succeeded(Res: HResult): Boolean;
begin
  Result := Res and $80000000 = 0;
end;

procedure OleError(ErrorCode: HResult);
begin
  raise EOleSysError.Create('', ErrorCode, 0);
end;

procedure OleCheck(Result: HResult);
begin
  if not Succeeded(Result) then OleError(Result);
end;

{ EOleSysError }
const
SOleError = 'OLE error %.8x';

constructor EOleSysError.Create(const Message: string;
  ErrorCode: HRESULT; HelpContext: Integer);
var
  S: string;
begin
  S := Message;
  if S = '' then
  begin
    S := SysErrorMessage(ErrorCode);
    if S = '' then FmtStr(S, SOleError, [ErrorCode]);
  end;
  inherited CreateHelp(S, HelpContext);
  FErrorCode := ErrorCode;
end;


function GUIDToString(const ClassID: TGUID): string;
var
  P: PWideChar;
begin
  OleCheck(StringFromCLSID(ClassID, P));
  Result := P;
  CoTaskMemFree(P);
end;

function  CreateObjectID :String;
var newGUID:TGUID;
begin
{}
CoCreateGuid(newGuid);
result:= GUidTostring(newGUID);
end;



end.
