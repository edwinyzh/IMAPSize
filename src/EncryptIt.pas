{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: EncryptIt.pas,v 1.5 2004/03/31 23:27:38 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit EncryptIt;

{ Using slightly modified encrypt/decrypt functions from
  http://homepages.borland.com/efg2lab/Library/Delphi/MathFunctions/SimpleSecurity.ZIP
  It stores encrypted strings as hex valuex (which we need for storing
  passwords in an xml file }  

interface

uses Synacode, SysUtils;

const
     C1 = 52835;
     C2 = 22711;

function Encrypt(const S: String): String;
function Decrypt(const S: String): String;

implementation

function Encrypt(const s:  String):  String;
var b: Byte;
    i: Integer;
    key: Word;
begin
    key := 10208;
    Result := '';
    for i := 1 TO Length(s) do begin
        b := byte(s[i]) XOR (key SHR 8);
        key := (b + key) * C1 + C2;
        Result := Result + IntToHex(b,2)
    end
end;


function Decrypt(const s: String): String;
var b: Byte;
    i: Integer;
    key: Word;
begin
    b := 0;
    key := 10208;
    Result := '';
    for i := 1 TO LENGTH(s) div 2 do
    begin
        try
            b :=  StrToInt('$' + copy(s, 2*i-1, 2));
        except
            on EConvertError DO b := 0
        end;
        Result := Result + char( b XOR (key SHR 8) );
        key := (b + key) * C1 + C2;
    end
end;

end.