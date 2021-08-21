unit BackupFilenameFormat;

interface

uses sysutils;

{ Can use: %FROM %SUBJECT %DATE %MSGNUM %CURTIME }

const
    FROM_STR = $01;
    SUBJECT_STR = $02;
    DATETIME_STR = $04;
    CURTIME_STR = $10;
    MSGNUM_STR = $20;

type
    TBackupFilenameFormat = class
        templateStr: String;
    public
        constructor Create(template: String);
        function GetRequiredFields: Integer;
        function ExpandTemplate(from, subject, date, curtime, msgnum: String): String;
    published
        property filenameTemplateStr: String read templateStr;
    end;

implementation

constructor TBackupFilenameFormat.Create(template: String);
begin
    templateStr := template;
end;

{ Return a bitmask, with bitfields defining whether a string is present in the template }
function TBackupFilenameFormat.GetRequiredFields: Integer;
var bm: Integer;
begin
    bm:=0;
    if Pos('%FROM',filenameTemplateStr)>0 then bm:=bm or FROM_STR;
    if Pos('%SUBJECT',filenameTemplateStr)>0 then bm:=bm or SUBJECT_STR;
    if Pos('%DATE',filenameTemplateStr)>0 then bm:=bm or DATETIME_STR;
    if Pos('%CURTIME',filenameTemplateStr)>0 then bm:=bm or CURTIME_STR;
    if Pos('%MSGNUM',filenameTemplateStr)>0 then bm:=bm or MSGNUM_STR;
    Result:=bm;
end;

{ Expand the template with actual values passed }
function TBackupFilenameFormat.ExpandTemplate(from, subject, date, curtime, msgnum: String): String;
var res: String;
begin
    res:=filenameTemplateStr;
    res:=StringReplace(res, '%FROM',from,[rfIgnoreCase]);
    res:=StringReplace(res, '%SUBJECT',subject,[rfIgnoreCase]);
    res:=StringReplace(res, '%DATE',date,[rfIgnoreCase]);
    res:=StringReplace(res, '%CURTIME',curtime,[rfIgnoreCase]);
    res:=StringReplace(res, '%MSGNUM',msgnum,[rfIgnoreCase]);
    Result:=res;
end;

end.
