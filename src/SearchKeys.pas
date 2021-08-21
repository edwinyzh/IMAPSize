{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: SearchKeys.pas,v 1.4 2004/03/31 23:27:32 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit SearchKeys;

{ Unit that provides values/mappings for specifying search
  keys in the advanced search dialog }

interface

uses VecLog, classes;

const

   KEY_IDX_SUBJECT = 0;
   KEY_IDX_BODY = 1;
   KEY_IDX_ENTIRE_MESSAGE = 2;
   KEY_IDX_SENDER = 3;
   KEY_IDX_RECIPIENT = 4;
   KEY_IDX_CORRESPONDENT = 5;
   KEY_IDX_HEADER_FIELD = 6;
   KEY_IDX_DATE = 7;
   KEY_IDX_SIZE = 8;
   KEY_IDX_STATUS = 9;
   KEY_IDX_KEYWORD_FLAG = 10;

   SEARCH_KEY: array[0..10] of String = (
        'Subject',          {0}
        'Body',             {1}
        'Entire Message',   {2}
        'Sender',           {3}
        'Recipient',        {4}
        'Correspondent',    {5}
        'Header Field',     {6}
        'Date (dd/mm/yyyy)',{7}
        'Size',             {8}
        'Status',           {9}
        'Keyword Flag'      {10}
   );

   EMPTY_ARRAY: array[0..0] of String = (''); //@todo

   REL_TEXT:   array[0..1] of String = ('contains','does not contain');
   REL_DATE:   array[0..2] of String = ('is','is before', 'is after');
   REL_SIZE:   array[0..1] of String = ('is greater than', 'is less than');
   REL_STATUS: array[0..1] of String = ('is','is not');

   VAL_FLAGS:  array[0..5] of String = ('seen', 'flagged', 'deleted', 'draft', 'replied', 'recent');

type
    TSearchKeyInfo = class
        Name: String;
        Display: String;
        Relations: TStringList;   // List of possible relations for this key
        ValueList: TStringList;   // List of possible values for this search key
        constructor Create(inName: String; const inRelations, inValueList: Array of String);
        destructor Destroy; override;
    end;

    { Class that contains some static info about the search keys.
      Can easily be extended/modified with new search keys/values if needed,
      even from a settings file }
    TSearchKeys = class
        KeyInfos: TList;
        constructor Create;
        destructor Destroy; override;
        procedure PopulateSearchKeys;
        function GetSearchKey(name: String): TSearchKeyInfo;
        procedure GetSearchKeyNames(var names: TStringList);
    end;

implementation

uses Main;

{--------------- TSearchKeyInfo ---------------}

constructor TSearchKeyInfo.Create(inName: String; const inRelations, inValueList: Array of String);
var i: Integer;
begin
    Name:=inName;
    Display:=inName;  // Currently same as name. Modify this if the display should vary from the name (handle)
    Relations := TStringList.Create;
    if High(inRelations)>0 then
        for i := Low(inRelations) to High(inRelations) do Relations.Add(inRelations[i]);

    ValueList := TStringList.Create;
    if High(inValueList)>0 then
        for i := Low(inValueList) to High(inValueList) do ValueList.Add(inValueList[i]);
end;

destructor TSearchKeyInfo.Destroy;
begin
    Relations.Free;
    ValueList.Free;
end;

{--------------- TSearchKeys ---------------}

constructor TSearchKeys.Create;
begin
    KeyInfos := TList.Create;
    PopulateSearchKeys;
end;

destructor TSearchKeys.Destroy;
var i: Integer;
begin
    for i:=0 to KeyInfos.Count-1 do begin
        if KeyInfos[i] <> nil then TSearchKeyInfo(KeyInfos[i]).Free;
    end;
    KeyInfos.Free;
end;

procedure TSearchKeys.PopulateSearchKeys;
begin
    KeyInfos.Add(TSearchKeyInfo.Create(SEARCH_KEY[0],REL_TEXT,EMPTY_ARRAY));
    KeyInfos.Add(TSearchKeyInfo.Create(SEARCH_KEY[1],REL_TEXT,EMPTY_ARRAY));
    KeyInfos.Add(TSearchKeyInfo.Create(SEARCH_KEY[2],REL_TEXT,EMPTY_ARRAY));
    KeyInfos.Add(TSearchKeyInfo.Create(SEARCH_KEY[3],REL_TEXT,EMPTY_ARRAY));
    KeyInfos.Add(TSearchKeyInfo.Create(SEARCH_KEY[4],REL_TEXT,EMPTY_ARRAY));
    KeyInfos.Add(TSearchKeyInfo.Create(SEARCH_KEY[5],REL_TEXT,EMPTY_ARRAY));
    KeyInfos.Add(TSearchKeyInfo.Create(SEARCH_KEY[6],EMPTY_ARRAY,EMPTY_ARRAY));
    KeyInfos.Add(TSearchKeyInfo.Create(SEARCH_KEY[7],REL_DATE,EMPTY_ARRAY));
    KeyInfos.Add(TSearchKeyInfo.Create(SEARCH_KEY[8],REL_SIZE,EMPTY_ARRAY));
    KeyInfos.Add(TSearchKeyInfo.Create(SEARCH_KEY[9],REL_STATUS,VAL_FLAGS));
    KeyInfos.Add(TSearchKeyInfo.Create(SEARCH_KEY[10],EMPTY_ARRAY,EMPTY_ARRAY));
end;

{ Returns the specified SearchKeyInfo. Returns nil if not found }
function TSearchKeys.GetSearchKey(name: String): TSearchKeyInfo;
var found: Boolean; i: Integer;
begin
    found:=false; i:=0;
    while ((not found) and (i<KeyInfos.Count)) do begin
        if TSearchKeyInfo(KeyInfos[i]).Name = name then found := true
        else Inc(i);
    end;
    if found then Result:=TSearchKeyInfo(KeyInfos[i])
    else begin
        devLog.Error('SearchKey not found: '+name);
        Result:=nil;
    end;
end;

{ Retrieves the list of available search key names.
  Parameter should be passed in and freed upon return }
procedure TSearchKeys.GetSearchKeyNames(var names: TStringList);
var i: Integer;
begin
    names.Clear;
    for i:=0 to KeyInfos.Count-1 do begin
        names.Add(TSearchKeyInfo(KeyInfos[i]).Name);
    end;
end;

end.




