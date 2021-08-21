{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: SavedSearches.pas,v 1.2 2004/03/31 23:27:32 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}
unit SavedSearches;

interface

uses Classes, SysUtils, EncryptIt, VecLog;

type

    TSavedSearchKey = class(TPersistent)
    private
        fFieldNameIndex: Integer;
        fRelationIndex: Integer;
        fRelation: String; // this one set if fRelationIndex is -1
        fValueIndex: Integer;
        fValue: String;    // this one set if fValueIndex is -1
    published
        property FieldNameIndex: Integer read fFieldNameIndex write fFieldNameIndex;
        property RelationIndex: Integer read fRelationIndex write fRelationIndex;
        property Relation: String read fRelation write fRelation;
        property ValueIndex: Integer read fValueIndex write fValueIndex;
        property Value: String read fValue write fValue;
    end;

    { Just a wrapper around a TSavedSearchKey, since it needs to be a CollectionItem }
    TSavedSearchKeyItem = class(TCollectionItem)
    private
        fSavedSearchKey: TSavedSearchKey;
    public
        constructor Create(Collection:TCollection);override;
        destructor  Destroy;override;
    published
        property SavedSearchKey: TSavedSearchKey read fSavedSearchKey write fSavedSearchKey;
    end;

    TSavedSearch = class(TCollectionItem)
    private
        fName: String;  // name of the search
        fAndOr: Integer;  // 0 - And, 1 - Or
        fSavedSearchKeys: TCollection;
    public
        constructor Create(Collection:TCollection); override;
        destructor  Destroy;override;
        procedure AddSearchKey(key: TSavedSearchKey);
    published
        property Name: String read fName write fName;
        property SavedSearchKeys: TCollection read fSavedSearchKeys write fSavedSearchKeys;
        property AndOr: Integer read fAndOr write fAndOr;
    end;

    TSavedSearches = class(TPersistent)
    private
        fSavedSearches: TCollection;
        function GetSavedSearchIndex(name: String): Integer;
    public
        constructor Create;
        destructor Destroy; override;
        procedure GetSavedSearchNames(var names: TStringList);
        function GetSavedSearch(name: String): TSavedSearch;
        //procedure AddSavedSearch(ss: TSavedSearch);
        procedure RemoveSavedSearch(name: String);
        function GetCount: Integer;
    published
        property SavedSearches: TCollection read fSavedSearches write fSavedSearches;
    end;


implementation

uses Main;

{ ============ TSavedSearchKeyItem ============= }

constructor TSavedSearchKeyItem.Create(Collection:TCollection);
begin
    inherited create(Collection);
    Registerclass(TSavedSearchKey);
    fSavedSearchKey:=TSavedSearchKey.create;
end;

destructor TSavedSearchKeyItem.Destroy;
begin
    try
        fSavedSearchKey.Free;
    finally
        inherited destroy;
    end;
end;

{ ============ TSavedSearch ============= }

{ @param Collection is the collection to which this item will belong,
  this way the element is automatically added to that collection }
constructor TSavedSearch.Create(Collection:TCollection);
begin
    inherited create(Collection);
    RegisterClass(TSavedSearchKeyItem);
    fSavedSearchKeys:=TCollection.Create(TSavedSearchKeyItem);
    fAndOr := 0;  // default AND
end;

destructor TSavedSearch.Destroy;
begin
    try
        fSavedSearchKeys.Free;
    finally
        inherited destroy;
    end;
end;

procedure TSavedSearch.AddSearchKey(key: TSavedSearchKey);
var sski: TSavedSearchKeyItem;
begin
    sski := TSavedSearchKeyItem.Create(fSavedSearchKeys);
    sski.SavedSearchKey.FieldNameIndex := key.FieldNameIndex;
    sski.SavedSearchKey.RelationIndex := key.RelationIndex;
    sski.SavedSearchKey.Relation := key.Relation;
    sski.SavedSearchKey.ValueIndex := key.ValueIndex;
    sski.SavedSearchKey.Value := key.Value;
end;


{ ============ TSavedSearches ============= }

constructor TSavedSearches.Create;
begin
    RegisterClass(TSavedSearch);
    fSavedSearches:=TCollection.create(TSavedSearch);
end;

destructor TSavedSearches.Destroy;
begin
    try fSavedSearches.free;
    finally inherited destroy; end;
end;

{ Returns saved searches (if any) through the var parameter }
procedure TSavedSearches.GetSavedSearchNames(var names: TStringList);
var i: Integer; name: String;
begin
    names.Clear;
    for i:=0 to fSavedSearches.Count-1 do begin
        name:=(fSavedSearches.Items[i] as TSavedSearch).Name;
        names.Add(name);
    end;
end;

{ Returns the index of the specified saved search or -1 if not found }
function TSavedSearches.GetSavedSearchIndex(name: String): Integer;
var found: Boolean; i: Integer;
begin
    found := false; i:=0;
    while (i<fSavedSearches.Count) and (not found) do begin
        if (fSavedSearches.Items[i] as TSavedSearch).Name = name then found:=true
        else Inc(i);
    end;
    if found then Result:=i
    else Result:=-1;
end;

{ Returns the saved search with the specified name }
function TSavedSearches.GetSavedSearch(name: String): TSavedSearch;
var id: Integer;
begin
    id:=GetSavedSearchIndex(name);
    if id>-1 then
        Result:= (fSavedSearches.Items[id] as TSavedSearch)
    else begin
        devLog.error('GetSavedSearch can''t find search named '+name);
        Result:=nil;
    end;
end;



{ Remove the specified SavedSearch }
procedure TSavedSearches.RemoveSavedSearch(name: String);
var id: Integer;
begin
    id := GetSavedSearchIndex(name);
    if id>-1 then fSavedSearches.Delete(id);
end;

function TSavedSearches.GetCount: Integer;
begin
    Result:=fSavedSearches.Count;
end;


{ =========== Global ============ }

end.