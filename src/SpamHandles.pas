{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: SpamHandles.pas,v 1.5 2004/03/31 23:27:32 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit SpamHandles;

interface

uses Classes, SysUtils, VecLog;

type

    TSpamHandleInfo = class(TPersistent)
    private
        fName: String;
        fSpamHeader: String;
        fSpamComparedBy: Integer;
        fCompareValue: String;
    published
        property Name: String read fName write fName;
        property SpamHeader: String read fSpamHeader write fSpamHeader;
        property SpamComparedBy: Integer read fSpamComparedBy write fSpamComparedBy;
        property CompareValue: String read fCompareValue write fCompareValue;
    end;

    { Just a wrapper around a TAccountInfo, since it needs to be a CollectionItem }
    TSpamHandleItem = class(TCollectionItem)
    private
        fSpamHandleInfo: TSpamHandleInfo;
    public
        constructor Create(Collection:TCollection);override;
        destructor  Destroy;override;
    published
        property SpamHandleInfo: TSpamHandleInfo read fSpamHandleInfo write fSpamHandleInfo;
    end;

    TSpamHandles = class(TPersistent)
    private
        fSpamHandleItems: TCollection;
        fDefaultIndex: Integer;  // Index of the default account
        function GetSpamHandleIndex(name: String): Integer;
    public
        constructor Create;
        function GetSpamHandle(index: Integer): TSpamHandleInfo; overload;
        function GetSpamHandle(name: String): TSpamHandleInfo; overload;
        procedure addSpamHandle(spamHandle: TSpamHandleInfo);
        procedure updateSpamHandle(shName: String; spamHandle: TSpamHandleInfo);
        procedure removeSpamHandle(name: String);
        destructor  Destroy;override;
    published
        property DefaultIndex:Integer read fDefaultIndex write fDefaultIndex;
        property SpamHandleItems: TCollection read fSpamHandleItems write fSpamHandleItems;
    end;

    function SpamHandlesEqual(sh1, sh2: TSpamHandleInfo): Boolean;
    procedure CopySpamHandle(var shFrom, shTo: TSpamHandleInfo);

implementation

uses Main;

constructor TSpamHandles.Create;
begin
    //inherited create(Collection);
    Registerclass(TSpamHandleItem);
    fSpamHandleItems:=TCollection.create(TSpamHandleItem);
end;

function TSpamHandles.GetSpamHandle(index: Integer): TSpamHandleInfo;
begin
    Result := (fSpamHandleItems.Items[Index] as TSpamHandleItem).SpamHandleInfo;
end;


function TSpamHandles.GetSpamHandle(name: String): TSpamHandleInfo;
var index: Integer;
begin
    index := getSpamHandleIndex(name);
    //if (index > -1) then devLog.Warn('GetSpamHandle could not find handle: '+name); //@todo throw exception
    Result := GetSpamHandle(index);
end;

function TSpamHandles.GetSpamHandleIndex(name: String): Integer;
var found: Boolean; i: Integer;
begin
    found:=false;
    i:=0;
    while ((not found) and (i<fSpamHandleItems.Count)) do begin
        if (GetSpamHandle(i).Name = name) then begin
            found := true;
        end
        else Inc(i);
    end;
    if found then Result := i
    else Result := -1;
end;

procedure TSpamHandles.addSpamHandle(spamHandle: TSpamHandleInfo);
var newSH: TSpamHandleInfo; newSHItem: TSpamHandleItem;
begin
    newSHItem:=SpamHandleItems.Add as TSpamHandleItem;
    newSH:=newSHItem.SpamHandleInfo;
    CopySpamHandle(spamHandle,newSH);
end;

procedure TSpamHandles.removeSpamHandle(name: String);
var index: Integer;
begin
    index := getSpamHandleIndex(name);
    if index > -1 then begin
        fSpamHandleItems.Delete(index);
    end
end;

procedure TSpamHandles.updateSpamHandle(shName: String; spamHandle: TSpamHandleInfo);
var index: Integer; sh: TSpamHandleInfo;
begin
    index := getSpamHandleIndex(shName);
    sh:=(fSpamHandleItems.Items[Index] as TSpamHandleItem).SpamHandleInfo;
    CopySpamHandle(spamHandle,sh);
end;


destructor TSpamHandles.Destroy;
begin
    try
        fSpamHandleItems.free;
    finally
        inherited destroy;
    end;
end;


constructor TSpamHandleItem.Create(Collection:TCollection);
begin
    inherited create(Collection);
    Registerclass(TSpamHandleInfo);
    fSpamHandleInfo:=TSpamHandleInfo.create;
end;

destructor TSpamHandleItem.Destroy;
begin
    try
        fSpamHandleInfo.Free;
    finally
        inherited destroy;
    end;
end;

function SpamHandlesEqual(sh1, sh2: TSpamHandleInfo): boolean;
begin
    Result:=false;
    if ((sh1<>nil) and (sh2<>nil)) then begin
        if ((sh1.Name=sh2.Name) and
           (sh1.SpamHeader=sh2.SpamHeader) and
           (sh1.SpamComparedBy=sh2.SpamComparedBy) and
           (sh1.CompareValue=sh2.CompareValue))
        then Result:=true;
    end;
end;

procedure CopySpamHandle(var shFrom, shTo: TSpamHandleInfo);
begin
    shTo.fName:=shFrom.fName;
    shTo.fSpamHeader:=shFrom.fSpamHeader;
    shTo.fSpamComparedBy:=shFrom.fSpamComparedBy;
    shTo.fCompareValue:=shFrom.fCompareValue;
end;



end.