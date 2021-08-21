{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: GlobalSearchResult.pas,v 1.7 2004/03/31 23:27:37 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit GlobalSearchResult;

interface

uses classes, MyTypes, sysutils;

type

    TGlobalSearchInfo = class
        mMailboxIndex: Integer;
        mMsgUIDs: TStringList;
    public
        constructor Create;
        procedure getMsgUIDs(var msgUIDs: TStringList);
        procedure setUIDs(msgUIDs: TStringList);
        function getNumberOfUIDs: Integer;
        destructor Destroy; override;
    published
        property MailboxIndex: Integer read mMailboxIndex write mMailboxIndex;
    end;

    TListOfGlobalSearchInfos = array of TGlobalSearchInfo;

    TGlobalSearchResult = class
        mGlobalSearchInfos: TListOfGlobalSearchInfos;
        mErrorOccurred: Boolean;
    public
        constructor Create;
        function getLength: Integer;
        procedure SetMailboxes(mboxIndexes: TIntArray);
        function Get(Index: Integer): TGlobalSearchInfo;
        function GetByMboxIndex(mboxIndex: Integer): TGlobalSearchInfo;
        procedure SetUIDs(index: Integer; msgUIDs: TStringList);
        procedure Clear;
        destructor Destroy; override;
        property Results[Index: Integer]: TGlobalSearchInfo read Get; default;
    published
        property ErrorOccurred: Boolean read mErrorOccurred write mErrorOccurred;
    end;


implementation

    constructor TGlobalSearchInfo.Create;
    begin
        mMsgUIDs := TStringList.Create;
    end;

    { Returns a copy of the msgUIDs. String list should be created
      and destroyed in the calling methods }
    procedure TGlobalSearchInfo.getMsgUIDs(var msgUIDs: TStringList);
    var i: Integer;
    begin
        for i:=0 to mMsgUIDs.Count-1 do
            msgUIDs.Strings[i]:=mMsgUIDs.Strings[i];
    end;

    procedure TGlobalSearchInfo.setUIDs(msgUIDs: TStringList);
    var i: Integer;
    begin
        for i:=0 to msgUIDs.Count-1 do begin
            mMsgUIDs.Add(msgUIDs.Strings[i]);
        end;
    end;

    function TGlobalSearchInfo.getNumberOfUIDs: Integer;
    begin
        Result:=mMsgUIDs.Count;
    end;

    destructor TGlobalSearchInfo.Destroy;
    begin
        mMsgUIDs.Free;
        Inherited Destroy;
    end;


    { ----------------- TGlobalSearchResult ----------------- }

    constructor TGlobalSearchResult.Create;
    begin
        //
    end;

    { Set mailboxes to search for }
    procedure TGlobalSearchResult.SetMailboxes(mboxIndexes: TIntArray);
    var i: Integer;
    begin
        SetLength(mGlobalSearchInfos,Length(mboxIndexes));
        for i:=0 to Length(mboxIndexes)-1 do begin
            mGlobalSearchInfos[i]:=TGlobalSearchInfo.Create;
            mGlobalSearchInfos[i].MailboxIndex:=mboxIndexes[i];
        end;
    end;

    function TGlobalSearchResult.getLength: Integer;
    begin
        Result:=Length(mGlobalSearchInfos);
    end;

    { Returns the index-th element in the array
      (NOT the mailbox with that index) }
    function TGlobalSearchResult.Get(Index: Integer): TGlobalSearchInfo;
    begin
        Result := mGlobalSearchInfos[Index];
    end;

    function TGlobalSearchResult.GetByMboxIndex(mboxIndex: Integer): TGlobalSearchInfo;
    var found: boolean; i: integer; tmp: TGlobalSearchInfo;
    begin
        found:=false;
        i:=0;
        while (not found) and (i<Length(mGlobalSearchInfos)) do begin
            tmp := mGlobalSearchInfos[i];
            if tmp.MailboxIndex = mboxIndex then found:=true
            else Inc(i);
        end;
        if not found then tmp:=nil;
        Result:=tmp;
    end;

    procedure TGlobalSearchResult.SetUIDs(index: Integer; msgUIDs: TStringList);
    begin
        mGlobalSearchInfos[index].setUIDs(msgUIDs);
    end;

    procedure TGlobalSearchResult.Clear;
    var i: Integer;
    begin
        for i:=0 to Length(mGlobalSearchInfos)-1 do mGlobalSearchInfos[i].Free;
        SetLength(mGlobalSearchInfos,0);
    end;

    destructor TGlobalSearchResult.Destroy;
    begin
        Clear;
        Inherited Destroy;
    end;

end.