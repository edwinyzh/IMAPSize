{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: BodyPartsFetch.pas,v 1.1 2004/03/31 23:27:39 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

{ Class used for managing fetches of multiple body parts. Used by the message peeker }

unit BodyPartsFetch;

interface

uses classes, VecLog, MyUtil;

type

    FetchBodyPartStat = record
        bodyPartID: String;     // Body part ID (e.g. 1.1.2)
        fetched: Boolean;       // true when the body part has been fetched
    end;
    TListOfFetchBodyPartStats = array of FetchBodyPartStat;
    TListOfStringLists = array of TStringList;

    TBodyPartsFetch = class
        fetchStats: TListOfFetchBodyPartStats;
        bodyParts: TListOfStringLists;
        function GetInternalIDOfBodyPart(bodyPartId: String): Integer;
      public
        constructor Create(partsToFetch: TStringList);
        destructor Destroy; override;
        procedure AddPart(var bodyPart, originalBoundaries: TStringList; bodyPartId: String);
        function GetNextPartToFetch: String;
        function GetBodyPartLines(index: Integer): TStringList;
        function GetNumberOfParts: Integer;
        function GetBodyPartID(index: Integer): String;
    end;

implementation

uses Main, sysutils, Log;

constructor TBodyPartsFetch.Create(partsToFetch: TStringList);
var i: Integer;
begin
    SetLength(fetchStats,partsToFetch.Count);
    for i:=0 to partsToFetch.Count-1 do begin
        fetchStats[i].bodyPartID:=partsToFetch[i];
        fetchStats[i].fetched:=false;
    end;
    SetLength(bodyParts,partsToFetch.Count);
end;

{ Adds a body part to our list. Also removes any existing original MIME boundaries and adds
  any new ones that it may find...
  @param bodyPart is the actual content of the part,
  @param originalBoundaries are the boundaries of the original message. These should be removed.
  @param bodyPartID is the ID of the part }
procedure TBodyPartsFetch.AddPart(var bodyPart, originalBoundaries: TStringList; bodyPartId: String);
var i,j,k, index, bndPos: Integer; boundaryStarted: Boolean; bnd: String;
begin
    boundaryStarted:=false;  // true when the boundary (and content-type) section are active (need to skip)
    index:=GetInternalIDOfBodyPart(bodyPartId);
    if fetchStats[index].fetched then
        devLog.warn('Body part already added')
    else begin
        bodyParts[index]:=TStringList.Create; // will be freed on bodyPart.Destroy
        i:=0;
        while i<bodyPart.Count do begin
            if Pos('--',bodyPart[i])=1 then begin // RFC 2046 boundary always begins with '--'
                j:=0;
                while j<originalBoundaries.Count do begin
                    if Pos(originalBoundaries[j],bodyPart[i])>0 then begin
                        // Remove all previous lines (usually will contain the 'This is a MIME message...' string
                        bodyParts[index].Clear;
                        // Remove all lines until the empty line
                        repeat
                            Inc(i);
                            // If the line is a boundary definition and this boundary of the inline part
                            // is not in the list of boundaries, add it
                            bndPos:=Pos('boundary="',LowerCase(bodyPart[i]));
                            if bndPos>0 then begin
                                // This line contains the definition of the boundary. Extract the boundary
                                k:=bndPos+10;
                                bnd:='';
                                while bodyPart[i][k]<>'"' do begin bnd:=bnd+bodyPart[i][k]; Inc(k); end;
                                devLog.Trace('Inline MIME boundary is: '+bnd);
                                if originalBoundaries.IndexOf(bnd)=-1 then originalBoundaries.Add(bnd);
                            end;
                        until bodyPart[i]='';
                    end;
                    Inc(j);
                end;
            end;

            // Add the line to the body part
            bodyParts[index].Add(bodyPart[i]);
            Inc(i);
        end;
        fetchStats[index].fetched:=true;
    end;
end;

{ Returns the internal id (in fetchStats) for this BodyPart }
function TBodyPartsFetch.GetInternalIDOfBodyPart(bodyPartId: String): Integer;
var found: Boolean; i: Integer;
begin
    found:=false; i:=0;
    while (not found) and (i<Length(fetchStats)) do begin
        if (fetchStats[i].bodyPartID = bodyPartID) then found:=true
        else Inc(i);
    end;
    if not found then devLog.error('Unknown body part ID');  //@todo throw exception
    Result:=i;
end;

{ Returns the first body part from fetchStats that is not yet fetched }
function TBodyPartsFetch.GetNextPartToFetch: String;
var found: Boolean; i: Integer;
begin
    found:=false; i:=0;
    while (not found) and (i<Length(fetchStats)) do begin
        if not fetchStats[i].fetched then found:=true
        else Inc(i);
    end;
    if found then Result:=fetchStats[i].bodyPartID
    else Result:='DONE';
end;

{ Returns the lines that form this body part.
  @param index is the internal index in the body parts list }
function TBodyPartsFetch.GetBodyPartLines(index: Integer): TStringList;
begin
    Result:=bodyParts[index];
end;

function TBodyPartsFetch.GetNumberOfParts: Integer;
begin
    Result:=Length(fetchStats);
end;

{ Returns the part id (e.g. 1.1.2) of the part with the specified index }
function TBodyPartsFetch.GetBodyPartID(index: Integer): String;
begin
    result:=fetchStats[index].bodyPartID;
end;

destructor TBodyPartsFetch.Destroy;
var i: Integer;
begin
    SetLength(fetchStats,0);
    fetchStats:=nil;
    for i:=0 to Length(bodyParts)-1 do bodyParts[i].Free;
    SetLength(bodyParts,0);
    bodyParts:=nil;
end;

end.