{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: MailboxTree.pas,v 1.4 2004/04/04 20:17:11 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit MailboxTree;

{ Encapsulates the data for the tree view }

interface

uses Nodes, NodeStack, Accounts, Windows, sysutils, veclog;

type

    // Defines the states the tree content can be in...
    TTreeContent = (tcEmpty, tcHierarchy, tcSize);

    TMailboxTreeState = record
        content: TTreeContent;
        account: PAccountInfo; // Pointer to the account information of the account whose contents are displayed
    end;

    TMailboxTree = class
    private
        mNodeArrayCnt: Integer;          // Actual number of nodes in mNodeArray
        mNodeArraySize: Integer;         // Size of mNodeArray (this is usually bigger than mNodeArrayCnt
        ItemsCS: TRTLCriticalSection;
    public
        Items: TNodeArray;  // @todo try to encapsulate this completely (no direct access from outside)
        ListOfNodes: TListOfNodes;      // List of mailboxes used by ListNodes
        constructor Create;
        destructor Destroy; override;
        procedure CalculateSizes;
        procedure SetImageIndexes;
        procedure FormatSizes;
        procedure MakeListOfNodesForSize;
        procedure SortListOfNodesForSize;
        procedure Quicksort(var A: TListOfNodes; l, r: Integer);
        procedure EnterCS;
        procedure LeaveCS;
    published
        property Count: Integer read mNodeArrayCnt write mNodeArrayCnt;
        property Size: Integer read mNodeArraySize write mNodeArraySize;
    end;

    function formatSizeDisplay(size: Int64): String;

implementation

uses Main;

constructor TMailboxTree.Create;
begin
    InitializeCriticalSection(ItemsCS);
    SetLength(Items,0);
    SetLength(ListOfNodes,0);
end;

destructor TMailboxTree.Destroy;
begin
    SetLength(Items,0);
    SetLength(ListOfNodes,0);
    DeleteCriticalSection(ItemsCS);
end;

{ Calculates totals, percentages based on mNodeArray and stores calculated
  information in that array. After this method the array can be sent to the tree
  for display }
procedure TMailboxTree.CalculateSizes;
var i, NodeLevel, PrevLevel, TempLevel: Integer; stack: TnodeStack;
    parentNodeObj: TNodeRecord;
begin
    stack:=TNodeStack.Create;
    EnterCS;
    try
        try
            NodeLevel:=0; PrevLevel:=0;
            for i:=0 to mNodeArrayCnt-1 do begin

                // Don't calculate sizes if the size check has not been performed
                if (treeState.content<=tcHierarchy) then begin
                    Items[i].mNumMessages:=0;
                    Items[i].mSize:=0;
                end;

                NodeLevel:=Items[i].mLevel;

                if NodeLevel = 0 then begin
                    stack.Push(Items[i]);
                end
                else if NodeLevel>prevLevel then begin
                    // This node is a child of the previous one
                    stack.Peek(parentNodeObj);
                    stack.Push(Items[i]);
                end
                else if NodeLevel = prevLevel then begin
                    // New node is at the same level as the previous one
                    stack.GetParentNode(parentNodeObj, Items[i].mLevel);
                    stack.Push(Items[i]);
                end
                else begin
                    tempLevel:=prevLevel;
                    repeat
                        tempLevel:=stack.CalculateTotalsAndPercents(Items, tempLevel)
                    until tempLevel=NodeLevel;

                    // then process the current node
                    stack.GetParentNode(parentNodeObj, Items[i].mLevel);
                    stack.Push(Items[i]);
                end;
                prevLevel:=NodeLevel;
            end;
            // calculate totals for nodes remaining on stack
            tempLevel:=NodeLevel;
            repeat
                tempLevel:=stack.CalculateTotalsAndPercents(Items, tempLevel);
            until tempLevel=0;

            // Format sizes for display
            FormatSizes;
        except
            // do nothing
        end;
    finally
        stack.Free;
        LeaveCS;
    end;
end;

{ Sets the indexes for every node depending on the percentage of their size and
  the settings in ini }
procedure TMailboxTree.SetImageIndexes;
var i: Integer; doColor: Boolean; percentOfParent: Integer;
begin
    EnterCS;
    try
        for i:=0 to mNodeArrayCnt-1 do begin
            if i=0 then begin
                // Special symbol for the root node
                Items[i].mImageIndex:=3;
            end
            else if Items[i].mVirtual then begin
                // Set all other virtual nodes to a special image
                Items[i].mImageIndex:=10;
            end
            else begin
                percentOfParent:=Items[i].mPercentOfParent;
                if settings.Thresholds.AllowLowNumChange then begin
                    if Items[Items[i].mParentIndex].mNumOfChildren>=settings.Thresholds.AllowLowNumThr
                        then doColor:=true
                        else doColor:=false;
                end
                else doColor:=true;
                if doColor=true then begin
                   if percentOfParent>settings.Thresholds.Upper then Items[i].mImageIndex:=2
                   else if percentOfParent>settings.Thresholds.Lower then Items[i].mImageIndex:=1
                   else Items[i].mImageIndex:=0;
                end
                else begin
                    Items[i].mImageIndex:=0;
                end;
            end;
        end;
    finally
        LeaveCS;
    end;
end;

{ Formats all the sizes for display }
procedure TMailboxTree.FormatSizes;
var i: Integer;
begin
    EnterCS;
    try
        // Format sizes for display
        for i:=0 to mNodeArrayCnt-1 do begin
            Items[i].mSizeDisplay:=formatSizeDisplay(Items[i].mSize);
            Items[i].mTotalSizeDisplay:=formatSizeDisplay(Items[i].mTotalSize);
        end;
    finally
        LeaveCS;
    end;
end;

{ Makes a list of nodes with their full name and size
  Only mailboxes are taken into account }
procedure TMailboxTree.MakeListOfNodesForSize;
var i, listOfNodesCnt: Integer;
begin
    SetLength(ListOfNodes,0);
    devLog.Trace('========= setting length of listofnodes to '+inttostr(mNodeArrayCnt));
    SetLength(ListOfNodes,mNodeArrayCnt-1);
    listOfNodesCnt := 0;  // need this since we are skipping non-mailboxes

    EnterCS;  // This is really a cs for Items, but it won't harm...
    try
        // Skipping the virtual root node
        for i:=1 to mNodeArrayCnt-1 do begin
            // Only add mailboxes
            ListOfNodes[listOfNodesCnt].mFullNodeName:=Items[i].mFullDisplayedName;
            ListOfNodes[listOfNodesCnt].mSize:=Items[i].mSize;
            Inc(listOfNodesCnt);
        end;
    finally
        SetLength(ListOfNodes,listOfNodesCnt);   // update size of list
        LeaveCS;
    end;
end;

procedure TMailboxTree.SortListOfNodesForSize;
begin
    EnterCS;
    try
        Quicksort(ListOfNodes,0,Length(ListOfNodes)-1);
    finally
        LeaveCS;
    end;
end;

procedure TMailboxTree.Quicksort(var A: TListOfNodes; l, r: Integer);
var
  i, j: Integer;
  Help: TNodeForList; v: Integer;
begin
  v := A[(l + r) div 2].mSize;
  i := l; j := r;
  repeat
    while A[i].mSize < v do inc(i);
    while v < A[j].mSize do dec(j);
    if i <= j then begin
      Help := A[i]; A[i]:= A[j]; A[j]:= Help;
      inc(i); dec(j);
    end;
  until i > j;
  if l < j then Quicksort(A, l, j);
  if i < r then Quicksort(A, i, r);
end;

{ Enters critical section for the node array (Items) }
procedure TMailboxTree.EnterCS;
begin
    EnterCriticalSection(ItemsCS);
end;

{ Leaves critical section for the node array (Items) }
procedure TMailboxTree.LeaveCS;
begin
    LeaveCriticalSection(ItemsCS);
end;

{ Formats the display of the size (individual size) }
function formatSizeDisplay(size: Int64): String;
var str: String;

    function convertByte(size: Int64): String;
    begin
        Result := FloatToStrF(size,ffNumber,12,0);
    end;

begin
    str:=IntToStr(size);  // Default (for 0 (byte))
    case settings.TreeDisplay.SizeFormat of
       -1,0: str:=convertByte(size);
       1: str := FloatToStrF(size/1024,ffNumber,9,1) + 'K'; // KB
       2: str := FloatToStrF(size/ONE_MEG,ffNumber,6,2) + 'M'; // MB
       3: begin
             // Smart (displays whatever is most suitable)
             if (size>ONE_MEG) then str := FloatToStrF(size/ONE_MEG,ffNumber,6,2) + 'M'
             else if (size>1024) then str := FloatToStrF(size/1024,ffNumber,9,1) + 'K'
             else str:=convertByte(size);
          end;
    end;
    Result:=str;
end;

end.