{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: NodeStack.pas,v 1.6 2004/04/04 20:17:11 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit NodeStack;

interface

uses Nodes, GlobalConstants;

type

TNodeStack =  class
  private
    nodeArrayStack: TNodeArray;
    nodeCnt: Integer;
    arrayLength: Integer;
    //procedure CopyNode(var fromNode, toNode: TNodeRecord);
  public
    constructor Create;
    procedure Push(node: TNodeRecord); overload;
    procedure Push(nodeName: String; level, byteCount, absIndexInTree: Integer); overload;
    procedure Pop(var node: TNodeRecord);
    procedure Peek(var node: TNodeRecord);
    procedure GetParentNode(var parent: TNodeRecord; level: Integer);
    function CalculateTotalsAndPercents(var nodeArray: TNodeArray; level: Integer): Integer;
    function GetNewTempLevel(var nodeArray: TNodeArray; level: Integer): Integer;
    procedure ClearToLevel(level: Integer);
    procedure Clear;
    destructor Destroy; override;
  end;


implementation

uses Main, MailboxTree;

{ TNodeStack }

constructor TNodeStack.Create;
begin
    SetLength(nodeArrayStack,MBOX_TREE_SIZE_INCREMENT);
    arrayLength:=Length(nodeArrayStack);
    nodeCnt:=0;
end;

procedure TNodeStack.Push(node: TNodeRecord);
begin
    if ((nodeCnt+1)=arrayLength) then begin
       // if the array is full increase its size
       arrayLength:=arrayLength+MBOX_TREE_SIZE_INCREMENT;
       SetLength(nodeArrayStack,arrayLength);
    end;
    CopyNode(node,nodeArrayStack[nodeCnt]);
    nodeCnt:=nodeCnt+1;
end;

procedure TNodeStack.Push(nodeName: String; level, byteCount, absIndexInTree: Integer);
var node: TNodeRecord;
begin
    node.mNodeName:=nodeName;
    node.mLevel:=level;
    node.mSize:=byteCount;
    node.mAbsoluteIndex:=absIndexInTree;
    Push(node);
end;

procedure TNodeStack.Pop(var node: TNodeRecord);
begin
    CopyNode(nodeArrayStack[nodeCnt-1],node);
    nodeCnt:=nodeCnt-1;
end;

procedure TNodeStack.Peek(var node: TNodeRecord);
begin
    CopyNode(nodeArrayStack[nodeCnt-1],node);
end;


{ Finds the parent node of the node that is not yet on the stack,
  but whose level we are passing. E.g. if stack is 0,1,2,2 and
  we pass a 2, the node with level 1 will be returned. No changes on stack.}
procedure TNodeStack.GetParentNode(var parent: TNodeRecord; level: Integer);
var tempLevel, tempCnt: Integer;
begin
    tempCnt:=nodeCnt;
    repeat
        tempLevel:=nodeArrayStack[tempCnt-1].mLevel;
        if tempLevel=level then tempCnt:=tempCnt-1;
    until (tempLevel=(level-1));
    CopyNode(nodeArrayStack[tempCnt-1],parent);
end;

{ Calculates the total for the parent of the specified level and also the
  percentages for this level. Stores everything in the main mboxTree.Items (passed as var)
  Returns the level of the parent (which is the last one remaining on the stack }
function TNodeStack.CalculateTotalsAndPercents(var nodeArray: TNodeArray; level: Integer): Integer;
var tempCnt, tempLevel, i, parentIndex: Integer; tempSum, tempTotal: LongInt;
    tempNumMsgSum, tempNumMsgTotal: Integer;
begin
    if nodeCnt>1 then begin
        tempCnt:=nodeCnt;
        tempSum:=0;
        tempNumMsgSum:=0;

        tempLevel:=nodeArrayStack[tempCnt-1].mLevel;
        while tempLevel=level do begin

           if nodeArray[nodeArrayStack[tempCnt-1].mAbsoluteIndex].mIsLeaf then begin
              nodeArrayStack[tempCnt-1].mTotalNumMessages:=nodeArrayStack[tempCnt-1].mNumMessages;
              nodeArrayStack[tempCnt-1].mTotalSize:=nodeArrayStack[tempCnt-1].mSize;
              nodeArrayStack[tempCnt-1].mNumOfChildren:=0;
           end;
           // We are avoiding to add the sizes and num messages of message nodes
           // as they are contained in the totals of mailboxes.
           tempNumMsgSum:=tempNumMsgSum+nodeArrayStack[tempCnt-1].mTotalNumMessages;
           tempSum:=tempSum+nodeArrayStack[tempCnt-1].mTotalSize;
           tempCnt:=tempCnt-1;
           tempLevel:=nodeArrayStack[tempCnt-1].mLevel;
        end;

        tempNumMsgTotal:=tempNumMsgSum+nodeArrayStack[tempCnt-1].mNumMessages;
        tempTotal:=tempSum+nodeArrayStack[tempCnt-1].mSize;

        parentIndex:=nodeArrayStack[tempCnt-1].mAbsoluteIndex;
        nodeArrayStack[tempCnt-1].mTotalSize:=tempTotal;
        nodeArrayStack[tempCnt-1].mTotalNumMessages:=tempNumMsgTotal;

        // set the number of children for this level
        nodeArrayStack[tempCnt-1].mNumOfChildren:=nodeCnt-tempCnt;
        // now calculate percentages for this level
        for i:=tempCnt to nodeCnt-1 do begin

            if (tempTotal<>0) then
               nodeArrayStack[i].mPercentOfParent:=Round((nodeArrayStack[i].mTotalSize/tempTotal)*100)
            else nodeArrayStack[i].mPercentOfParent:=0;

            nodeArray[nodeArrayStack[i].mAbsoluteIndex].mTotalNumMessages:=nodeArrayStack[i].mTotalNumMessages;
            nodeArray[nodeArrayStack[i].mAbsoluteIndex].mTotalSize:=nodeArrayStack[i].mTotalSize;
            nodeArray[nodeArrayStack[i].mAbsoluteIndex].mPercentOfParent:=nodeArrayStack[i].mPercentOfParent;

            nodeArray[nodeArrayStack[i].mAbsoluteIndex].mNumOfChildren:=nodeArrayStack[i].mNumOfChildren;
            nodeArray[nodeArrayStack[i].mAbsoluteIndex].mParentIndex:=parentIndex;

        end;
        // Now that you calculated everything for this level, remove all the nodes to the parent level
        ClearToLevel(tempLevel);
        // if only remaining level is the root, set its values
        if tempLevel=0 then begin

           nodeArray[0].mTotalNumMessages:=nodeArrayStack[tempCnt-1].mTotalNumMessages;
           nodeArray[0].mTotalSize:=nodeArrayStack[tempCnt-1].mTotalSize;
           nodeArray[0].mPercentOfParent:=100;

           nodeArray[0].mNumOfChildren:=nodeArrayStack[tempCnt-1].mNumOfChildren;
           nodeArray[0].mImageIndex:=0;
        end;
    end
    else begin
        // document contains only the root level
       nodeArray[0].mTotalNumMessages:=nodeArray[0].mNumMessages;
       if (treeState.content>tcHierarchy) then nodeArray[0].mTotalSize:=nodeArray[0].mSize;
       if (treeState.content>tcHierarchy) then nodeArray[0].mPercentOfParent:=100;

       nodeArray[0].mNumOfChildren:=0;
       nodeArray[0].mImageIndex:=0;
       tempLevel:=0;
    end;
    Result:=tempLevel;
end;

{ This returns the same as CalculateTotals, but it doesn't perform
  any calculations.
  //@todo RENAME with appropriate name }
function TNodeStack.GetNewTempLevel(var nodeArray: TNodeArray; level: Integer): Integer;
var tempCnt, tempLevel, parentIndex: Integer;
begin
    if nodeCnt>1 then begin
        tempCnt:=nodeCnt;
        tempLevel:=nodeArrayStack[tempCnt-1].mLevel;
        while tempLevel=level do begin
           if nodeArray[nodeArrayStack[tempCnt-1].mAbsoluteIndex].mIsLeaf then begin
              nodeArrayStack[tempCnt-1].mNumOfChildren:=0;
           end;
           tempCnt:=tempCnt-1;
           tempLevel:=nodeArrayStack[tempCnt-1].mLevel;
        end;
        parentIndex:=nodeArrayStack[tempCnt-1].mAbsoluteIndex;
        // set the number of children for this level
        nodeArrayStack[tempCnt-1].mNumOfChildren:=nodeCnt-tempCnt;
        // Now that you calculated everything for this level, remove all the nodes to the parent level
        ClearToLevel(tempLevel);
        // if only remaining level is the root, set its values
        if tempLevel=0 then begin
           nodeArray[0].mNumOfChildren:=nodeArrayStack[tempCnt-1].mNumOfChildren;
        end;
    end
    else begin
        tempLevel:=0;
    end;
    Result:=tempLevel;
end;

{ Removes from the stack all nodes that are of level greater than the specified }
procedure TNodeStack.ClearToLevel(level: Integer);
var tempLevel: Integer;
begin
    repeat
       tempLevel:=nodeArrayStack[nodeCnt-1].mLevel;
       if tempLevel>level then nodeCnt:=nodeCnt-1;
    until (tempLevel<=level);
end;

procedure TNodeStack.Clear;
begin
    SetLength(nodeArrayStack,0);
end;

destructor TNodeStack.Destroy;
begin
    nodeArrayStack:=nil;
end;

end.