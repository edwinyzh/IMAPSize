{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: Nodes.pas,v 1.5 2004/03/31 23:27:33 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit Nodes;

interface

type

TNodeRecord = record
    mNodeName: String;           // Name of the node (article)
    mLevel: Integer;             // Level of the node
    mAbsoluteIndex: Integer;     // Absolute index of node in tree
    mNumMessages: Integer;       // Number of messages in folder
    mTotalNumMessages: Integer;  // Total number of messages in folder
    mSize: Int64;                // Size of the article
    mSizeDisplay: String;        // Size string to be displayed
    mTotalSize: Int64;           // Total size (including children)
    mTotalSizeDisplay: String;   // Total size display string
    mPercentOfParent: Integer;   // Percentage of this node's size compared to parents/
    mFullDisplayedName: String;  // Full node name in tree (includes path from root)
    mFullMailboxName: String;    // Name of the mailbox on the server that is mapped to this node
    mIsLeaf: Boolean;            // True if the node is a leaf
    mNumOfChildren: Integer;     // Number of children of this node
    mImageIndex: Integer;        // Index in ImageList1 of the image associated with this node
    mParentIndex: Integer;       // Index of this parents node
    mExpanded: Boolean;          // True if the node is expanded
    mVirtual: Boolean;           // True if the node doesn't represent a mailbox on the server
    mMaxUID: LongInt;              // Highest UID in the mailbox
    mUIDValidity: LongInt;         // UIDValidity of the mailbox
    mNoInferiors: Boolean;       // True if the server says the folder does not allow children
  end;
  PNodeRecord = ^TNodeRecord;
  TNodeArray = array of TNodeRecord;

  TNodeForList = record
    mFullNodeName: String;
    mSize: Integer;
  end;
  TListOfNodes = array of TNodeForList;

  procedure CopyNode(var fromNode, toNode: TNodeRecord);

implementation

procedure CopyNode(var fromNode, toNode: TNodeRecord);
begin
    toNode.mNodeName:=fromNode.mNodeName;
    toNode.mLevel:=fromNode.mLevel;
    toNode.mSize:=fromNode.mSize;
    toNode.mSizeDisplay:=fromNode.mSizeDisplay;
    toNode.mAbsoluteIndex:=fromNode.mAbsoluteIndex;
    toNode.mNumMessages:=fromNode.mNumMessages;
    toNode.mTotalNumMessages:=fromNode.mTotalNumMessages;
    toNode.mTotalSize:=fromNode.mTotalSize;
    toNode.mTotalSizeDisplay:=fromNode.mTotalSizeDisplay;
    toNode.mPercentOfParent:=fromNode.mPercentOfParent;
    toNode.mFullDisplayedName:=fromNode.mFullDisplayedName;
    toNode.mFullMailboxName:=fromNode.mFullMailboxName;
    toNode.mIsLeaf:=fromNode.mIsLeaf;
    toNode.mNumOfChildren:=fromNode.mNumOfChildren;
    toNode.mImageIndex:=fromNode.mImageIndex;
    toNode.mParentIndex:=fromNode.mParentIndex;
    toNode.mExpanded:=fromNode.mExpanded;
    toNode.mVirtual:=fromNode.mVirtual;
    toNode.mMaxUID:=fromNode.mMaxUID;
    toNode.mUIDValidity:=fromNode.mUIDValidity;
    toNode.mNoInferiors:=fromNode.mNoInferiors;
end;

end.