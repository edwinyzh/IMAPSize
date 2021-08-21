{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: Stacks.pas,v 1.1 2004/03/31 23:27:32 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

{ Stack for strings }
unit Stacks;

interface

uses classes, sysutils, VecLog;

const
    INT_MAX_SIZE = 50; // maximum size of the integer stack

type
    TStringStack = class
        elements: TStringList;
        constructor Create;
        destructor Destroy;
        procedure Push(s: String);
        function Pop: String;
        function Peek: String;
        function IsEmpty: Boolean;
        procedure Clear;
        procedure DumpContents;
    end;

    // Static stack. Can overflow...
    TIntegerStack = class
        elements: array[0..INT_MAX_SIZE] of Integer;
        count: Integer;
        constructor Create;
        procedure Push(i: Integer);
        function Pop: Integer;
        function Peek: Integer;
        function IsEmpty: Boolean;
        procedure Clear;
    end;

implementation

constructor TStringStack.Create;
begin
    elements:=TStringList.Create;
end;

destructor TStringStack.Destroy;
begin
    elements.Free;
end;

procedure TStringStack.Push(s: String);
begin
    elements.Add(s);
end;

function TStringStack.Pop: String;
begin
    Result:=elements.Strings[elements.Count-1];
    elements.Delete(elements.Count-1);
end;

function TStringStack.Peek: String;
begin
    Result:=elements.Strings[elements.Count-1];
end;

function TStringStack.IsEmpty: Boolean;
begin
    Result:=(elements.Count=0);
end;

procedure TStringStack.Clear;
begin
    elements.Clear;
end;

procedure TStringStack.DumpContents;
var i:Integer;
begin
    devLog.Trace('====== STACK CONTENTS: ======');
    for i:=0 to elements.Count-1 do devLog.Trace(elements.Strings[i]);
end;



{ ========= IntegerStack ========== }

constructor TIntegerStack.Create;
begin
    Clear;
end;

procedure TIntegerStack.Push(i: Integer);
begin
    if count> INT_MAX_SIZE then Raise Exception.Create('TIntegerStack Overflow. Please contact the author')
    else begin
        elements[count]:=i;
        Inc(count);
    end;
end;

function TIntegerStack.Pop: Integer;
begin
    Result:=elements[count-1];
    Dec(count);
end;

function TIntegerStack.Peek: Integer;
begin
    Result:=elements[count-1];
end;

function TIntegerStack.IsEmpty: Boolean;
begin
    Result:=(count=0);
end;

procedure TIntegerStack.Clear;
var i: Integer;
begin
    count:=0;
    for i:=0 to INT_MAX_SIZE do elements[i]:=0;
end;

end.