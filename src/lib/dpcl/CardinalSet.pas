unit CardinalSet;

interface

uses SysUtils, Classes;

const
  BLOCK_BITS = 10;
  BLOCK_CAPACITY = 1 shl BLOCK_BITS;
  BLOCK_SIZE = SizeOf(Pointer) + (1 shl (BLOCK_BITS - 3));
  MAX_BITS = 16;
  MAX_CAPACITY = 1 shl MAX_BITS;

type
  TIteratorProc = function(Element: Cardinal): Boolean of object;

  TCardinalSet = class(TPersistent)
  private
    FBitString: Pointer;
    FCapacity: Integer;
    FComplemented: LongInt;
    FBlock: Pointer;
    FBlockFirst: Integer;
    FBlockLeft: Integer;
    FCurrentDWord: Longword;
    function GetCount: Integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Error;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetBit(Index: Cardinal; Value: Boolean);
  public
    destructor Destroy; override;
    procedure Insert(Element: Cardinal);
    procedure Remove(Element: Cardinal);
    function Belongs(Element: Cardinal): Boolean;
    procedure Clear;
    procedure Pack;
    procedure Complement;
    procedure Intersection(Other: TCardinalSet);
    procedure Union(Other: TCardinalSet);
    procedure Subtract(Other: TCardinalSet);
    function IsEqualTo(Other: TCardinalSet): Boolean;
    function IsSubsetOf(Other: TCardinalSet): Boolean;
    function IsProperSubsetOf(Other: TCardinalSet): Boolean;
    function First: Cardinal;
    function Next: Cardinal;
    property Count: Integer read GetCount;
  end;

  ECardinalSetError = class(Exception);

implementation

destructor TCardinalSet.Destroy;
begin
  SetCapacity(0);
  inherited Destroy;
end;

{ Count the number of elements in set bit by bit }

function TCardinalSet.GetCount: Integer; assembler;
asm
        TEST    [EAX].FComplemented,1   //If the set is complemented
        JZ      @@NCmp                  //-1 is returned (denotes infinity)
        MOV     EAX,-1
        RET

@@NCmp: MOV     EDX,[EAX].FBitString
        OR      EDX,EDX
        JNZ     @@1
        MOV     EAX,0
        RET

@@1:    PUSH    EDI
        PUSH    EBX
        MOV     EBX,0

@@Loop: PUSH    EDX
        MOV     EDI,EDX
        ADD     EDI,TYPE Pointer
        MOV     ECX,(1 shl (BLOCK_BITS - 5))
        SUB     EAX,EAX
@@Scan: REPE    SCASD
        JNZ     @@Count

        POP     EDX
        MOV     EDX,[EDX]
        OR      EDX,EDX
        JNZ     @@Loop

        MOV     EAX,EBX
        POP     EBX
        POP     EDI
        RET

@@Count:MOV     EAX,[EDI-4]
@@2:    BSF     DX,EAX
        JZ      @@Scan
        BTR     EAX,DX
        INC     EBX
        JMP     @@2
end;

{ Copy the contents of this other set into other set }

procedure TCardinalSet.AssignTo(Dest: TPersistent); assembler;
asm
        MOV     ECX,[EAX].FComplemented
        MOV     [EDX].FComplemented,ECX

        PUSH    EAX
        PUSH    EDX
        XCHG    EAX,EDX
        MOV     EDX,[EDX].TCardinalSet.FCapacity
        CALL    TCardinalSet.SetCapacity
        POP     EDX
        POP     EAX

        MOV     EAX,[EAX].TCardinalSet.FBitString
        OR      EAX,EAX
        JZ      @@Exit
        MOV     EDX,[EDX].FBitString
        PUSH    EDI
        PUSH    ESI

@@Loop: MOV     ESI,EAX
        ADD     ESI,TYPE Pointer
        MOV     EDI,EDX
        ADD     EDI,TYPE Pointer
        MOV     ECX,(1 shl (BLOCK_BITS - 5))
        REP     MOVSD

        MOV     EDX,[EDX]
        MOV     EAX,[EAX]
        OR      EAX,EAX
        JNZ     @@Loop

        POP     ESI
        POP     EDI
@@Exit:
end;


{ In-class error handler }

procedure TCardinalSet.Error;
begin
  raise ECardinalSetError.Create('Element out of bounds');
end;

{ Expand or shrink the set }

procedure TCardinalSet.SetCapacity(NewCapacity: Integer);
var block, nextblock, prevblock: Pointer;
    fillbyte: Byte;
begin
  NewCapacity := (NewCapacity + BLOCK_CAPACITY - 1) and
    (not (BLOCK_CAPACITY - 1));
  if NewCapacity > MAX_CAPACITY then Error;
  if NewCapacity <> FCapacity then
  begin
    FCapacity := NewCapacity;
    prevblock := @FBitString;
    block := FBitString;
    while (block <> nil) and (NewCapacity > 0) do
    begin
      Dec(NewCapacity, BLOCK_CAPACITY);
      prevblock := block;
      block := Pointer(block^);
    end;
    if NewCapacity = 0 then
    begin
      Pointer(prevblock^) := nil;
      while block <> nil do
      begin
        nextblock := Pointer(block^);
        FreeMem(block, BLOCK_SIZE);
        block := nextblock;
      end
    end else
      while NewCapacity > 0 do
      begin
        Dec(NewCapacity, BLOCK_CAPACITY);
        GetMem(block, BLOCK_SIZE);
        if FComplemented <> 0 then fillbyte := $FF
        else fillbyte := 0;
        FillChar(block^, BLOCK_SIZE, fillbyte);
        Pointer(block^) := nil;
        Pointer(prevblock^) := block;
        prevblock := block;
      end;
  end;
end;

{ Set or reset designated bit in a set }

procedure TCardinalSet.SetBit(Index: Cardinal; Value: Boolean); assembler;
asm
      { EAX = Self
        EDX = Index
        CL  = Value }

        CMP     Index,[EAX].FCapacity
        JB      @@1

        PUSH    Self
        PUSH    Index
        PUSH    ECX {Value}
        INC     Index
        CALL    TCardinalSet.SetCapacity
        POP     ECX {Value}
        POP     Index
        POP     Self

@@1:    MOV     EAX,[EAX].FBitString
@@2:    TEST    Index,(not (BLOCK_CAPACITY - 1))
        JZ      @@3
        MOV     EAX,[EAX]
        SUB     Index,BLOCK_CAPACITY
        JMP     @@2
@@3:    ADD     EAX,TYPE Pointer
        OR      Value,Value
        JZ      @@4
        BTS     [EAX],Index
        RET

@@4:    BTR     [EAX],Index
        RET
end;

{ Insertion and deletion }

procedure TCardinalSet.Insert(Element: Cardinal); assembler;
asm
        MOV     CL,1
        CALL    TCardinalSet.SetBit
end;

procedure TCardinalSet.Remove(Element: Cardinal);assembler;
asm
        MOV     CL,0
        CALL    TCardinalSet.SetBit
end;

{ Test if set contains certain element (bit is set) }

function TCardinalSet.Belongs(Element: Cardinal): Boolean; assembler;
asm
        CMP     Element,MAX_CAPACITY
        JAE     TCardinalSet.Error
        CMP     Element,[EAX].FCapacity
        JB      @@1
        MOV     EAX,[EAX].FComplemented
        AND     EAX,1
        RET

@@1:    MOV     EAX,[EAX].FBitString
@@2:    TEST    Element,(not (BLOCK_CAPACITY - 1))
        JZ      @@3
        MOV     EAX,[EAX]
        SUB     Element,BLOCK_CAPACITY
        JMP     @@2

@@3:    ADD     EAX,TYPE Pointer
        BT      [EAX],Element
        SETC    AL
end;

{ Set capacity to zero }

procedure TCardinalSet.Clear;
begin
  SetCapacity(0);
end;

{ Deallocate unused blocks at the end of set }

procedure TCardinalSet.Pack; assembler;
asm
        PUSH    EDI
        PUSH    EAX
        MOV     EDX,[EAX].FCapacity
        ADD     EDX,BLOCK_CAPACITY
        MOV     ECX,[EAX].FBitString
        MOV     EAX,[EAX].FComplemented

@@1:    OR      ECX,ECX
        JZ      @@Loop
        PUSH    ECX
        MOV     ECX,[ECX]
        JMP     @@1

@@Loop: SUB     EDX,BLOCK_CAPACITY
        JZ      @@Cap
        POP     EDI
        ADD     EDI,TYPE Pointer
        MOV     ECX,(1 shl (BLOCK_BITS - 5))
@@Scan: REPE    SCASD
        JZ      @@Loop

@@2:    MOV     ECX,EDX
@@3:    SUB     ECX,BLOCK_CAPACITY
        JZ      @@Cap
        POP     EDI
        JMP     @@3

@@Cap:  POP     EAX
        POP     EDI
        CMP     EDX,[EAX].FCapacity
        JAE     @@Exit
        CALL    TCardinalSet.SetCapacity
@@Exit:
end;

{ Complement each bit in the set and the FComplemented field }

procedure TCardinalSet.Complement; assembler;
asm
        NOT     [EAX].FComplemented
        MOV     EDX,[EAX].FBitString
        OR      EDX,EDX
        JZ      @@Exit

@@Loop: PUSH    ESI
        MOV     ESI,EDX
        ADD     ESI,TYPE Pointer
        MOV     ECX,(1 shl (BLOCK_BITS - 5))
@@1:    LODSD
        NOT     EAX
        MOV     [ESI-4],EAX
        LOOP    @@1
        MOV     EDX,[EDX]
        OR      EDX,EDX
        JNZ     @@Loop
        POP     ESI
@@Exit:
end;

{ Private subroutine which compares the capacities of two sets and makes the
  set with smaller capacity as large as the set with bigger capacity. The sets
  are specified in EAX and EDX. If both sets have zero capacity the carry flag
  is set, otherwise carry is cleared. Subroutine preserves EAX and EDX regist-
  ers but NOT the ECX register }

procedure MakeEqualLength(CSet1, CSet2: TCardinalSet); assembler;
asm
        MOV     ECX,[CSet1].FCapacity
        CMP     ECX,[CSet2].FCapacity
        JE      @@Same

        PUSH    EAX
        PUSH    EDX
        JA      @@Exp2
        MOV     EDX,[CSet2].FCapacity
        JMP     @@Set
@@Exp2: MOV     EAX,EDX
        MOV     EDX,ECX

@@Set:  CALL    TCardinalSet.SetCapacity
        POP     EDX
        POP     EAX
@@Exit: STC
        RET

@@Same: OR      ECX,ECX
        JNZ     @@Exit
        CLC
end;

{ Binary set oparations union, intersection and subtraction }

procedure TCardinalSet.Intersection(Other: TCardinalSet); assembler;
asm
        MOV     ECX,[Other].FComplemented
        AND     [EAX].FComplemented,ECX
        CALL    MakeEqualLength
        JC      @@1
        RET

@@1:    PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     EBX,[EAX].FBitString
        MOV     EDX,[Other].FBitString

@@Loop: MOV     ESI,EDX
        ADD     ESI,TYPE Pointer
        MOV     EDI,EBX
        ADD     EDI,TYPE Pointer
        MOV     ECX,(1 shl (BLOCK_BITS - 5))
@@2:    LODSD
        AND     EAX,[EDI]
        STOSD
        LOOP    @@2
        MOV     EDX,[EDX]
        MOV     EBX,[EBX]
        OR      EDX,EDX
        JNZ     @@Loop

        POP     EBX
        POP     ESI
        POP     EDI
end;

procedure TCardinalSet.Union(Other: TCardinalSet); assembler;
asm
        MOV     ECX,[Other].FComplemented
        OR      [EAX].FComplemented,ECX
        CALL    MakeEqualLength
        JC      @@1
        RET

@@1:    PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     EBX,[EAX].FBitString
        MOV     EDX,[Other].FBitString

@@Loop: MOV     ESI,EDX
        ADD     ESI,TYPE Pointer
        MOV     EDI,EBX
        ADD     EDI,TYPE Pointer
        MOV     ECX,(1 shl (BLOCK_BITS - 5))
@@2:    LODSD
        OR      EAX,[EDI]
        STOSD
        LOOP    @@2
        MOV     EDX,[EDX]
        MOV     EBX,[EBX]
        OR      EDX,EDX
        JNZ     @@Loop

        POP     EBX
        POP     ESI
        POP     EDI
end;

procedure TCardinalSet.Subtract(Other: TCardinalSet); assembler;
asm
        MOV     ECX,[Other].FComplemented
        NOT     ECX
        AND     [EAX].FComplemented,ECX
        CALL    MakeEqualLength
        JC      @@1
        RET

@@1:    PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     EBX,[EAX].FBitString
        MOV     EDX,[Other].FBitString

@@Loop: MOV     ESI,EDX
        ADD     ESI,TYPE Pointer
        MOV     EDI,EBX
        ADD     EDI,TYPE Pointer
        MOV     ECX,(1 shl (BLOCK_BITS - 5))
@@2:    LODSD
        NOT     EAX
        AND     EAX,[EDI]
        STOSD
        LOOP    @@2
        MOV     EDX,[EDX]
        MOV     EBX,[EBX]
        OR      EDX,EDX
        JNZ     @@Loop

        POP     EBX
        POP     ESI
        POP     EDI
end;

{ Binary test functions }

function TCardinalSet.IsEqualTo(Other: TCardinalSet): Boolean; assembler;
asm
        MOV     ECX,[EAX].FComplemented
        CMP     ECX,[Other].FComplemented
        JE      @@1
        SUB     AL,AL
        RET

@@1:    CALL    MakeEqualLength
        JC      @@2
        MOV     AL,1
        RET

@@2:    PUSH    EDI
        PUSH    ESI
        MOV     EAX,[EAX].FBitString
        MOV     EDX,[Other].FBitString

@@Loop: MOV     ESI,EAX
        ADD     ESI,TYPE Pointer
        MOV     EDI,EDX
        ADD     EDI,TYPE Pointer
        MOV     ECX,(1 shl (BLOCK_BITS - 5))
        REPE    CMPSD
        JZ      @@3
        SUB     AL,AL
        JMP     @@Exit

@@3:    MOV     EDX,[EDX]
        MOV     EAX,[EAX]
        OR      EAX,EAX
        JNZ     @@Loop
        MOV     AL,1

@@Exit: POP     ESI
        POP     EDI
        RET
end;

function TCardinalSet.IsSubsetOf(Other: TCardinalSet): Boolean; assembler;
asm
        MOV     ECX,[Other].FComplemented
        NOT     ECX
        AND     ECX,[EAX].FComplemented
        JZ      @@1
        SUB     AL,AL
        RET

@@1:    CALL    MakeEqualLength
        JC      @@2
        MOV     AL,1
        RET

@@2:    PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     EBX,[EAX].FBitString
        MOV     EDX,[Other].FBitString

@@Loop: MOV     ESI,EDX
        MOV     EDI,EBX
        MOV     ECX,(1 shl (BLOCK_BITS - 5))
@@Scan: MOV     EAX,[ESI+ECX*4]
        NOT     EAX
        AND     EAX,[EDI+ECX*4]
        LOOPZ   @@Scan
        JZ      @@3
        SUB     AL,AL
        JMP     @@Exit

@@3:    MOV     EDX,[EDX]
        MOV     EBX,[EBX]
        OR      EBX,EBX
        JNZ     @@Loop
        MOV     AL,1

@@Exit: POP     EBX
        POP     ESI
        POP     EDI
        RET
end;


function TCardinalSet.IsProperSubsetOf(Other: TCardinalSet): Boolean;
begin
  Result := IsSubsetOf(Other) and not IsEqualTo(Other);
end;


{ Iterator functions }

function TCardinalSet.First: Cardinal;
begin
  FBlock := FBitString;
  FBlockFirst := 0;
  FBlockLeft := (1 shl (BLOCK_BITS - 5));
  FCurrentDWord := 0;
  Result := Next;
end;

function TCardinalSet.Next: Cardinal; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX

        MOV     ESI,EAX
        MOV     EBX,[EAX].FBlock
        OR      EBX,EBX
        JZ      @@Out
        MOV     ECX,[EAX].FBlockLeft
        MOV     EAX,[EAX].FCurrentDWord

@@Bits: BSF     EDX,EAX
        JNZ     @@Found

@@Loop: MOV     EDI,ECX
        NEG     EDI
        AND     EDI,(1 shl (BLOCK_BITS - 5)) - 1
        ADD     EDI,EBX
        ADD     EDI,TYPE Pointer
@@Scan: REPE    SCASD
        JE      @@Next

        MOV     EAX,[EDI-4]
        JMP     @@Bits

@@Next: MOV     ECX,(1 shl (BLOCK_BITS - 5))
        ADD     [ESI].FBlockFirst,BLOCK_CAPACITY
        MOV     EBX,[EBX]
        OR      EBX,EBX
        JNZ     @@Loop
        MOV     [ESI].FBlock,EBX

@@Out:  MOV     EAX,-1
        TEST    [ESI].FComplemented,1
        JZ      @@Exit
        MOV     ECX,[ESI].FBlockFirst
        INC     ECX
        JZ      @@Exit
        MOV     [ESI].FBlockFirst,ECX
        ADD     EAX,ECX
        JMP     @@Exit

@@Found:BTR     EAX,EDX
        MOV     [ESI].FCurrentDWord,EAX
        MOV     [ESI].FBlockLeft,ECX
        MOV     [ESI].FBlock,EBX
        MOV     EAX,[ESI].FBlockFirst
        NOT     ECX
        AND     ECX,(1 shl (BLOCK_BITS - 5)) - 1
        SHL     ECX,5
        ADD     EDX,ECX
        ADD     EAX,EDX

@@Exit: POP     EBX
        POP     EDI
        POP     ESI
end;

end.
