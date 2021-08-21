{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: MyTypes.pas,v 1.3 2004/03/31 23:27:34 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit MyTypes;

interface

type

    // Wrapper around a primitive boolean (for use in collections, etc)
    TBoolean = class(TObject)
    public 
        val: Boolean;
        constructor Create(b: Boolean);
    end;

    TIntArray = array of Integer;

    Float = Extended;


implementation

constructor TBoolean.Create(b: Boolean);
begin
    val:=b;
end;


end.

