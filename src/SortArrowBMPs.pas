{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: SortArrowBMPs.pas,v 1.4 2004/03/31 23:27:32 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit SortArrowBMPs;

interface

uses graphics, classes;

type
    TSortArrowBMPs = class
    private
        mUpArrowBMP, mDownArrowBMP: TBitmap;
        procedure CreateSortBmps(var UpBmp, DownBmp: TBitmap; Font: TFont);
    public
        constructor Create(Font: TFont);
        destructor Destroy; override;
    published
        property UpArrow: TBitmap read mUpArrowBMP;
        property DownArrow: TBitmap read mDownArrowBMP;
    end;

implementation


constructor TSortArrowBMPs.Create(Font: TFont);
begin
    CreateSortBmps(mUpArrowBMP, mDownArrowBMP, Font);
end;

destructor TSortArrowBMPs.Destroy;
begin
    if mUpArrowBMP <> nil then mUpArrowBMP.Free;
    if mDownArrowBMP <> nil then mDownArrowBMP.Free;
end;

procedure TSortArrowBMPs.CreateSortBmps(var UpBmp, DownBmp: TBitmap; Font: TFont);
var
  HeaderHeight: integer;
  MidPoint: integer;
begin
  if UpBmp = NIL then
    UpBmp := TBitmap.Create;
  if DownBmp = NIL then
    DownBmp := TBitmap.Create;

  UpBmp.Canvas.Font.Assign(Font);
  HeaderHeight := UpBmp.Canvas.TextHeight('Wy') - 6;
  if HeaderHeight > 0 then
  begin
    if Odd(HeaderHeight) then
      Inc(HeaderHeight);
    UpBmp.Width := HeaderHeight;
    UpBmp.Height := HeaderHeight;
    DownBmp.Width := HeaderHeight;
    DownBmp.Height := HeaderHeight;
    MidPoint := HeaderHeight div 2;

    { Don't ask about the drawing.  I just fooled around until I got
      something I liked. }
    with UpBmp.Canvas do
    begin
      Brush.Color := clBtnFace;
      FillRect(Rect(0, 0, HeaderHeight, HeaderHeight));
      Pen.Color := clBtnShadow;
      MoveTo(MidPoint, HeaderHeight-2);
      LineTo(HeaderHeight-1, 0);
      Pixels[HeaderHeight-1, 0] := Pen.Color;
      Pen.Color := clBtnHighlight;
      MoveTo(HeaderHeight-2, 0);
      LineTo(0, 0);
      LineTo(MidPoint-1, HeaderHeight-2);
      Pixels[MidPoint-1, HeaderHeight-2] := Pen.Color;
    end;

    with DownBmp.Canvas do
    begin
      Brush.Color := clBtnFace;
      FillRect(Rect(0, 0, HeaderHeight, HeaderHeight));
      Pen.Color := clBtnHighlight;
      MoveTo(0, HeaderHeight-1);
      LineTo(MidPoint-1, 0);
      Pen.Color := clBtnShadow;
      MoveTo(MidPoint, 0);
      LineTo(HeaderHeight-1, HeaderHeight-1);
      LineTo(-1, HeaderHeight-1);
      Pixels[MidPoint, 0] := clBtnFace;
    end;
  end;
end;


end.
 