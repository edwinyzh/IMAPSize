unit middletestu;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,middlex;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button2: TButton;
    Memo3: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var L:TList; i,max:integer; s:STring;
begin
{
  L:=BuildLeaves(memo1.text);
  max:=L.count-1;
  memo2.lines.clear;
  for i:= 0 to max do
   begin
    if TParseLeaf(L[i]) is TTagLeaf then
       s:= 'TAG:'+TTagLeaf(L[i]).tag
       else
       s:= 'TEXT:'+TBodyLeaf(L[i]).body;
    memo2.lines.add(inttostr(i)+':'+s);
   end;}
end;

procedure TForm1.Button2Click(Sender: TObject);
var Mid:TMiddleXNode;
begin
 Mid:=TMiddleXNode.create;
 Mid.xml:=memo1.Lines.text;
 memo3.Text := Mid.xml;
end;

end.
