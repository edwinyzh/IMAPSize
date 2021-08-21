{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: ListNodes.pas,v 1.8 2004/03/31 23:27:36 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit ListNodes;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, ExtCtrls, Nodes, ComCtrls, EnhListView;

type
  TFListNodes = class(TForm)
    PanelPozadina: TPanel;
    Panel1: TPanel;
    Button1: TButton;
    ListView1: TListView;
    procedure FormActivate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    mPathFilter: String;
  public
    { Public declarations }
    procedure SetPathFilter(pathFilter: String);
  end;

var
  FListNodes: TFListNodes;

implementation

uses Main;

{$R *.DFM}


procedure TFListNodes.FormCreate(Sender: TObject);
begin
    mPathFilter:='';
end;

procedure TFListNodes.FormActivate(Sender: TObject);
var i: Integer;
    ln, pathLength: Integer;
    substr: String;
    filteredCnt: Integer;
    filteredList: TListOfNodes;
begin
    ln:=Length(mboxTree.ListOfNodes);
    ListView1.Items.Clear;
    if mPathFilter='' then begin
        for i:=ln-1 downto 0 do begin
            with ListView1.Items.Add do begin
                Caption:=mboxTree.ListOfNodes[i].mFullNodeName;
                SubItems.Add(IntToStr(mboxTree.ListOfNodes[i].mSize));
            end;
        end;
    end
    else begin
        filteredCnt:=0;
        SetLength(filteredList,ln);
        pathLength:=Length(mPathFilter);
        for i:= ln-1 downto 0 do begin
            substr:=Copy(mboxTree.ListOfNodes[i].mFullNodeName,0,pathLength);
            if CompareStr(substr,mPathFilter)=0 then begin
               filteredList[filteredCnt]:=mboxTree.ListOfNodes[i];
               Inc(filteredCnt);
            end;
        end;
        for i:=0 to filteredCnt-1 do begin
            with ListView1.Items.Add do begin
                Caption:=filteredList[i].mFullNodeName;
                SubItems.Add(IntToStr(filteredList[i].mSize));
            end;
        end;
    end;
    SetLength(filteredList,0);
end;

procedure TFListNodes.Button1Click(Sender: TObject);
begin
    Close;
end;

procedure TFListNodes.FormResize(Sender: TObject);
begin
    //StringGrid1.ColWidths[1]:=100;
    //StringGrid1.ColWidths[0]:=StringGrid1.Width-StringGrid1.ColWidths[1];
    Button1.Left:=(Panel1.Width-Button1.Width) div 2;
end;

procedure TFListNodes.SetPathFilter(pathFilter: String);
begin
    mPathFilter:=pathFilter;
end;

procedure TFListNodes.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    if FListNodes.WindowState = wsNormal then begin
        settings.Positions.LDTop:=FListNodes.Top;
        settings.Positions.LDLeft:=FListNodes.Left;
        settings.Positions.LDWidth:=FListNodes.Width;
        settings.Positions.LDHeight:=FListNodes.Height;
        settings.Positions.LDMaximized:=false;
    end
    else if FListNodes.WindowState=wsMaximized then begin
        settings.Positions.LDMaximized:=true;
    end;
end;

procedure TFListNodes.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if Key=VK_ESCAPE then Close;
end;

end.