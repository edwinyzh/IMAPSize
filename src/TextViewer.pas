unit TextViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  HTMLLite, StdCtrls, ComCtrls;

const
    TEXT_PAGE_IDX=0;  
    HTML_PAGE_IDX=1;

type
  TFTextViewer = class(TForm)
    PageControl1: TPageControl;
    TabText: TTabSheet;
    TabHTML: TTabSheet;
    Memo1: TMemo;
    htmlLite1: ThtmlLite;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    procedure SetCaption(title: String);
    procedure SetWordWrap(enable: Boolean);
  end;

var
  FTextViewer: TFTextViewer;

implementation

{$R *.DFM}

procedure TFTextViewer.SetCaption(title: String);
begin
    Caption:=title;
end;

procedure TFTextViewer.SetWordWrap(enable: Boolean);
begin
    Memo1.WordWrap:=enable;
end;

procedure TFTextViewer.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
    Action := caFree;
end;

end.
