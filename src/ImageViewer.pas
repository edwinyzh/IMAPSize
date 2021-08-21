unit ImageViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GIFImage, ExtCtrls, ComCtrls;

const
    JPG_PAGE_IDX=0;  // also bmp,wmf,etc (see TImage)
    GIF_PAGE_IDX=1;

type
  TFImageViewer = class(TForm)
    PageControl1: TPageControl;
    TabJPG: TTabSheet;
    TabGIF: TTabSheet;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    ScrollBox2: TScrollBox;
    GIFImage1: TGIFImage;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FImageViewer: TFImageViewer;

implementation

{$R *.DFM}

procedure TFImageViewer.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
    Action := caFree;
end;

end.