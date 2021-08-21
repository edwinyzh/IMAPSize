{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: About.pas,v 1.10 2004/04/04 20:17:13 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit About;

// Remove . if this is a beta release
{.$DEFINE BETA}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, JDUrlLabel, ComCtrls;

type
  TFAbout = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel1: TPanel;
    Label1: TLabel;
    VersionLabel: TLabel;
    Label5: TLabel;
    JDUrlLabel2: TJDUrlLabel;
    JDUrlLabel3: TJDUrlLabel;
    Label4: TLabel;
    Button1: TButton;
    Panel2: TPanel;
    Label6: TLabel;
    JDUrlLabel1: TJDUrlLabel;
    Label7: TLabel;
    JDUrlLabel4: TJDUrlLabel;
    Label3: TLabel;
    JDUrlLabel6: TJDUrlLabel;
    Label8: TLabel;
    JDUrlLabel7: TJDUrlLabel;
    Label9: TLabel;
    JDUrlLabel8: TJDUrlLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    JDUrlLabel9: TJDUrlLabel;
    Label13: TLabel;
    JDUrlLabel10: TJDUrlLabel;
    Label14: TLabel;
    JDUrlLabel11: TJDUrlLabel;
    Label15: TLabel;
    JDUrlLabel12: TJDUrlLabel;
    Label16: TLabel;
    JDUrlLabel13: TJDUrlLabel;
    Label17: TLabel;
    JDUrlLabel14: TJDUrlLabel;
    Label18: TLabel;
    JDUrlLabel15: TJDUrlLabel;
    Label19: TLabel;
    Label21: TLabel;
    JDUrlLabel17: TJDUrlLabel;
    Label2: TLabel;
    JDUrlLabel5: TJDUrlLabel;
    JDUrlLabel16: TJDUrlLabel;
    Label20: TLabel;
    JDUrlLabel18: TJDUrlLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  function getVersion : string;

var
  FAbout: TFAbout;

implementation

{$R *.DFM}



procedure TFAbout.Button1Click(Sender: TObject);
begin
    Close;
end;

procedure TFAbout.FormCreate(Sender: TObject);
begin
    // Set version info and align
    VersionLabel.Caption := 'Version '+getVersion;
    //VersionLabel.Left:= (Panel1.Width-VersionLabel.Width) div 2;
    {$IFDEF BETA}
    VersionLabel.Caption := VersionLabel.Caption + ' Beta';
    {$ENDIF}
end;

procedure TFAbout.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if Key=VK_ESCAPE then Close;
end;

function getVersion : string;
{ ---------------------------------------------------------
   Extracts the FileVersion element of the VERSIONINFO
   structure that Delphi maintains as part of a project's
   options.

   Results are returned as a standard string.  Failure
   is reported as "".

   Note that this implementation was derived from similar
   code used by Delphi to validate ComCtl32.dll.  For
   details, see COMCTRLS.PAS, line 3541.

   Taken from: http://www.techtricks.com/delphi/getversion.php
  -------------------------------------------------------- }
const
   NOVIDATA = '';

var
  dwInfoSize,           // Size of VERSIONINFO structure
  dwVerSize,            // Size of Version Info Data
  dwWnd: DWORD;         // Handle for the size call.
  FI: PVSFixedFileInfo; // Delphi structure; see WINDOWS.PAS
  ptrVerBuf: Pointer;   // pointer to a version buffer
  strFileName,          // Name of the file to check
  strVersion : string;  // Holds parsed version number
begin

   strFileName := paramStr( 0 );
   dwInfoSize :=
      getFileVersionInfoSize( pChar( strFileName ), dwWnd);

   if ( dwInfoSize = 0 ) then
      result := NOVIDATA
   else
   begin

      getMem( ptrVerBuf, dwInfoSize );
      try

         if getFileVersionInfo( pChar( strFileName ),
            dwWnd, dwInfoSize, ptrVerBuf ) then

            if verQueryValue( ptrVerBuf, '\',
                              pointer(FI), dwVerSize ) then

            strVersion :=
               format( '%d.%d.%d',
                       [ hiWord( FI.dwFileVersionMS ),
                         loWord( FI.dwFileVersionMS ),
                         hiWord( FI.dwFileVersionLS ) ] );


      finally
        freeMem( ptrVerBuf );
      end;
    end;
  Result := strVersion;
end;

end.
