unit LevelControlForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, LogCtrls;


// CLASS: TVLogViewForm
// DESCRIPTION:
//      A form which will allow the user to instantly change to logging level
//      of any logger within an application.  This form leverages Vectric's
//      TVLogLevelControl component.


// 2003 Vectrics, Inc. (tm) - Sample Code License Agreement
// Vectrics provides this source code with the user accepting all liability
// for situations where this source code is used within the user's applications.
// The user is free to modify and incorporate this code within any software
// application.
// Vectrics accepts no responsibility for outcomes resulting from building or
// running applications incorporating this code or application using modifications
// of this code.
// Usage of this code indicates acceptance of this license agreement.



type
  TVLevelControlForm = class(TForm)
    LevelControlPanel: TPanel;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    LevelControl : TVLogLevelControl;
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}


// Layout the visual components needed
procedure TVLevelControlForm.FormCreate(Sender: TObject);
begin
    LevelControl := TVLogLevelControl.Create(LevelControlPanel);
    LevelControl.Parent := LevelControlPanel;
    LevelControl.Left := 2;
    LevelControl.Width := LevelControlPanel.Width - 4;
    LevelControl.Top := 2;
    LevelControl.Height := LevelControlPanel.Height - 4;
    LevelControl.Visible := true;
    LevelControl.Align := alClient;
end;

end.
