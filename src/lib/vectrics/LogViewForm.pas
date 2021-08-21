unit LogViewForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, Logging, LogCtrls;

// CLASS: TVLogViewForm
// DESCRIPTION:
//      A form which will allow the user to instantly change to logging level
//      of any logger within an application.  This form leverages existing
//      Vectric visual components to display logging information saved by a
//      TVLogPersistence component.


type
  TVLogViewForm = class(TForm)
    MainPanel: TPanel;
    ControlPanel: TPanel;
    TopPanel: TPanel;
    Splitter: TSplitter;
    ListPanel: TPanel;
    RightPanel: TPanel;
    StatusBar1: TStatusBar;
    DetailPanel: TPanel;
    Panel1: TPanel;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Log : TVLogger;
    LogListView : TVLogListView;
    EventDetailDisplay : TVEventDetailDisplay;
    LogListControl : TVLogListControl;
    fPersistence: TVLogPersistence;
  public
    constructor Create(owner: TComponent); override;
    constructor CreateForm(owner: TComponent; persistence: TVLogPersistence); overload; virtual;
  end;

implementation

{$R *.dfm}

constructor TVLogViewForm.Create(owner: TComponent);
begin
    raise TVException.Create('For TVLogViewForm, Do not call constructor with just owner as a parameter, use CreateForm constructor instead');
end;

// Constructor - must have a TVLogPersistence instance passed.  The log list view
// will show the events from this persistence object.
constructor TVLogViewForm.CreateForm(owner: TComponent; persistence: TVLogPersistence);
begin
    inherited Create(owner);
    Log := TVLogger.GetInstance('vectrics.logging.TVLogListViewForm');
    fPersistence := persistence;
end;


procedure TVLogViewForm.FormResize(Sender: TObject);
begin
end;


// This procedure handels the OnCanResize event of the splitter.
// For now, always accept.
procedure TVLogViewForm.FormCreate(Sender: TObject);
begin
    Log.Debug('FormCreate() called - Creating LogListView component');

    Log.Debug('FormCreate()  - Creating the list view that will show the events.');
    LogListView := TVLogListView.Create(ListPanel);
    LogListView.Parent := ListPanel;
    LogListView.SetPersistence(fPersistence);
    LogListView.Left := 2;
    LogListView.Width := ListPanel.Width - 4;
    LogListView.Top := 2;
    LogListView.Height := ListPanel.Height - 4;
    LogListView.NumberOfRows := 12;
    LogListView.Visible := true;
    LogListView.GetPersistence().MaxSize := 5000;
    LogListView.Align := alClient;


    Log.Debug('FormCreate() - Creating EventDetailDisplay component');
    EventDetailDisplay := TVEventDetailDisplay.Create(DetailPanel);
    EventDetailDisplay.Parent := DetailPanel;
    EventDetailDisplay.Left := 2;
    EventDetailDisplay.Width := DetailPanel.Width - 4;
    EventDetailDisplay.Top := 2;
    EventDetailDisplay.Height := DetailPanel.Height - 4;
    EventDetailDisplay.Visible := true;
    EventDetailDisplay.Align := alClient;


    Log.Debug('FormCreate() called - Creating LogListControl component');
    LogListControl := TVLogListControl.Create(ControlPanel);
    LogListControl.Parent := ControlPanel;
    LogListControl.Left := 2;
    LogListControl.Width := ControlPanel.Width - 4;
    LogListControl.Top := 2;
    LogListControl.Height := ControlPanel.Height - 4;
    LogListControl.Visible := true;
    LogListControl.Align := alClient;

    Log.Debug('FormCreate() finished');
end;


end.
