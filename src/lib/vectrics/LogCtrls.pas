{

    Vectrics Logging - distributed logging for Java
    Copyright (C) 2003 -  Mike Moore

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

}

unit LogCtrls;

{$ObjExportAll On}

interface

uses
  SysUtils, Classes, Controls, ComCtrls, Forms, Logging, SyncObjs,
  Messages, Windows, CommCtrl,
  Menus, Graphics, StdCtrls, RichEdit, ToolWin, ImgList, ExtCtrls, //ListActns,
  PageList, Buttons;
type
  TVEventDetailDisplay = class;
  TVLogListControl = class;



  TVLogListView = class(TListView)
  private
    //Log: TVLogger;
    fSettingNumberOfRows : boolean;
    fActive : boolean;
    fWriteLevel: TVLevelValue;
    fDisplayLevel: TVLevelValue;
    fAutoRowCount : boolean;
    fFilter: TVFilter;
    fDefaultPersistence : TVLogPersistence;
    fDirty : boolean;
    fDateFormat : String;
    fPageStartIndex : Integer;
    fLastStartIndex : Integer;
    fLastEndIndex : Integer;
    fEventDetailFrame : TVEventDetailDisplay;
    fPersistence : TVLogPersistence;
    fMaxLines : Integer;
    fFreeCount : Integer;
    fMutex : TCriticalSection;
    fBufferMutex : TCriticalSection;
    fBufferedEventList : TList;
    fSuspended : boolean;
    fNumberOfRows: Integer;
    //fTrackCaptionWidth: Integer;

    fIndexColumn : TListColumn;
    fDateColumn : TListColumn;
    fSeverityColumn : TListColumn;
    fLoggerColumn : TListColumn;
    fMessageColumn : TListColumn;
    fListControl: TVLogListControl;
    function FindDetailFrameInBranch(parent: TComponent):  TVEventDetailDisplay;
  protected
    procedure ShowEvents(list: TVPageList);
    procedure ShowEvent(row: Integer; event: TVLoggingEvent);
    procedure  Change(item: TListItem; change: Integer); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy(); override;
    procedure SetPersistence(persistence: TVLogPersistence);
    function GetPersistence(): TVLogPersistence;
    procedure  Delete(item: TListItem); override;
    function GetListControl(): TVLogListControl;
    procedure SetListControl(listControl: TVLogListControl);
    procedure AddBufferedEvent(event: TVLoggingEvent);
    procedure SetDetailFrame(eventDetailFrame: TVEventDetailDisplay);
    function GetDetailFrame(): TVEventDetailDisplay;
    procedure Refresh();
    procedure  PreviousPage();
    procedure  NextPage();
    class function Encode(before: String): String;
    procedure SetSuspended(value: boolean);
    function  GetSuspended(): boolean;
    procedure SetDisplayLevel(level: TVLevelValue);
    function GetDisplayLevel(): TVLevelValue;
    function GetLevel(): TVLevelValue;
    procedure SetLevel(level: TVLevelValue);
    procedure SetPageStartIndex(index: Integer);
    function GetPageStartIndex(): Integer;
    function GetLastStartIndex(): INteger;
    function GetFollow(): boolean;
    procedure SetFollow(follow: boolean);
    procedure SetPageToPercent(percent: Integer);

    procedure SetNumberOfRows(rowCount: Integer);
    function GetNumberOfRows(): Integer;
    procedure SetFilter(filter: TVFilter);
    function GetFilter(): TVFilter;
    procedure Resize(); override;
  published
    property Suspended: boolean read GetSuspended write SetSuspended;
    property DisplayLevel: TVLevelValue read GetDisplayLevel write SetDisplayLevel;
    property NumberOfRows: Integer read GetNumberOfRows write SetNumberOfRows default 0;
    property DateFormat: String read fDateFormat write fDateFormat;
    property Filter: TVFilter read GetFilter write SetFilter;
    property Active: boolean read fActive write fActive;
    property AutoRowCount: boolean read fAutoRowCount write fAutoRowCount default true;
  end;


TVEventDetailDisplay = class(TPanel)
  private
    fShowNdc: boolean;
    fLoggingEvent: TVLoggingEvent;

    NdcLabel : TStaticText;
    NdcCaption : TStaticText;

    IndexCaption : TStaticText;
    IndexLabel : TStaticText;

    MessageMemo : TMemo;
    MessageCaption : TStaticText;

    TimeLabel : TStaticText;
    TimeCaption : TStaticText;

    SeverityLabel : TStaticText;
    SeverityCaption : TStaticText;

    LoggerLabel : TStaticText;
    LoggerCaption : TStaticText;

    procedure AddMessageToMemo(_msg: String);
  public
    constructor Create(Owner: TComponent); override;
    procedure SetLoggingEvent(event: TVLoggingEvent);
    procedure Resize(); override;
    procedure SetShowNdc(value: boolean);
    function GetShowNdc(): boolean;
  published
    property ShowNdc: boolean read GetShowNdc write SetShowNdc default false;
end;


TVLogListControl = class(TPanel)
  private
      Log : TVLogger;
    fCreateComplete: boolean;
    ListTimer : TTimer;
    fInitialized: boolean;
    fLogListView: TVLogListView;
    fFollowLatestCheckWidth: Integer;
    FollowLatestCheck : TCheckBox;
    LevelCombo : TComboBox;
//    fCurrentLogId: Integer;
    fRefreshInterval: Integer;
    fLevelComboWidth : Integer;
    fLevelCaptionWidth : Integer;
    fLevelPanelWidth : Integer;
    fSizeCaptionWidth : Integer;
    fSizeLabelWidth : Integer;

    LevelCaption : TStaticText;
    LevelPanel : TPanel;
    fTop : Integer;
    NextButton : TBitBtn;
    PreviousButton : TBitBtn;
    fPrevNextBtnWidth: Integer;
    fSpacing : Integer;
    fFollowLatestPanelWidth: Integer;
    PrevNextPanel : TPanel;
    fPrevNextBtnHeight: Integer;
    fPrevNextPanelWidth: Integer;
    fTopRowHeight : Integer;
    LogTrackBar: TTrackBar;
    fRefreshInProgress : boolean;
    fTrackWidth: Integer;
    fTrackCaptionWidth: Integer;

    LogTrackCaption: TStaticText;
    TrackPanel: TPanel;
    FollowLatestPanel: TPanel;
    TopPanel : TPanel;
    Row2Panel : TPanel;
    SizePanel : TPanel;
    SizeCaption : TStaticText;
    SizeLabel : TStaticText;
    function FindLogListViewInBranch(parent: TComponent): TVLogListView;
  protected
    procedure CreateWnd(); override;
    function GetTimer(): TTimer;
    procedure NextLogButtonClick(Sender: TObject); virtual;
    procedure PreviousLogButtonClick(Sender: TObject); virtual;
    procedure Resize; override;
    procedure LogTrackBarChange(Sender: TObject);

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy(); override;
    procedure FillLogListView();
    procedure SetLogListView(listView: TVLogListView);
    function GetLogListView(): TVLogListView;
    procedure LevelComboChange(Sender: TObject);
    procedure RefreshLevelCombo();
    procedure ListTimerTimer(Sender: TObject);
    procedure SetFollowLatest(value: boolean);
    function GetFollowLatest(): boolean;
    procedure FollowLatestCheckClick(Sender: TObject);

end;



TVLogLevelControl = class(TPanel)
public
  constructor Create(Owner: TComponent); override;
  procedure Update(); override;
  procedure RefreshAllNodes();
protected
  procedure Resize(); override;
  procedure CreateWnd(); override;
  procedure ShowLoggerButtonClick(Sender: TObject);

private
  DetailPanel : TPanel;
  RefreshButton : TButton;
  ShowLoggerButton : TButton;
  ShowLoggerEdit: TEdit;
  BranchInheritButton: TButton;
  InheritCheck: TCheckBox;
  LogLevelRadioGroup: TRadioGroup;
  //InheritedPriorityLabel: TStaticText;
  LoggerLabel : TStaticText;
  LogTreeView : TTreeView;
  LoggerCaption : TStaticText;
  fShowingSelectedLogger: boolean;
  fDetailPanelWidth : Integer;
  procedure ShowSelectedLoggerInfo();
  function GetSelectedLogger(): TVLogger;
  procedure InheritCheckClick(Sender: TObject);
  procedure  LogLevelRadioGroupClick(Sender: TObject);
  procedure  BranchInheritButtonClick(Sender: TObject);
  function FindTreeNodeForLogger(logger: TVLogger): TTreeNode;
  procedure RefreshNode(node: TTreeNode);
  function ShowTreeLoggerInfo(parentNode: TTreeNode; logger: TVLogger; depth: Integer): TTreeNode;
  procedure LogTreeViewChange(Sender: TObject; Node: TTreeNode);
  procedure RefreshButtonClick(Sender: TObject);

  function LevelToIndex(level: TVLevelValue): INteger;
  function IndexToLevel(index: Integer): TVLevelValue;
end;



procedure Register;

implementation



//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================
constructor TVLogLevelControl.Create(Owner: TComponent);
begin
    inherited;

    BevelOuter := bvNone;
    BevelInner := bvNone;
    BorderStyle := bsNone;

    fShowingSelectedLogger :=false;
    fDetailPanelWidth := 300;

    DetailPanel := TPanel.Create(self);
    DetailPanel.Parent := self;
    DetailPanel.BevelInner := bvNone;
    DetailPanel.BevelOuter := bvLowered;
    DetailPanel.Visible := true;
    DetailPanel.Caption := '';

    InheritCheck := TCheckBox.Create(self);
    InheritCheck.Parent := DetailPanel;
    InheritCheck.Caption := 'Inherit Level';
    InheritCheck.Left := 0;
    InheritCheck.Top := 200;
    InheritCheck.Width := 200;
    InheritCheck.Visible := true;
    InheritCheck.OnClick := InheritCheckClick;

    LogLevelRadioGroup := TRadioGroup.Create(self);
    LogLevelRadioGroup.Parent := DetailPanel;
    LogLevelRadioGroup.Enabled := false;
    LogLevelRadioGroup.Caption := 'Logger Level';
    LogLevelRadioGroup.Visible := true;

    LoggerCaption := TStaticText.Create(self);
    LoggerCaption.Parent := DetailPanel;
    LoggerCaption.BorderStyle := sbsNone;
    LoggerCaption.Caption := 'Logger';
    LoggerCaption.Visible := true;

    LoggerLabel := TStaticText.Create(self);
    LoggerLabel.Parent := DetailPanel;
    LoggerLabel.Visible := true;
    LoggerLabel.AutoSize := false;
    LoggerLabel.Width := 300;
    LoggerLabel.Height := 15;
    LoggerLabel.BorderStyle := sbsSunken;
    LoggerLabel.Caption := 'Inherit';
    LoggerLabel.Left := 80;

    LogTreeView := TTreeView.Create(self);
    LogTreeView.Parent := self;
    LogTreeView.Left := 2;
    LogTreeView.Top := 2;
    LogTreeView.Width := Width - 4;
    LogTreeView.Height := 200;
    LogTreeView.HideSelection := false;
    LogTreeView.Visible := true;
    LogTreeView.Images := TVLogManager.GetInstance().GetDisplayManager().GetLevelInhIcons();
    LogTreeView.OnChange := LogTreeViewChange;
    LogTreeView.ReadOnly := true;

    RefreshButton := TButton.Create(self);
    RefreshButton.Parent := DetailPanel;
    RefreshButton.Caption := 'Refresh';
    RefreshButton.Width := 90;
    RefreshButton.Height := 20;
    RefreshButton.Visible := true;
    RefreshButton.OnClick := RefreshButtonClick;

    BranchInheritButton := TButton.Create(self);
    BranchInheritButton.Parent := DetailPanel;
    BranchInheritButton.Caption := '&Branch Inherit';
    BranchInheritButton.Width := 90;
    BranchInheritButton.Height := 20;
    BranchInheritButton.Visible := true;
    BranchInheritButton.OnClick := BranchInheritButtonClick;

    ShowLoggerEdit := TEdit.Create(self);
    ShowLoggerEdit.Parent := DetailPanel;
    ShowLoggerEdit.Text := '';
    ShowLoggerEdit.Height := 18;

    ShowLoggerButton := TButton.Create(self);
    ShowLoggerButton.Parent := DetailPanel;
    ShowLoggerButton.Height := 20;
    ShowLoggerButton.Width := 90;
    ShowLoggerButton.Caption := 'Show Logger';
    ShowLoggerButton.OnClick := ShowLoggerButtonClick;

    Width := 500;
    Height := 400;
end;

procedure TVLogLevelControl.CreateWnd();
begin
  inherited;
  LogLevelRadioGroup.Items.Add('Trace');
  LogLevelRadioGroup.Items.Add('Debug');
  LogLevelRadioGroup.Items.Add('Info');
  LogLevelRadioGroup.Items.Add('Warn');
  LogLevelRadioGroup.Items.Add('Error');
  LogLevelRadioGroup.Items.Add('Fatal');
  Resize();
  Refresh();
  Caption := '';
end;

procedure TVLogLevelControl.Resize();
begin
  inherited;
  Caption := '';

  DetailPanel.Top := 2;
  DetailPanel.Height := Height - 4;
  DetailPanel.Width := fDetailPanelWidth;
  DetailPanel.Left := Width - (2 + DetailPanel.Width);

  LogTreeView.Top := 2;
  LogTreeView.Height := Height - 4;
  LogTreeView.Width := DetailPanel.Left - 4;

  LoggerCaption.Top := 3;
  LoggerCaption.Left := 4;

  LoggerLabel.Top := LoggerCaption.Top + LoggerCaption.Height;
  LoggerLabel.Left := LoggerCaption.Left;
  LoggerLabel.Width := DetailPanel.Width - (LoggerLabel.Left * 2);

  InheritCheck.Left := LoggerLabel.Left;
  InheritCheck.Top := LoggerLabel.Top + LoggerLabel.Height + 12;
  InheritCheck.Width := DetailPanel.Width - (LoggerLabel.Left * 2);

  LogLevelRadioGroup.Left := LoggerLabel.Left;
  LogLevelRadioGroup.Columns := 1;
  LogLevelRadioGroup.Top := InheritCheck.Top + InheritCheck.Height + 4;
  LogLevelRadioGroup.OnClick := LogLevelRadioGroupClick;
  LogLevelRadioGroup.Width := DetailPanel.Width - (LoggerLabel.Left * 2);
  LogLevelRadioGroup.Height := 100;


  BranchInheritButton.Left := LoggerLabel.Left;
  BranchInheritButton.Top := LogLevelRadioGroup.Top + LogLevelRadioGroup.Height + 8;

  ShowLoggerEdit.Left := BranchInheritButton.Left;
  ShowLoggerEdit.Top := BranchInheritButton.Top + BranchInheritButton.Height + 16;
  ShowLoggerEdit.Width := LogLevelRadioGroup.Width;

  ShowLoggerButton.Left := BranchInheritButton.Left;
  ShowLoggerButton.Top := ShowLoggerEdit.Top + ShowLoggerEdit.Height + 4;

  RefreshButton.Left := LoggerLabel.Left;
  RefreshButton.Top := DetailPanel.Height - (4 + RefreshButton.Height);

end;


procedure TVLogLevelControl.ShowSelectedLoggerInfo();
var
  logger: TVLogger;
  catName: String;
begin
    fShowingSelectedLogger := true;
    //int itemIndex = CategoriesCombo.ItemIndex;
    logger := GetSelectedLogger();
    if (logger = nil) then begin
        InheritCheck.Enabled := false;
        LogLevelRadioGroup.Enabled := false;
        LoggerLabel.Caption := '';
    end
    else begin
        catName := logger.GetCategory();
        InheritCheck.Enabled := true;
        InheritCheck.Checked := (logger.getLevel() = lvInherit);
        if ((catName = '') or (Length(catName) = 0)) then begin
            catName := 'root';
            InheritCheck.Checked := false;
            InheritCheck.Enabled := false;
        end;
        LoggerLabel.Caption := catName;

        LogLevelRadioGroup.Enabled := true;
        if (InheritCheck.Checked) then begin
            LogLevelRadioGroup.Enabled := false;
            LogLevelRadioGroup.ItemIndex := LevelToIndex(logger.getEffectiveLevel());
        end
        else begin
            LogLevelRadioGroup.Enabled := true;
            LogLevelRadioGroup.ItemIndex := LevelToIndex(logger.getLevel());
        end;
    end;
    fShowingSelectedLogger := false;
end;

procedure TVLogLevelControl.RefreshButtonClick(Sender: TObject);
begin
  Update();
end;


//---------------------------------------------------------------------------

function TVLogLevelControl.GetSelectedLogger(): TVLogger;
var
  node: TTreeNode;
begin
    result := nil;

    node := LogTreeView.Selected;
    if (node <> nil) then begin
        result := TVLogger(node.Data);
    end;
end;

procedure TVLogLevelcontrol.ShowLoggerButtonClick(Sender: TObject);
begin
  TVLogger.GetInstance(ShowLoggerEdit.Text);
  ShowLoggerEdit.Text := '';
  Refresh();
end;

procedure  TVLogLevelControl.InheritCheckClick(Sender: TObject);
var
  logger: TVLogger;
  node: TTreeNode;
begin
    if (fShowingSelectedLogger = false) then begin
        logger := GetSelectedLogger();
        if (logger = nil) then begin
        end
        else begin
            if (InheritCheck.Checked) then begin
                logger.SetLevel(lvInherit);
            end
            else begin
                if (logger.GetLevel() = lvInherit) then begin
                    logger.SetLevel(logger.getEffectiveLevel());
                end;
            end;
        end;
        ShowSelectedLoggerInfo();
        // TVLogManager.GetInstance().SaveLevels();
        node := FindTreeNodeForLogger(logger);
        if (node <> nil) then begin
            RefreshAllNodes();
        end;
    end;
end;


function TVLogLevelControl.LevelToIndex(level: TVLevelValue): INteger;
begin
  Result := 0;
  if (level = lvTrace) then
    result := 0
  else if (level = lvDebug) then
    result := 1
  else if (level = lvInfo) then
    result := 2
  else if (level = lvWarn) then
    result := 3
  else if (level = lvError) then
    result := 4
  else if (level = lvFatal) then
    result := 5;
end;


function TVLogLevelControl.IndexToLevel(index: Integer): TVLevelValue;
begin
  Result := lvInherit;
  if (index <= 0) then
    result := lvTrace
  else if (index = 1) then
    result := lvDebug
  else if (index = 2) then
    result := lvInfo
  else if (index = 3) then
    result := lvWarn
  else if (index = 4) then
    result := lvError
  else if (index = 5) then
    result := lvFatal;
end;

procedure  TVLogLevelControl.LogLevelRadioGroupClick(Sender: TObject);
var
  logger: TVLogger;
  node: TTreeNode;
begin
    if (fShowingSelectedLogger = false) then begin
        logger := GetSelectedLogger();
        if (logger <> nil ) then begin
            logger.SetLevel(IndexToLevel(LogLevelRadioGroup.ItemIndex));
            TVLogManager.GetInstance().SaveLevels();
            node := FindTreeNodeForLogger(logger);
            if (node <> nil) then begin
                RefreshAllNodes();
            end;
            TVLogManager.GetInstance().SaveLevels();
        end;
    end;
end;


//---------------------------------------------------------------------------

procedure  TVLogLevelControl.BranchInheritButtonClick(Sender: TObject);
var
  logger: TVLogger;
  recurse: boolean;
begin
    logger := GetSelectedLogger();
    if (logger <> nil) then begin
        recurse := true;
        logger.ForceChildrenToInherit(recurse);
        TVLogManager.GetInstance().SaveLevels();
    end;
    RefreshAllNodes();
end;


procedure TVLogLevelControl.Update();
var
  root: TVLogger;
begin
    inherited;
    LogTreeView.Items.Clear();
    root := TVLogger.GetRootLogger();
    LogTreeView.Selected := ShowTreeLoggerInfo(nil, root, 0);
    LogTreeView.FullExpand();
    ShowSelectedLoggerInfo();
end;


function TVLogLevelControl.FindTreeNodeForLogger(logger: TVLogger): TTreeNode;
var
  foundNode: TTreeNode;
  node: TTreeNode;
  i: integer;
  curLogger: TVLogger;
begin
    foundNode := nil;
    for i := 0 to LogTreeView.Items.Count - 1 do begin
        node := LogTreeView.Items.Item[i];
        curLogger := TVLogger(node.Data);
        if (curLogger.GetCategory() = logger.GetCategory()) then begin
            foundNode := node;
        end;

    end;
    result := foundNode;
end;

function TVLogLevelControl.ShowTreeLoggerInfo(parentNode: TTreeNode; logger: TVLogger; depth: Integer): TTreeNode;
var
  childNode: TTreeNode;
  children: TList;
  i: integer;
  child: TVLogger;
begin

    if (parentNode = nil) then begin
        childNode := LogTreeView.Items.AddChildObject(parentNode, '', logger);
    end
    else begin
        childNode := LogTreeView.Items.AddChildObject(parentNode, '', logger);
    end;

    RefreshNode(childNode);
    children := logger.GetLoggerChildren();

    for i := 0 to children.Count - 1 do begin
        child := TVLogger(children.Items[i]);
        ShowTreeLoggerInfo(childNode, child, depth + 1);
    end;
    children.Destroy();

    result := childNode;
end;

procedure TVLogLevelControl.RefreshAllNodes();
var
  i: Integer;
  node: TTreeNode;
begin
    for i := 0 to LogTreeView.Items.Count - 1 do begin
        node := LogTreeView.Items.Item[i];
        RefreshNode(node);
    end;
end;

procedure TVLogLevelControl.RefreshNode(node: TTreeNode);
var
  imageIndex: Integer;
  logger: TVLogger;
  categoryName: String;
begin

    logger := TVLogger(node.Data);
    if (logger <> nil) then begin
      imageIndex :=  TVLogManager.GetInstance().GetDisplayManager().GetSeverityIndex(logger.GetEffectiveLevel());
      if (logger.GetLevel = lvInherit) then begin
        imageIndex := 6 + ImageIndex;
      end;
      node.ImageIndex := imageIndex;
      node.SelectedIndex := imageIndex;

      categoryName := 'root';
      if (logger.GetCategory() <> TVLogger.GetRootLogger().getCategory()) then begin
          categoryName := logger.getCategory();
      end;
    end;

    node.Text :=  categoryName;
end;


procedure  TVLogLevelControl.LogTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
    ShowSelectedLoggerInfo();
    if (LogTreeView.Selected <> nil) then begin
        RefreshNode(LogTreeView.Selected);
    end;
end;



//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================


constructor TVLogListControl.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  BevelOuter := bvNone;
  Caption := '';
  BevelInner := bvNone;
  BorderStyle := bsNone;

  Log := TVLogger.GetInstance('vectrics.logging.visual.TVListControl');

  fCreateComplete := false;
  Width := 400;
  Height := 30;
  fInitialized := false;
  fSpacing := 10;
  fRefreshInterval := 500;
  fLevelComboWidth := 70;
  fLevelCaptionWidth := 80;
  fFollowLatestPanelWidth := 2 + fLevelCaptionWidth + fSpacing;
  fLevelPanelWidth := fLevelComboWidth + fLevelCaptionWidth + fSpacing;
  fTop := 2;
  fPrevNextBtnWidth := 90;
  fPrevNextBtnHeight := 20;
  fPrevNextPanelWidth := fPrevNextBtnWidth * 2 + fSpacing + 4;
  fSizeCaptionWidth := 80;
  fSizeLabelWidth := 80;
  fTrackWidth := 260;
  fTrackCaptionWidth := 45;

  BorderStyle := bsNone;

  TopPanel := TPanel.Create(self);
  TopPanel.Parent := self;

  Row2Panel := TPanel.Create(self);
  Row2Panel.Parent := self;

  FollowLatestPanel := TPanel.Create(self);
  FollowLatestPanel.Parent := TopPanel;

  FollowLatestCheck := TCheckBox.Create(self);
  FollowLatestCheck.Parent := FollowLatestPanel;

  LevelPanel := TPanel.Create(self);
  LevelPanel.Parent := TopPanel;

  LevelCaption := TStaticText.Create(self);
  LevelCaption.Parent := LevelPanel;

  LevelCombo := TComboBox.Create(LevelPanel);
  LevelCombo.Parent := LevelPanel;

  PrevNextPanel := TPanel.Create(self);
  PrevNextPanel.Parent := TopPanel;
  PrevNextPanel.Align := alRight;





    PreviousButton := TBitBtn.Create(self);
    PreviousButton.Parent := PrevNextPanel;
    PreviousButton.Align := alNone;
    PreviousButton.Parent := PrevNextPanel;
    PreviousButton.Caption := '';
    PreviousButton.Hint := 'Display previous page of events';
    PreviousButton.Width := PreviousButton.Height;


    NextButton := TBitBtn.Create(self);
    NextButton.Parent := PrevNextPanel;
    NextButton.Width := PreviousButton.Height;
    NextButton.Parent := PrevNextPanel;
    NextButton.Align := alNone;
    NextButton.Caption := '';
    NextButton.Hint := 'Display next page of events';



  TrackPanel := TPanel.Create(self);
  TrackPanel.Parent := Row2Panel;
  TrackPanel.Align := alRight;

  LogTrackCaption := TStaticText.Create(self);
  LogTrackCaption.Parent := TrackPanel;

  LogTrackBar := TTrackBar.Create(self);
  LogTrackBar.Parent := TrackPanel;

  SizePanel := TPanel.Create(self);
  SizePanel.Parent := Row2Panel;

  SizeCaption := TStaticText.Create(self);
  SizeCaption.Parent := SizePanel;

  SizeLabel := TStaticText.Create(self);
  SizeLabel.Parent := SizePanel;

  Resize();
end;


procedure TVLogListControl.CreateWnd();
begin
  inherited;
  LevelCombo.Items.AddObject('All', nil);
  LevelCombo.Items.AddObject('Trace', nil);
  LevelCombo.Items.AddObject('Debug',nil);
  LevelCombo.Items.AddObject('Info', nil);
  LevelCombo.Items.AddObject('Warn', nil);
  LevelCombo.Items.AddObject('Error', nil);
  LevelCombo.Items.AddObject('Fatal', nil);
  LevelCombo.ItemIndex := 0;
  LevelCombo.OnChange := LevelComboChange;
  LevelCombo.Visible := true;

  fCreateComplete := true;

  ListTimer := TTimer.Create(self);
  ListTimer.Interval := 1000;
  ListTimer.OnTimer := ListTimerTimer;
  ListTimer.Enabled := true;

  SetFollowLatest(true);
  fRefreshInProgress := false;

    PreviousButton.Glyph := TVLogManager.GetInstance().GetDisplayManager().GetPreviousButtonImage(PreviousButton).Picture.Bitmap;
    NextButton.Glyph := TVLogManager.GetInstance().GetDisplayManager().GetNextButtonImage().Picture.Bitmap;

end;


destructor TVLogListControl.Destroy();
begin
  if (Assigned(ListTimer)) then begin
    ListTimer.Enabled := false;
  end;

  if (fLogListView <> nil) then begin
      if (fLogListView.GetListControl() = self) then begin
          try
              fLogListView.SetListControl(nil);
          except
          end;
      end;
  end;
  inherited;
end;


procedure TVLogListControl.Resize;
begin
  fSpacing := 10;
  fRefreshInterval := 500;
  fLevelComboWidth := 120;
  fLevelCaptionWidth := 70;
  fFollowLatestCheckWidth := 90;
  fFollowLatestPanelWidth := fFollowLatestCheckWidth + fSpacing;
  fLevelPanelWidth := fLevelComboWidth + fLevelCaptionWidth + fSpacing;
  fTop := 2;
  fPrevNextBtnWidth := 18;
  fPrevNextBtnHeight := 18;
  fPrevNextPanelWidth := fPrevNextBtnWidth * 2 + fSpacing + 4;
  fTopRowHeight := 30;

  TopPanel.Width := Width;
  TopPanel.Caption := '';
  TopPanel.Height := fTopRowHeight;
  TopPanel.Align := alTop;
  TopPanel.Visible := true;
  TopPanel.BevelOuter := bvNone;
  TopPanel.BevelInner := bvNone;
  TopPanel.BorderStyle := bsNone;
  TopPanel.Visible := true;

  FollowLatestPanel.Parent := TopPanel;
  FollowLatestPanel.Width := fFollowLatestPanelWidth;
  FollowLatestPanel.Caption := '';
  FollowLatestPanel.Align := alLeft;
  FollowLatestPanel.Visible := true;
  FollowLatestPanel.BevelOuter := bvNone;
  FollowLatestPanel.BevelInner := bvNone;
  FollowLatestPanel.BorderStyle := bsNone;

  FollowLatestCheck.Parent := FollowLatestPanel;
  FollowLatestCheck.Width := fFollowLatestCheckWidth;
  FollowLatestCheck.Caption := 'Follow Latest';
  FollowLatestCheck.Left := 2;
  FollowLatestCheck.Align := alNone;
  FollowLatestCheck.Height := 15;
  FollowLatestCheck.Visible := true;
  FollowLatestCheck.Top := fTop + 2;
  FollowLatestCheck.OnClick := FollowLatestCheckClick;

  LevelPanel.Parent := TopPanel;
  LevelPanel.Width := fLevelPanelWidth;
  LevelPanel.Align := alLeft;
  LevelPanel.Visible := true;
  LevelPanel.BevelOuter := bvNone;
  LevelPanel.BevelInner := bvNone;
  LevelPanel.BorderStyle := bsNone;

  LevelCaption.Parent := LevelPanel;
  LevelCaption.Caption := 'Threshold';
  LevelCaption.Width := fLevelCaptionWidth;
  LevelCaption.Left := 2;
  LevelCaption.Top := fTop + 2;
  LevelCaption.Visible := true;
  LevelCaption.AutoSize := false;

  LevelCombo.Parent := LevelPanel;
  LevelCombo.Top := fTop;
  LevelCombo.Width := fLevelComboWidth;
  LevelCombo.Height := 15;
  LevelCombo.Left := LevelCaption.Left + LevelCaption.Width;
  LevelCombo.Style := csDropDownList;
  LevelCombo.Visible := true;

  PrevNextPanel.Parent := TopPanel;
  PrevNextPanel.Caption := '';
  PrevNextPanel.Width := (2 * fPrevNextBtnWidth) + 4 + fSpacing;
  PrevNextPanel.Visible := true;
  PrevNextPanel.BevelOuter := bvNone;
  PrevNextPanel.BevelInner := bvNone;
  PrevNextPanel.BorderStyle := bsNone;

  PreviousButton.Width := fPrevNextBtnWidth;
  PreviousButton.Height := 20;
  PreviousButton.Width := 20;
  PreviousButton.Visible := true;
  PreviousButton.Left := 0;
  PreviousButton.Top := 0;
  PreviousButton.OnClick := PreviousLogButtonClick;

  NextButton.Width := fPrevNextBtnWidth;
  NextButton.Height := 20;
  NextButton.Width := 20;
  NextButton.Visible := true;
  NextButton.Left := PreviousButton.Left + PreviousButton.Width + 4;
  NextButton.Top := 0;
  NextButton.OnClick := NextLogButtonClick;

  Row2Panel.Width := TopPanel.Width;
  Row2Panel.Align := alLeft;
  Row2Panel.Caption := '';
  Row2Panel.Visible := true;
  Row2Panel.Height := fTopRowHeight;
  Row2Panel.BevelOuter := bvNone;
  Row2Panel.BevelInner := bvNone;
  Row2Panel.BorderStyle := bsNone;
  Row2Panel.Visible := true;

  TrackPanel.Parent := TopPanel;
  TrackPanel.Caption := '';
  TrackPanel.Width := fTrackWidth + fTrackCaptionWidth + fSpacing;
  TrackPanel.Visible := true;
  TrackPanel.BevelOuter := bvNone;
  TrackPanel.BevelInner := bvNone;
  TrackPanel.BorderStyle := bsNone;

  LogTrackCaption.Parent := TrackPanel;
  LogTrackCaption.Caption := 'Position';
  LogTrackCaption.Width := fTrackCaptionWidth;
  LogTrackCaption.Left := 2;
  LogTrackCaption.Top := fTop + 2;
  LogTrackCaption.Visible := true;
  LogTrackCaption.AutoSize := false;

  LogTrackBar.Width := fTrackWidth;
  LogTrackBar.Min := 0;
  LogTrackBar.Max := 1000;
  LogTrackBar.Left := LogTrackCaption.LEft + LogTrackCaption.Width;
  LogTrackBar.Enabled := true;
  LogTrackBar.Position := 1000;
  LogTrackBar.ThumbLength := 17;
  LogTrackBar.TickStyle := tsManual;
  LogTrackBar.OnChange := LogTrackBarChange;

  SizePanel.Parent := Row2Panel;
  SizePanel.Caption := '';
  SizePanel.Width := fLevelPanelWidth;
  SizePanel.Align := alLeft;
  SizePanel.Visible := true;
  SizePanel.BevelOuter := bvNone;
  SizePanel.BevelInner := bvNone;
  SizePanel.BorderStyle := bsNone;

  SizeCaption.Parent := SizePanel;
  SizeCaption.Caption := 'Storage Size';
  SizeCaption.Width := fSizeCaptionWidth;
  SizeCaption.Left := 2;
  SizeCaption.Top := fTop;
  SizeCaption.Visible := true;
  SizeCaption.AutoSize := false;

  SizeLabel.Parent := SizePanel;
  SizeLabel.Caption := '0 kb';
  SizeLabel.Width := fSizeLabelWidth;
  SizeLabel.Left := fSizeCaptionWidth + 4;
  SizeLabel.Top := fTop;
  SizeLabel.Visible := true;
  SizeLabel.AutoSize := false;

  inherited;
end;


procedure TVLogListControl.LogTrackBarChange(Sender: TObject);
var
  percent: Integer;
begin
  if (fRefreshInProgress = false) then begin
    percent := LogTrackBar.Position;
    SetFollowLatest(false);
    if (GetLogListView() <> nil) then begin
      GetLogListView().SetPageToPercent(percent);
    end;
  end;
end;



function TVLogListControl.GetTimer(): TTimer;
begin
  if (ListTimer = nil) then begin

  end;

  result := ListTimer;
end;

procedure TVLogListControl.FillLogListView();
begin
    if (GetLogListView() <> nil) then begin
        GetLogListView().Refresh();
    end;
end;


procedure TVLogListControl.SetLogListView(listView: TVLogListView);
begin
    fLogListView := listView;
    if (fLogListView <> nil) then begin
        fLogListView.SetListControl(self);
        SetFollowLatest(FollowLatestCheck.Checked);
        RefreshLevelCombo();
        fInitialized := true;
    end
    else begin
        SetFollowLatest(false);

    end;
end;


function TVLogListControl.FindLogListViewInBranch(parent: TComponent): TVLogListView;
var
  component: TComponent;
  i: Integer;
begin
  result := nil;
  i := 0;
  while ((result = nil) and (i < parent.ComponentCount)) do begin
      component := parent.Components[i];
      if (component is TVLogListView) then begin
          result := TVLogListView(component);
      end
      else begin
          result := FindLogListViewInBranch(component);
      end;
      Inc(i);
  end;
end;


function TVLogListControl.GetLogListView(): TVLogListView;
var
  parent: TComponent;
begin
    parent := Owner;
    if (fLogListView = nil) then begin
        // This is a search routine that looks for a LogListView instance attached to the owner.
        // This routine runs just the first time the list view is requested (fInitialized = false).
        if (fInitialized = false) then begin
            while ((parent <> nil) and (fLogListView = nil)) do begin
                fLogListView := FindLogListViewInBranch(parent);
                parent := parent.Owner;
            end;

            if (fLogListView <> nil) then begin
                SetLogListView(fLogListView);
            end;
        end;

    end;
    result := fLogListView;
end;



procedure TVLogListControl.NextLogButtonClick(Sender: TObject);
begin
  SetFollowLatest(false);
  if (GetLogListView() <> nil) then begin
    GetLogListView().NextPage();
  end;
end;


procedure TVLogListControl.PreviousLogButtonClick(Sender: TObject);
begin
  SetFollowLatest(false);
  if (GetLogListView() <> nil) then begin
    GetLogListView().PreviousPage();
  end;
end;



//----------------------------------------------------------------------------------------
procedure TVLogListControl.FollowLatestCheckClick(Sender: TObject);
begin
    SetFollowLatest(FollowLatestCheck.Checked);
end;


//----------------------------------------------------------------------------------------
function TVLogListControl.GetFollowLatest(): boolean;
begin
    result := FollowLatestCheck.Checked;
end;


//----------------------------------------------------------------------------------------
procedure TVLogListControl.SetFollowLatest(value: boolean);
begin
  if (value = true) then begin
      FollowLatestCheck.Checked := true;
      if not (csDesigning in ComponentState) then begin
        GetTimer().Interval := fRefreshInterval;
      end;
      if (GetLogListView() <> nil) then begin
          GetLogListView().Suspended := false;
      end;
  end
  else begin
      FollowLatestCheck.Checked := false;
      if (GetLogListView() <> nil) then begin
          GetLogListView().Suspended := true;
      end;
  end;
end;



//----------------------------------------------------------------------------------------

procedure TVLogListControl.ListTimerTimer(Sender: TObject);
var
  size: Integer;
  percent: Integer;
  indexPosition: Integer;
  currentIndex: Integer;
  persistenceSize: Integer;
begin
  if not (csDesigning in ComponentState) then begin
    try
        fRefreshInProgress := true;
        if (fCreateComplete) then begin
            persistenceSize := GetLogListView().GetPersistence().GetSize();

            SizeLabel.Caption := IntToStr(persistenceSize div 1000) + ' kb';

            if (FollowLatestCheck.Checked) then begin

              if (GetLogListView() <> nil) then begin
                  GetLogListView().Refresh();
              end;
            end;

            // Refresh track bar to reflect position of latest display
              if (GetLogListView() <> nil) then begin
                currentIndex := GetLogListView().GetLastStartIndex();
                if (currentIndex < 0) then begin
                  percent := 1000;
                end
                else begin
                  indexPosition := GetLogListView().GetPersistence().FindPositionOfIndex(currentIndex);
                  size := GetLogListView().GetPersistence().GetSize();
                  percent := ((100 * indexPosition) div size) * 10;
                end;

              end
              else begin
                percent := 0;
              end;
              LogTrackBar.Position := percent;
        end;
        fRefreshInProgress := false;
    except
        on ex: Exception do begin
            SetFollowLatest(false);
            Log.Error('Exception while refreshing list view.', ex);
            raise TVException.Create('Exception during list refresh: ' + ex.Message);
        end;
    end;
  end;
end;


//---------------------------------------------------------------------------


procedure TVLogListControl.RefreshLevelCombo();
begin
    if (GetLogListView() <> nil) then begin

        if (GetLogListView().DisplayLevel = lvInherit) then begin
            LevelCombo.ItemIndex := 0;
        end
        else if (GetLogListView().DisplayLevel = lvTrace) then begin
            LevelCombo.ItemIndex := 1;
        end
        else if (GetLogListView().DisplayLevel = lvDebug) then begin
            LevelCombo.ItemIndex := 2;
        end
        else if (GetLogListView().DisplayLevel = lvInfo) then begin
            LevelCombo.ItemIndex := 3;
        end
        else if (GetLogListView().DisplayLevel = lvWarn) then begin
            LevelCombo.ItemIndex := 4;
        end
        else if (GetLogListView().DisplayLevel = lvError) then begin
            LevelCombo.ItemIndex := 5;
        end
        else if (GetLogListView().DisplayLevel = lvFatal) then begin
            LevelCombo.ItemIndex := 6;
        end;
    end;
end;

procedure TVLogListControl.LevelComboChange(Sender: TObject);
var
  index: Integer;
begin
    index := LevelCombo.ItemIndex;

    if (GetLogListView() <> nil) then begin

        if (index <= 0) then begin
            GetLogListView().DisplayLevel := lvInherit;
        end
        else if (index <= 1) then begin
            GetLogListView().DisplayLevel := lvTrace;
        end
        else if (index <= 2) then begin
            GetLogListView().DisplayLevel := lvDebug;
        end
        else if (index <= 3) then begin
            GetLogListView().DisplayLevel := lvInfo;
        end
        else if (index <= 4) then begin
            GetLogListView().DisplayLevel := lvWarn;
        end
        else if (index <= 5) then begin
            GetLogListView().DisplayLevel := lvError;
        end
        else if (index <= 6) then begin
            GetLogListView().DisplayLevel := lvFatal;
        end
        else if (index <= 7) then begin
            GetLogListView().DisplayLevel := lvOff;
        end;
        GetLogListView().Refresh();
    end;

end;


//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================

constructor TVLogListView.Create(Owner: TComponent);
begin
    inherited Create(Owner);
    //Log := TVLogger.GetInstance('vectrics.logging.visual.TVLogListView');
    HideSelection := false;
    fWriteLevel := lvTrace;
    fDisplayLevel := lvTrace;
    fActive := true;
    fAutoRowCount := true;
    fFilter := nil;
    fDateFormat := 'mm-dd h:nn:ss';
    fDirty := false;
    fPageStartIndex := -1;
    fLastStartIndex := -1;
    fLastEndIndex := -1;
    SmallImages := TVLogManager.GetInstance().GetDisplayManager().GetSeverityIcons();
    fEventDetailFrame := nil;
    fPersistence := nil;
    fDefaultPersistence := nil;
    fMaxLines := 200;
    fFreeCount := 10;
    fBufferMutex := TCriticalSection.Create();
    fMutex := TCriticalSection.Create();
    fBufferedEventList := TList.Create();
    ViewStyle := vsReport;
    fSuspended := false;
    ReadOnly := true;
    fSettingNumberOfRows := false;

    fIndexColumn := Columns.Add();
    fIndexColumn.Caption := 'Index';
    fIndexColumn.Width := 70;

    fDateColumn := Columns.Add();
    fDateColumn.Caption := 'Date & Time';
    fDateColumn.Width := 110;

    fSeverityColumn := Columns.Add();
    fSeverityColumn.Caption := 'Severity';
    fSeverityColumn.Width := 50;

    fLoggerColumn := Columns.Add();
    fLoggerColumn.Caption := 'Logger';
    fLoggerColumn.Width := 120;

    fMessageColumn := Columns.Add();
    fMessageColumn.Caption := 'Message';
    fMessageColumn.AutoSize := true;

    GridLines := true;
    RowSelect := true;

//    GetPersistence();
    fNumberOfRows := 15;
end;



destructor TVLogListView.Destroy();
begin
    if (fListControl <> nil) then begin
        try
            fListControl.SetLogListView(nil);
        except
        end;
    end;

    fBufferedEventList.Destroy();

    fBufferMutex.Acquire();
    fBufferMutex.Destroy();

    fMutex.Acquire();
    fMutex.Destroy();

    //if (fPersistence <> nil) then begin
    //    fPersistence.Destroy();
    //end;
    inherited;
end;


procedure  TVLogListView.Delete(item: TListItem);
var
  event: TVLoggingEvent;
begin
    if (item.Data <> nil) then begin
        event := TVLoggingEvent(item.Data);
        event.Destroy();
        item.Data := nil;
    end;
    inherited Delete(item);
end;

function TVLogListView.GetPersistence(): TVLogPersistence;
begin
    if (fPersistence = nil) then begin

        fDefaultPersistence := TVFileLogPersistence.Create(self);
        fPersistence := fDefaultPersistence;
        if (fPersistence.FindLastIndex > TVLogManager.GetInstance.GetNextId()) then begin
          TVLogManager.GetInstance().SetNextId(fPersistence.FindLastIndex);
        end;

    end;
    result := fPersistence;
end;

procedure TVLogListView.SetPersistence(persistence: TVLogPersistence);
begin
    if (persistence = nil) then begin
      raise Exception.Create('Cannot assign NULL persistence to TVLogListView.');
    end
    else begin
      if (fDefaultPersistence <> nil) then begin
          fDefaultPersistence.Destroy();
          fDefaultPersistence := nil;
      end;
      fPersistence := persistence;
    end;
end;


function TVLogListView.GetListControl(): TVLogListControl;
begin
    result := fListControl;
end;

procedure TVLogListView.SetListControl(listControl: TVLogListControl);
begin
    fListControl := listControl;
end;


procedure TVLogListView.AddBufferedEvent(event: TVLoggingEvent);
begin
    fBufferMutex.Acquire();
    while (fBufferedEventList.Count > fMaxLines) do begin
        fBufferedEventList.Delete(0);
    end;
    fBufferedEventList.Add(event);
    fBufferMutex.Release();
end;


procedure TVLogListView.SetDetailFrame(eventDetailFrame: TVEventDetailDisplay);
begin
    fEventDetailFrame := eventDetailFrame;
end;




function TVLogListView.FindDetailFrameInBranch(parent: TComponent): TVEventDetailDisplay;
var
  component: TComponent;
  i: Integer;
begin
  result := nil;
  i := 0;
  while ((result = nil) and (i < parent.ComponentCount)) do begin
      component := parent.Components[i];
      if (component is TVEventDetailDisplay) then begin
          result := TVEventDetailDisplay(component);
      end
      else begin
          result := FindDetailFrameInBranch(component);
      end;
      Inc(i);
  end;
end;


procedure TVLogListView.SetPageToPercent(percent: Integer);
var
  position: Integer;
  //index: Integer;
begin
  position := ((GetPersistence().GetSize() div 100) * percent) div 10;
  fPageStartIndex := GetPersistence().FindIndexAtPosition(position);
  if ((fPageStartIndex = 0) and (GetPersistence().GetSize() > 0)) then begin
    fPageStartIndex := GetPersistence().FindLastIndex();
  end;
  Refresh();
end;


//Left off:  Removed a call to GetPersistence() in constructor.

function TVLogListView.GetDetailFrame(): TVEventDetailDisplay;
var
  parent: TComponent;
begin
    parent := Owner;
    if (fEventDetailFrame = nil) then begin
        // This is a search routine that looks for a LogListView instance
        //  attached to the owner.
        while ((parent <> nil) and (fEventDetailFrame = nil)) do begin
            fEventDetailFrame := FindDetailFrameInBranch(parent);
            parent := parent.Owner;
        end;

        if (fEventDetailFrame <> nil) then begin
            SetDetailFrame(fEventDetailFrame);
        end;

    end;
    result := fEventDetailFrame;
end;


procedure TVLogListView.Resize();
var
    rows: Integer;
    rowHeight: Integer;
begin
    inherited;
    if (fAutoRowCount) then begin
        rowHeight := 18;
        rows := (Height - rowHeight) div rowHeight;
        if (numberOfRows < 3) then begin
            numberOfRows := 3;
        end;
        NumberOfRows := rows;
    end;
end;


procedure TVLogListView.SetNumberOfRows(rowCount: Integer);
begin
    if (rowCount <> fNumberOfRows) then begin
        if (fSettingNumberOfRows = false) then begin
            fSettingNumberOfRows := true;
            fLastEndIndex := -1;
            Items.Clear();
            fNumberOfRows := rowCount;
            Refresh();
            fSettingNumberOfRows := false;
        end
    end;
end;

function TVLogListView.GetNumberOfRows(): Integer;
begin
    result := fNumberOfRows;
end;


procedure  TVLogListView.Refresh();
var
  startIndex: integer;
  list: TVPageList;
begin
    fMutex.Acquire();

    try
        //Log.Debug('Refresh() called');
        if (Active) and (Showing) then begin
          GetPersistence().SetReadLevel(fDisplayLevel);

          if (fPageStartIndex < 0) then begin
              startIndex := GetPersistence().FindLastIndex();
          end
          else begin
              startIndex := fPageStartIndex;
          end;


          if ((startIndex <> fLastEndIndex) or (fDirty)) then begin
              fLastStartIndex := -1;
              fLastEndIndex := -1;
              list := TVPageList.Create();
              //Log.Trace('Refresh() - finding page of events from Persistence.');
              getPersistence().FindEventPage(startIndex, fNumberOfRows, list, fFilter);
              //Log.Trace('Refresh() - showing page of events.');
              ShowEvents(list);

              //Log.Trace('Refresh() - cleaning up pagelist.');
              list.DeleteMembers;
              list.Destroy();
              fDirty := false;
          end
          else begin
            //Log.Trace('Refresh() - did not perform refresh - start index was the same and Dirty field not set.');
          end;
        end;
        //Log.Debug('Refresh() - finished.');
    except
        on ex: Exception do begin
            TVLogger.GetInstance('vectrics.logging.TVLogListView').Error('Exception during refresh', ex);
        end;
    end;

    fMutex.Release();
end;


procedure  TVLogListView.NextPage();
begin
    if (fLastEndIndex > 0) then begin
        fPageStartIndex := getPersistence().FindNextPageIndex(fLastEndIndex, fNumberOfRows - 1, nil);
        Refresh();
    end
    else begin
        fPageStartIndex := getPersistence().FindNextPageIndex(fPageStartIndex, fNumberOfRows, nil);
        Refresh();
    end;
end;

procedure  TVLogListView.PreviousPage();
begin
    if (fLastStartIndex > 0) then begin
        fPageStartIndex := fLastStartIndex;//getPersistence().findPreviousPageIndex(fLastStartIndex, fNumberOfRows);
        Refresh();
    end
    else  begin
        if (fPageStartIndex < 0) then begin
          fPageStartIndex := GetPersistence().FindLastIndex();
        end;
        fPageStartIndex := getPersistence().FindPreviousPageIndex(fPageStartIndex, fNumberOfRows, nil);
        Refresh();
    end;
end;

function TVLogListView.GetLastStartIndex(): INteger;
begin
  result := fLastEndIndex;
end;

procedure TVLogListView.SetFilter(filter: TVFilter);
begin
  fFilter := filter;
end;

function TVLogListView.GetFilter(): TVFilter;
begin
  result := fFilter;
end;


procedure TVLogListView.ShowEvents(list: TVPageList);
var
  event: TVLoggingEvent;
  row: Integer;
  i: integer;
begin
    if ((csDesigning in ComponentState) = false) then begin
        fLastStartIndex := -1;
        fLastEndIndex := -1;
        row := 0;
        for  i := 0 to list.Count - 1 do begin
            event := TVLoggingEvent(list.Objects[i]);
            if ((event.GetIndex() > fLastEndIndex) or (fLastEndIndex < 0)) then begin
                fLastEndIndex := event.GetIndex();
            end;
            if ((event.GetIndex() < fLastStartIndex) or (fLastStartIndex < 0)) then begin
                fLastStartIndex := event.GetIndex();
            end;
            try
              ShowEvent(row, event);
            except
              raise Exception.Create('Exception showing event: ' + IntToStr(event.GetIndex()));
            end;
            Inc(row);
        end;

        while (row < fNumberOfRows) do begin
            ShowEvent(row, nil);
            Inc(row);
        end;

        if (Selected <> nil) then begin
            event := TVLoggingEvent(Selected.Data);
            if (event <> nil) then begin
                if (GetDetailFrame() <> nil) then begin
                    GetDetailFrame().SetLoggingEvent(event);
                end;
            end;
        end;
    end;

end;


procedure  TVLogListView.Change(item: TListItem; change: Integer);
var
  event: TVLoggingEvent;
begin
    inherited Change(Item, change);
    if (change = LVIF_STATE) then begin
        if (Selected <> nil) then begin
            event := TVLoggingEvent(Selected.Data);//(TVLoggingEvent *)Item.Data;
            if (Assigned(event)) then begin
              if (GetDetailFrame() <> nil) then begin
                  GetDetailFrame().setLoggingEvent(event);
              end;
            end;
        end;
    end;
end;


class function TVLogListView.Encode(before: String): String;
var
  newData: String;
  flags: TReplaceFlags;
begin
    newData := before;
    flags := [rfReplaceAll];
    newData := StringReplace(newData, #13, ' >', flags);
    newData := StringReplace(newData, #10, '', flags);
    newData := StringReplace(newData, #9, '     ', flags);
    result := newData;
end;


procedure TVLogListView.ShowEvent(row: Integer; event: TVLoggingEvent);
var
  oldEvent: TVLoggingEvent;
  existEvent: TVLoggingEvent;
  newItem: TListItem;
  idString: String;
  dateStr: String;
  severityString: String;
  cat: String;
  msg: String;
  now: TDateTime;
  copyEvent: TVLoggingEvent;
  oldEventIndex: Integer;
  eventIndex: Integer;
begin
    try
      // Create new row if we haven't already allocated enough rows to hold
      // new event.
      newItem := nil;
      if (row < Items.Count) then begin
          newItem := Items.Item[row];
      end
      else begin
          while (row >=  Items.Count) do begin
              newItem := Items.Add();
          end;
      end;
    except
      raise Exception.Create('Exception in ShowEvent() during row allocation - event = ' + IntToStr(event.GetIndex()));
    end;

    try
      oldEvent := TVLoggingEvent(newItem.Data);
      oldEventIndex := 0;
      if (oldEvent <> nil) then
        oldEventIndex := oldEvent.GetIndex();

      if (event <> nil) then begin
        eventIndex := event.GetIndex();
      end
      else begin
        eventIndex := -1;
      end;


      if ((oldEvent = nil) or (oldEventIndex <> eventIndex) or (fDirty)) then begin
        try
          idString := '';
          severityString := '';
          cat := '';
          severityString := '';
          msg := '';
            if (event = nil) then begin
                newItem.ImageIndex := -1;
            end
            else begin
                idString := IntToStr(event.GetIndex());
                now := event.GetTime();
                DateTimeToString(dateStr, fDateFormat, now);
                cat := event.GetLogger();
                severityString := TVLevel.ToString(event.GetLevel());
                msg := event.GetMessage();
                newItem.ImageIndex :=  TVLogManager.GetInstance().GetDisplayManager().GetSeverityIndex(event.GetLevel());
            end;

            if (newItem.Data <> nil) then begin
                existEvent := TVLoggingEvent(newItem.Data);
                newItem.Data := nil;
                try
                    if (existEvent <> nil) then begin
                        existEvent.Destroy();
                    end;
                except
                end;
            end;
          except
            msg := 'Exception displaying event: ' + IntToStr(event.GetIndex());
          end;


          try
            newItem.Caption := Encode(idString);
            newItem.SubItems.Clear();
            newItem.SubItems.Add(Encode(dateStr));
            newItem.SubItems.Add(Encode(severityString));
            newItem.SubItems.Add(Encode(cat));
            newItem.SubItems.Add(Encode(msg));

            copyEvent := nil;
            if (event <> nil) then begin
                copyEvent := TVLoggingEvent.Create();
                copyEvent.Assign(event);
            end;
            newItem.Data := TObject(copyEvent);
          except
            msg := 'Exception displaying event - setting item properties: ' + IntToStr(event.GetIndex());
          end;
      end;
    except
      raise Exception.Create('Exception in ShowEvent() - event = ' + IntToStr(event.GetIndex()));
    end;

end;


procedure TVLogListView.SetSuspended(value: boolean);
begin
    fSuspended := value;
    if (value = false) then begin
        fPageStartIndex := -1;
    end;
end;


function  TVLogListView.GetSuspended(): boolean;
begin
    result := fSuspended;
end;


procedure  TVLogListView.SetDisplayLevel(level: TVLevelValue);
begin
    fDisplayLevel := level;
    fDirty := true;
end;


function TVLogListView.GetDisplayLevel(): TVLevelValue;
begin
    result := fDisplayLevel;
end;


function TVLogListView.GetLevel(): TVLevelValue;
begin
    result := fWriteLevel;
end;


procedure  TVLogListView.SetLevel(level: TVLevelValue);
begin
    fWriteLevel := level;
    if (fPersistence <> nil) then begin
        fPersistence.SetWriteLevel(level);
    end;
end;


procedure  TVLogListView.SetPageStartIndex(index: Integer);
begin
    fPageStartIndex := -1;
end;

function TVLogListView.GetPageStartIndex(): Integer;
begin
    result := fPageStartIndex;
end;


function TVLogListView.GetFollow(): boolean;
begin
    result := (fPageStartIndex < 0);
end;


procedure TVLogListView.SetFollow(follow: boolean);
begin
    if (follow = true) then begin
        fPageStartIndex := -1;
    end
    else begin
        if (fPageStartIndex < 0) then begin
            fPageStartIndex := fLastStartIndex;
        end;
    end;
end;



//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================


constructor TVEventDetailDisplay.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  setLoggingEvent(nil);
  Caption := '';

  BevelInner := bvNone;
  BevelOuter := bvNone;

  IndexCaption := TStaticText.Create(self);
  IndexCaption.Parent := self;
  IndexCaption.BorderStyle := sbsNone;
  IndexCaption.AutoSize := False;
  IndexCaption.Caption := 'ID';

  IndexLabel := TStaticText.Create(self);
  IndexLabel.Parent := self;
  IndexLabel.BorderStyle := sbsSunken;
  IndexLabel.AutoSize := False;

  LoggerCaption := TStaticText.Create(self);
  LoggerCaption.Parent := self;
  LoggerCaption.BorderStyle := sbsNone;
  LoggerCaption.AutoSize := False;
  LoggerCaption.Caption := 'Logger';

  LoggerLabel := TStaticText.Create(self);
  LoggerLabel.Parent := self;
  LoggerLabel.BorderStyle := sbsSunken;
  LoggerLabel.AutoSize := False;


  SeverityCaption := TStaticText.Create(self);
  SeverityCaption.Parent := self;
  SeverityCaption.BorderStyle := sbsNone;
  SeverityCaption.AutoSize := False;
  SeverityCaption.Caption := 'Severity';

  SeverityLabel := TStaticText.Create(self);
  SeverityLabel.Parent := self;
  SeverityLabel.BorderStyle := sbsSunken;
  SeverityLabel.AutoSize := False;


  TimeCaption := TStaticText.Create(self);
  TimeCaption.Parent := self;
  TimeCaption.BorderStyle := sbsNone;
  TimeCaption.AutoSize := False;
  TimeCaption.Caption := 'Time';

  TimeLabel := TStaticText.Create(self);
  TimeLabel.Parent := self;
  TimeLabel.AutoSize := False;
  TimeLabel.BorderStyle := sbsSunken;

  NdcCaption := TStaticText.Create(self);
  NdcCaption.Parent := self;
  NdcCaption.BorderStyle := sbsNone;
  NdcCaption.AutoSize := False;
  NdcCaption.Caption := 'NDC';

  NdcLabel := TStaticText.Create(self);
  NdcLabel.Parent := self;
  NdcLabel.AutoSize := False;
  NdcLabel.BorderStyle := sbsSunken;


  MessageCaption := TStaticText.Create(self);
  MessageCaption.Parent := self;
  MessageCaption.BorderStyle := sbsNone;
  MessageCaption.AutoSize := False;
  MessageCaption.Caption := 'Message';

  MessageMemo := TMemo.Create(self);
  MessageMemo.Parent := self;

  Resize();
end;


procedure TVEventDetailDisplay.Resize();
var
  Y: Integer;
  X: Integer;
  LineHeight: Integer;
  ControlWidth: Integer;
  ControlHeight : Integer;
  CaptionLeft: Integer;
  memoHeight: Integer;
begin
  Caption := '';
  X := 120;
  Y := 5;
  LineHeight := 24;
  CaptionLeft := 2;
  ControlWidth := Width - (X + 4);
  if (ControlWidth < 80) then begin
    ControlWidth := 80;
  end;
  ControlHeight := 16;

  IndexCaption.Top := Y;
  IndexCaption.Left := CaptionLeft;
  IndexCaption.Width := ControlWidth;
  IndexCaption.Height := ControlHeight;

  IndexLabel.Top := Y;
  IndexLabel.Left := X;
  IndexLabel.Width := ControlWidth;
  IndexLabel.Height := ControlHeight;
  Y := Y + LineHeight;

  LoggerCaption.Top := Y;
  LoggerCaption.Left := CaptionLeft;
  LoggerCaption.Width := ControlWidth;
  LoggerCaption.Height := ControlHeight;

  LoggerLabel.Top := Y;
  LoggerLabel.Left := X;
  LoggerLabel.Width := ControlWidth;
  LoggerLabel.Height := ControlHeight;
  Y := Y + LineHeight;


  SeverityCaption.Top := Y;
  SeverityCaption.Left := CaptionLeft;
  SeverityCaption.Width := ControlWidth;
  SeverityCaption.Height := ControlHeight;

  SeverityLabel.Top := Y;
  SeverityLabel.Left := X;
  SeverityLabel.Width := ControlWidth;
  SeverityLabel.Height := ControlHeight;
  Y := Y + LineHeight;

  TimeCaption.Top := Y;
  TimeCaption.Left := CaptionLeft;
  TimeCaption.Width := ControlWidth;
  TimeCaption.Height := ControlHeight;

  TimeLabel.Parent := self;
  TimeLabel.AutoSize := False;
  TimeLabel.BorderStyle := sbsSunken;
  TimeLabel.Top := Y;
  TimeLabel.Left := X;
  TimeLabel.Width := ControlWidth;
  TimeLabel.Height := ControlHeight;
  Y := Y + LineHeight;

  if (fShowNdc) then begin
    NdcCaption.Top := Y;
    NdcCaption.Left := CaptionLeft;
    NdcCaption.Width := ControlWidth;
    NdcCaption.Height := ControlHeight;

    NdcLabel.Top := Y;
    NdcLabel.Left := X;
    NdcLabel.Width := ControlWidth;
    NdcLabel.Height := ControlHeight;
    Y := Y + LineHeight;
  end
  else begin
    NdcCaption.Visible := false;
    NdcLabel.Visible := false;
  end;



  MessageCaption.Top := Y;
  MessageCaption.Left := CaptionLeft;
  MessageCaption.Width := ControlWidth;
  MessageCaption.Height := ControlHeight;

  MessageMemo.Top := Y;
  MessageMemo.Left := X;
  MessageMemo.Width := ControlWidth;
  MessageMemo.Color := clBtnFace;
  MessageMemo.ReadOnly := true;
  memoHeight := self.Height - (MessageMemo.Top + 2);
  if (memoHeight < 20) then begin
    memoHeight := 20;
  end;
  MessageMemo.Height := memoHeight;
end;

procedure TVEventDetailDisplay.SetShowNdc(value: boolean);
begin
  fShowNdc := value;
  Resize();
end;

function TVEventDetailDisplay.GetShowNdc(): boolean;
begin
  result := fShowNdc;
end;



procedure TVEventDetailDisplay.SetLoggingEvent(event: TVLoggingEvent);
begin
  if (event <> fLoggingEvent) then begin
    fLoggingEvent := event;
    if (event = nil) then begin
        LoggerLabel.Caption := '';
        SeverityLabel.Caption := '';
        TimeLabel.Caption := '';
        MessageMemo.Text := '';
    end
    else begin
        IndexLabel.Caption := IntToStr(event.GetIndex());
        LoggerLabel.Caption := event.GetLogger();
        SeverityLabel.Caption := TVLevel.ToString(event.GetLevel());
        TimeLabel.Caption := DateTimeToStr(event.GetTime());
        NdcLabel.Caption := event.GetNdc();

        MessageMemo.Text := '';
        AddMessageToMemo(event.GetMessage());
        if (Length(event.GetException()) > 0) then begin
          MessageMemo.Lines.Add('');
          MessageMemo.Lines.Add('EXCEPTION:');
          AddMessageToMemo(event.GetException());
        end;
        MessageMemo.SelStart := 0;
        MessageMemo.SelLength := 0;
    end;
  end;
end;

procedure TVEventDetailDisplay.AddMessageToMemo(_msg: String);
var
  msg: String;
  posLine: Integer;
  line: String;
  eolStr: String;
begin
  msg := _msg;
  eolStr := #13;
  while (Length(msg) > 0) do begin
    posLine := Pos(eolStr, msg);
    if (posLine <= 0) then
      posLine := Length(msg) + 2;

    line := Copy(msg, 1, posLine - 2);
    MessageMemo.Lines.Add(line);
    if (posLine + 2 < Length(msg)) then begin
      msg := Copy(msg, posLine + 1, Length(msg) - posLine);
    end
    else begin
      msg := '';
    end;
  end;
end;



procedure Register;
begin
  RegisterComponents('Logging', [TVLogListView]);
  RegisterComponents('Logging', [TVLogListControl]);
  RegisterComponents('Logging', [TVLogLevelControl]);
  RegisterComponents('Logging', [TVEventDetailDisplay]);
end;



end.
