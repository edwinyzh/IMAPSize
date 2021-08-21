{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: GlobalSearch.pas,v 1.10 2004/03/31 23:27:38 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit GlobalSearch;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, ComCtrls, ExtCtrls, Buttons, IMAPWorker, VecLog, MyTypes,
  Accounts, GlobalSearchResult, AdvSearchDlg, Log, GlobalConstants;

type
  TFGlobalSearch = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel2: TPanel;
    ListView1: TListView;
    Panel1: TPanel;
    Panel3: TPanel;
    CheckListBox1: TCheckListBox;
    Panel4: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel5: TPanel;
    Panel6: TPanel;
    Label2: TLabel;
    EditSearchString: TEdit;
    Button3: TButton;
    StatusBar1: TStatusBar;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    fAccountName: String;
    procedure SelectAll(select: Boolean);
    procedure PerformGlobalSearch(operation: TIMAPOperation);
    procedure InvokeAdvSearchDlg;
    procedure UpdateSearchResults(index: Integer);
  public
    SearchResult: TGlobalSearchResult; // Serves for passing/holding global search results
    procedure PrepareMailboxList;
    procedure HandleGlobalSearchUpdate(var Message: TMessage); message WM_GLOBAL_SEARCH_UPDATE;
    procedure HandleGlobalSearchOver(var Message: TMessage); message WM_GLOBAL_SEARCH_OVER;
    procedure HandleRequestOver(var Message: TMessage); message WM_REQUEST_OVER;
    function NumChecked: Integer;
    procedure ClearSearchResults;
    procedure DisplayStatusOnCompletion;
    procedure FillCheckboxList;
  end;

var
  FGlobalSearch: TFGlobalSearch;

implementation

uses Main, RequestManager, MailboxTree, CustomThread, CheckerThread;

{$R *.DFM}

procedure TFGlobalSearch.FormDestroy(Sender: TObject);
begin
    SearchResult.Free;
end;

{ Fills (or invokes filling from remote server) of the mailbox list }
procedure TFGlobalSearch.PrepareMailboxList;
var needHierarchyLoad: Boolean; requestPtr: PRequest;
begin
    // Only fill mailboxes if account is not already loaded
    needHierarchyLoad:=false;
    if not (FMain.pActiveAccount^.Name = fAccountName) then begin
        // Current Account is not defined in FGlobalSearch,
        // check if we have the correct data in mboxTree
        // (content of the tree should be at least tcHierarchy and the account should be the active account,
        // if none of these is true we need to retrieve the hierarchy...)
        if (treeState.account<>nil) then begin
            if (treeState.content > tcEmpty) and (treeState.account^.Name=FMain.pActiveAccount^.Name) then begin
                fAccountName := treeState.account^.Name;  // store for future reference
                FillCheckboxList;  // Fill the checkbox list with data from mboxTree (no need for hierarchy reload)
            end
            else
                needHierarchyLoad := true;
        end
        else needHierarchyLoad:=true;
    end;

    if needHierarchyLoad then begin
        New(requestPtr);
        requestPtr^ := TRequest.Create(thtChecker);
        requestPtr^.requestType:=rtLogin;
        requestPtr^.requestComponent:=rcGlobalSearch;
        requestPtr^.accountInfoPtr:=FMain.pActiveAccount;
        requestPtr^.checkerOperation:=chOpHierarchyOnly;
        requestMgr.InvokeRequest(requestPtr);
    end;
end;

{ Handles message WM_GLOBAL_SEARCH_UPDATE.
  This is sent when a new mailbox containing a result is found.
  The index of the new search result is located in WParam }
procedure TFGlobalSearch.HandleGlobalSearchUpdate(var Message: TMessage);
var searchResultIndex: Integer;
begin
    searchResultIndex:=Message.WParam;
    UpdateSearchResults(searchResultIndex);
end;

{ Handles message WM_GLOBAL_SEARCH_OVER.
  This is sent by the RequestMgr when the Global search is done }
procedure TFGlobalSearch.HandleGlobalSearchOver(var Message: TMessage);
begin
    DisplayStatusOnCompletion;
    Show;  // show in case the dialog has been hidden in the meantime...
end;

{ Handles messages sent by the request manager when a request is over }
procedure TFGlobalSearch.HandleRequestOver(var Message: TMessage);
var requestID: Integer; success: Integer; request: TRequest;
begin
    devLog.Trace('TFGlobalSearch.HandleRequestOver');
    requestId:=Message.WParam;
    try
        request := requestMgr.GetRequest(requestId);
        success:=Message.LParam;
        if request.requestType = rtLogin then begin
            if success=1 then begin
                fAccountName := treeState.account^.Name;
                FillCheckboxList;
            end;
        end;
    finally
        requestMgr.RemoveRequest(requestId, false);
    end;
end;

procedure TFGlobalSearch.FillCheckboxList;
var i: Integer;
begin
    CheckListBox1.Clear;
    for i:=0 to mboxTree.Count-1 do begin
        if not mboxTree.Items[i].mVirtual then
            CheckListBox1.Items.Add(mboxTree.Items[i].mFullDisplayedName);
    end;

    for i:=0 to CheckListBox1.Items.Count-1 do begin
        CheckListBox1.Checked[i]:=true;
    end;
end;

{ Returns number of checked items in the check box list }
function TFGlobalSearch.NumChecked: Integer;
var count, i: Integer;
begin
    count:=0;
    for i:=0 to CheckListBox1.Items.Count-1 do begin
        if CheckListBox1.Checked[i] then Inc(count);
    end;
    Result:=count;
end;

procedure TFGlobalSearch.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if (Key=VK_RETURN) then begin
        {
            RETURN: search text
            CTRL-RETURN: advanced search
        }
        if (ssCtrl in Shift) then InvokeAdvSearchDlg
        else PerformGlobalSearch(iopSearchGlobal);
    end
    else if Key=VK_ESCAPE then Close;
end;

procedure TFGlobalSearch.InvokeAdvSearchDlg;
begin
    // Once we load the dialog it keeps loaded
    // Will be freed on app exit if assigned
    if not Assigned(FAdvSearchDlg) then FAdvSearchDlg := TFAdvSearchDlg.Create(FMain);
    if FAdvSearchDlg.ShowModal = mrOK then begin
        PerformGlobalSearch(iopSearchAdvancedGlobal);
    end;
end;

procedure TFGlobalSearch.Button1Click(Sender: TObject);
begin
    SelectAll(true);
end;

procedure TFGlobalSearch.Button2Click(Sender: TObject);
begin
    SelectAll(false);
end;

procedure TFGlobalSearch.SelectAll(select: Boolean);
var i: Integer;
begin
    for i:=0 to CheckListBox1.Items.Count-1 do begin
        CheckListBox1.Checked[i]:=select;
    end;
end;

procedure TFGlobalSearch.SpeedButton1Click(Sender: TObject);
begin
    PerformGlobalSearch(iopSearchGlobal);
end;

procedure TFGlobalSearch.SpeedButton2Click(Sender: TObject);
begin
    InvokeAdvSearchDlg;
end;

procedure TFGlobalSearch.PerformGlobalSearch(operation: TIMAPOperation);
var mboxes: TIntArray; i,checkedCnt: Integer; searchStr: String; perform: boolean;
    requestPtr: PRequest;
begin
    if NumChecked>0 then begin

        perform:=true;

        if (operation = iopSearchAdvancedGlobal) then begin
            searchStr := FAdvSearchDlg.SearchString;
        end
        else begin
            // iopSearchGlobal
            if Length(EditSearchString.Text)>0 then
                searchStr:=EditSearchString.Text
            else begin
                perform:=false;
                MessageDlg('Search string not specified!',mtInformation,[mbOK],0);
            end;
        end;

        if perform then begin
            SearchResult.Clear;
            // Fill in mailbox details in GlobalSearchResults
            SetLength(mboxes,NumChecked);
            try
                checkedCnt:=0;
                for i:=0 to CheckListBox1.Items.Count-1 do begin
                    if CheckListBox1.Checked[i] then begin
                        mboxes[checkedCnt]:=i+1;  // +1 caused by 0 being the account
                        Inc(checkedCnt);
                    end;
                end;
                SearchResult.SetMailboxes(mboxes);

                New(requestPtr); // Will be freed by the request manager when the request completes...
                requestPtr^ := TRequest.Create(thtIMAPWorker);
                requestPtr^.requestType:=rtGlobalSearch;
                requestPtr^.imapOperation:=operation;
                requestPtr^.requestComponent:=rcGlobalSearch;
                requestPtr^.accountInfoPtr:=FMain.pActiveAccount; //@todo is this OK?
                requestPtr^.data.searchString := searchStr;
                requestMgr.InvokeRequest(requestPtr);

                // Set the results page as the active one...
                ClearSearchResults;
                StatusBar1.SimpleText:='Searching...';
                PageControl1.ActivePageIndex:=1;
            finally
                SetLength(mboxes,0);
            end;
        end;
    end
    else MessageDlg('No mailboxes selected!',mtInformation,[mbOK],0);
end;

procedure TFGlobalSearch.ClearSearchResults;
begin
    ListView1.Items.Clear;
end;

procedure TFGlobalSearch.UpdateSearchResults(index: Integer);
var searchItem: TGlobalSearchInfo; ListItem: TListItem;
begin
    try
        searchItem := SearchResult[index];
        if searchItem.getNumberOfUIDs > 0 then begin
            ListItem := ListView1.Items.Add;
            ListItem.Caption := mboxTree.Items[searchItem.MailboxIndex].mFullDisplayedName;
            ListItem.SubItems.Add(IntToStr(searchItem.getNumberOfUIDs));
        end;
    except
        // catch and ignore any errors
    end;
end;

{ This method is called from HandleGlobalSearchOver when an IMAP operation (global search)
  has been completed }
procedure TFGlobalSearch.DisplayStatusOnCompletion;
var ListItem: TListItem; searchItem: TGlobalSearchInfo;
    i, cnt: Integer;
begin
    cnt:=0;
    for i:=0 to SearchResult.getLength-1 do begin
        searchItem := SearchResult[i];
        if searchItem.getNumberOfUIDs > 0 then cnt:=cnt+1;
    end;
    // If an error occurred, a separate message will be displayed (see IMAPWorker.PerformGlobalSearch)
    if ((cnt=0) and (not SearchResult.ErrorOccurred)) then begin
        StatusBar1.SimpleText:='Search completed - no matches found';
        MessageDlg('None of the mailboxes specified contains a message with the specified search',mtInformation,[mbOK],0);
    end
    else if (SearchResult.ErrorOccurred) then
        StatusBar1.SimpleText:='Search not completed due to errors'
    else begin
        StatusBar1.SimpleText:='Search completed';
        PageControl1.ActivePageIndex:=1;
    end;
end;

procedure TFGlobalSearch.Button3Click(Sender: TObject);
begin
    FillCheckboxList;
end;

procedure TFGlobalSearch.FormCreate(Sender: TObject);
begin
    Top := settings.Positions.GSTop;
    Left := settings.Positions.GSLeft;
    SearchResult:=TGlobalSearchResult.Create;
end;

procedure TFGlobalSearch.FormDeactivate(Sender: TObject);
begin
    settings.positions.GSTop := Top;
    settings.positions.GSLeft := Left;
end;

procedure TFGlobalSearch.ListView1DblClick(Sender: TObject);
var sel: String; selIndex: Integer; gsInfo: TGlobalSearchInfo;
begin
    // Display searched messages of the selected mailbox in the main message window
    if ListView1.SelCount > 0 then begin
        sel:=ListView1.Selected.Caption;
        selIndex := FMain.GetAbsoluteMboxIndex(sel);
        gsInfo:=SearchResult.GetByMboxIndex(selIndex);
        devLog.Debug('selected: '+inttostr(mboxTree.Items[gsInfo.mMailboxIndex].mAbsoluteIndex));
        devLog.Debug('that is: '+mboxTree.Items[mboxTree.Items[gsInfo.mMailboxIndex].mAbsoluteIndex].mFullMailboxName);
        FMain.PerformMultipleFetch(iopGetMultipleMessages, selIndex, gsInfo.mMsgUIDs);
    end;
end;


procedure TFGlobalSearch.BitBtn1Click(Sender: TObject);
begin
    PerformGlobalSearch(iopSearchGlobal);
end;

procedure TFGlobalSearch.BitBtn2Click(Sender: TObject);
begin
    InvokeAdvSearchDlg;
end;

end.