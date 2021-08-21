{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: AdvSearchDlg.pas,v 1.3 2004/03/31 23:27:39 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit AdvSearchDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, SearchKeyFrm, SavedSearches, VecLog;

type

  TFAdvSearchDlg = class(TForm)
    Panel1: TPanel;
    ButtonSearch: TButton;
    ButtonClear: TButton;
    Panel2: TPanel;
    RadioButtonOR: TRadioButton;
    ButtonMore: TButton;
    ButtonLess: TButton;
    ButtonClose: TButton;
    GroupBox1: TGroupBox;
    CBSearchSets: TComboBox;
    ButtonAddSearch: TButton;
    ButtonDelSearch: TButton;
    Panel3: TPanel;
    ScrollBox1: TScrollBox;
    Frm0: TSearchKeyFrame;
    RadioButtonAND: TRadioButton;
    procedure ButtonSearchClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonMoreClick(Sender: TObject);
    procedure ButtonLessClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonAddSearchClick(Sender: TObject);
    procedure ButtonDelSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBSearchSetsChange(Sender: TObject);
  private
    searchKeyFrames: TList;
    function ValidityCheckOK: Boolean;
    procedure ClearSearches;
    procedure _ButtonMoreClick(resetSearchSetIndex: Boolean);
  public
    SearchString: String;
  end;

var
  FAdvSearchDlg: TFAdvSearchDlg;

implementation

uses Main;

{$R *.DFM}


procedure TFAdvSearchDlg.FormCreate(Sender: TObject);
var s: TStringList;
begin
    searchKeyFrames:=TList.Create;
    searchKeyFrames.Add(Frm0);
    // Add an emtpy string
    // CBSearchSets.Items.Add('');
    // Then add any saved searches
    s := TStringList.Create;
    settings.SavedSearches.GetSavedSearchNames(s);
    CBSearchSets.Items.AddStrings(s);
    s.Free;
end;

procedure TFAdvSearchDlg.FormDestroy(Sender: TObject);
var i: Integer;
begin
    for i:=0 to searchKeyFrames.Count-1 do begin
        TSearchKeyFrame(searchKeyFrames[i]).Free;
    end;
    searchKeyFrames.Free;
end;

{ Perform search }
procedure TFAdvSearchDlg.ButtonSearchClick(Sender: TObject);
var i: Integer; s, temp: String;
begin
    if ValidityCheckOK then begin
        try
            s:='UID SEARCH ';
            for i:=0 to searchKeyFrames.Count-1 do begin
                temp := TSearchKeyFrame(searchKeyFrames[i]).GetSearchString;
                if temp<>'' then begin
                    // If OR is chosen and it's not the last key, prefix with OR
                    if ((RadioButtonOr.Checked) and (i<(searchKeyFrames.Count-1))) then s:=s+'OR ';
                    s:=s+temp;
                end;
                if (i<searchKeyFrames.Count-1) then s:=s+' ';
                //devLog.Trace(TSearchKeyFrame(searchKeyFrames[i]).GetSearchString);
            end;
            devLog.trace('Complete search string: '+s);

            // Make the string available to the caller through the SearchString global var
            SearchString := s;

            // Close the dialog
            // Once created, this dialog remains in memory
            ModalResult:=mrOK;
        except
            on E:Exception do begin
                devLog.warn('Invalid data for search: '+E.Message);
                MessageDlg('You have entered invalid data ('+E.message+'). Please enter valid values.',mtWarning,[mbOK],0);
            end;
        end;
    end;
end;

{ Clear search }
procedure TFAdvSearchDlg.ButtonClearClick(Sender: TObject);
begin
    ClearSearches;
    // Clear selected search set combo (in case a set is selected)
    CBSearchSets.ItemIndex := -1;
end;

{ More rules }
procedure TFAdvSearchDlg.ButtonMoreClick(Sender: TObject);
begin
    _ButtonMoreClick(true);
end;

{ We need this private method since it is called from CBSearchSetChange
  when no clearance of index selection should be performed }
procedure TFAdvSearchDlg._ButtonMoreClick(resetSearchSetIndex: Boolean);
var Fr: TSearchKeyFrame; num: Integer;
begin
    num:=searchKeyFrames.Count;
    Fr := TSearchKeyFrame.Create(Self);
    Fr.Parent := ScrollBox1;
    Fr.Left:=5;
    Fr.Top:=num*40; // +10?
    Fr.Name:='Frm'+IntToStr(num);
    searchKeyFrames.Add(Fr);
    ButtonLess.Enabled:= (searchKeyFrames.Count>1);
    if resetSearchSetIndex then begin
        // Clear selected search set combo (in case a set is selected)
        CBSearchSets.ItemIndex := -1;
    end;
end;

{ Less rules }
procedure TFAdvSearchDlg.ButtonLessClick(Sender: TObject);
var id,i: Integer; comp: TComponent;
begin
    id:=searchKeyFrames.Count-1;
    if id > 0 then begin
        name := TSearchKeyFrame(searchKeyFrames[id]).Name;
        TSearchKeyFrame(searchKeyFrames[id]).Free;
        searchKeyFrames.Delete(id);
        // Clear selected search set combo (in case a set is selected)
        CBSearchSets.ItemIndex := -1
    end;
    // else do nothing, can't remove if only one left...

    ButtonLess.Enabled:= (searchKeyFrames.Count>1);
end;

{ Close }
procedure TFAdvSearchDlg.ButtonCloseClick(Sender: TObject);
begin
    ModalResult:=mrCancel;
end;

{ Add a search set based on the current values in the dialog }
procedure TFAdvSearchDlg.ButtonAddSearchClick(Sender: TObject);
var name: String; i: Integer; found: Boolean; ss: TSavedSearch;
    ssKeyItem: TSavedSearchKeyItem; savedKey: TSavedSearchKey;
begin
    if ValidityCheckOK then begin
        name := InputBox('Save Search Set','Enter the name for this search set', '');
        if name<>'' then begin
            // Go through the combo and check if a set with that name already exists...
            found := false;
            for i:=0 to CBSearchSets.Items.Count-1 do begin
                if CBSearchSets.Items[i] = name then begin
                    found := true;
                    break;
                end;
            end;
            if found then begin
                MessageDlg('A search set with the specified name already exists. Please choose another name.',mtInformation,[mbOK],0);
            end
            else begin
                // Create a TSavedSearch that corresponds to the data in the SearchKeyFrames
                ss := TSavedSearch.Create(settings.SavedSearches.SavedSearches);  // will be freed by its collection
                ss.Name := name;
                if (RadioButtonAnd.Checked) then ss.AndOr:=0
                else ss.AndOr:=1;
                for i:=0 to searchKeyFrames.Count-1 do begin
                    if TSearchKeyFrame(searchKeyFrames[i]).HasNeededInfo then begin
                        savedKey := TSavedSearchKey.Create;  // temporary data holder
                        // Get the key from data on form
                        TSearchKeyFrame(searchKeyFrames[i]).GetSearchKeyFromFields(savedKey);
                        ss.AddSearchKey(savedKey);   // will add a new savedkeyitem and copy the data from savedKey (can be freed safely now)
                        // free temporary object
                        savedKey.Free;
                    end;
                end;

                // Add the new search set name to the list of sets
                CBSearchSets.Items.Add(name);
                // And select it (the last one
                CBSearchSets.ItemIndex := CBSearchSets.Items.Count-1;
            end;
        end
        else MessageDlg('Please specify a valid name',mtInformation,[mbOK],0);
    end;
end;

{ Remove the selected search set }
procedure TFAdvSearchDlg.ButtonDelSearchClick(Sender: TObject);
begin
    if (CBSearchSets.ItemIndex>-1) then begin
        if MessageDlg('Are you sure you want to delete the search set "'+CBSearchSets.Items[CBSearchSets.ItemIndex]+'"?',mtConfirmation,mbYesNoCancel,0) = mrYes
        then begin
            settings.SavedSearches.RemoveSavedSearch(CBSearchSets.Items[CBSearchSets.ItemIndex]);
            CBSearchSets.Items.Delete(CBSearchSets.ItemIndex);
            CBSearchSets.ItemIndex:=-1; // reset to none selected (the empty set name)
            CBSearchSets.Repaint;
            ClearSearches;
        end;
    end
    else begin
        MessageDlg('Nothing to remove. Please select a search set you want to remove.',mtInformation,[mbOK],0);
    end;
end;

{ Checks if there are any non-valid keys and prompts for
  users assistance if needed. Returns true if the caller
  method can procede with the operation }
function TFAdvSearchDlg.ValidityCheckOK: Boolean;
var i, totalKeys, validKeys: Integer;
begin
    Result:=false;

    // First check if any of the search frames doesn't have all the necessary info
    totalKeys:=0;
    validKeys:=0;
    for i:=0 to searchKeyFrames.Count-1 do begin
        Inc(totalKeys);
        if TSearchKeyFrame(searchKeyFrames[i]).HasNeededInfo then Inc(validKeys);
    end;

    if (totalKeys>0) then begin
        if (validKeys = totalKeys) then begin
            Result:=true;
        end
        else if (validKeys>0) then begin
            // Not all keys are valid
            Result := (MessageDlg('Some search keys are not valid (missing values). Proceed anyway?',mtConfirmation,[mbYes,mbNo],0) = mrYes);
        end
        else begin
            // None of the keys are valid
            MessageDlg('None of the search keys are valid. Please enter valid values.',mtWarning,[mbOK],0);
        end
    end
    else devLog.error('AdvSearchDlg.ValidityCheck: Total keys in search is 0');
end;

procedure TFAdvSearchDlg.CBSearchSetsChange(Sender: TObject);
var pSavedSearch: TSavedSearch; ptrSavedSearchKey: TSavedSearchKey;
    i: Integer;
begin
    // First clear searches,
    ClearSearches;

    // Delete the first one too
    TSearchKeyFrame(searchKeyFrames[0]).Free;
    searchKeyFrames.Delete(0);

    // Then repopulate
    pSavedSearch := TSavedSearch(settings.SavedSearches.GetSavedSearch(CBSearchSets.Items[CBSearchSets.ItemIndex]));
    devLog.Trace('Saved search name: '+pSavedSearch.Name);

    // and = 0, or = 1
    if pSavedSearch.AndOr = 0 then RadioButtonAnd.Checked:=true
    else RadioButtonOr.Checked:=true;

    for i:=0 to pSavedSearch.SavedSearchKeys.Count-1 do begin
        ptrSavedSearchKey := TSavedSearchKeyItem(pSavedSearch.SavedSearchKeys.Items[i]).SavedSearchKey;

        _ButtonMoreClick(false);
        TSearchKeyFrame(searchKeyFrames[searchKeyFrames.Count-1]).SetValues(ptrSavedSearchKey);
    end;
end;


{ Clear all search keys except the first one
  On the first one set the first combo to index = 0 }
procedure TFAdvSearchDlg.ClearSearches;
var i: Integer;
begin
    for i:=searchKeyFrames.Count-1 downto 1 do begin
        TSearchKeyFrame(searchKeyFrames[i]).Free;
        searchKeyFrames.Delete(i); //@todo need this line?
    end;
    TSearchKeyFrame(searchKeyFrames[i]).Reset;
end;

end.