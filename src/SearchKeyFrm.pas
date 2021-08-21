{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: SearchKeyFrm.pas,v 1.3 2004/03/31 23:27:32 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit SearchKeyFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, SearchKeys, SavedSearches, VecLog, SplitFns;

const

  RFCMonthNames: array[1..12] of string =
    ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');

type

  EInvalidData = class(Exception);

  TSearchKeyFrame = class(TFrame)
    ComboBoxSearchKeys: TComboBox;
    ComboBoxRelations: TComboBox;
    ComboBoxValues: TComboBox;
    EditBoxRelations: TEdit;
    EditBoxValues: TEdit;
    procedure ComboBoxSearchKeysChange(Sender: TObject);
  private
    { Private declarations }
    function getRFCDate(dateStr: String): String;
  public
    constructor Create(AOwner: TComponent); override;
    function HasNeededInfo: Boolean;
    procedure GetSearchKeyFromFields(var savedKey: TSavedSearchKey);
    procedure Reset;
    procedure SetValues(pSavedSearchKey: TSavedSearchKey);
    function GetSearchString: String;
  end;

implementation

uses Main;

{$R *.DFM}


constructor TSearchKeyFrame.Create(AOwner: TComponent);
var names: TStringList;
begin
    Inherited Create(AOwner);
    names := TStringList.Create;
    srchKeys.GetSearchKeyNames(names);
    ComboBoxSearchKeys.Items.Clear;
    ComboBoxSearchKeys.Items.AddStrings(names);
    ComboBoxSearchKeys.ItemIndex:=0;
    ComboBoxSearchKeysChange(Self);
    names.Free;
end;


procedure TSearchKeyFrame.ComboBoxSearchKeysChange(Sender: TObject);
var searchKey: TSearchKeyInfo;
begin
    searchKey := srchKeys.GetSearchKey(ComboBoxSearchKeys.Items[ComboBoxSearchKeys.ItemIndex]);

    if searchKey.Relations.Count>0 then begin
        ComboBoxRelations.Visible := true;
        EditBoxRelations.Visible := false;
        ComboBoxRelations.Items.Clear;
        ComboBoxRelations.Items.AddStrings(searchKey.Relations);
        ComboBoxRelations.ItemIndex:=0;
    end
    else begin
        ComboBoxRelations.Visible := false;
        EditBoxRelations.Visible := true;
        EditBoxRelations.Text:='';
    end;

    if searchKey.ValueList.Count>0 then begin
        ComboBoxValues.Visible := true;
        EditBoxValues.Visible := false;
        ComboBoxValues.Items.Clear;
        ComboBoxValues.Items.AddStrings(searchKey.ValueList);
        ComboBoxValues.ItemIndex:=0;
    end
    else begin
        ComboBoxValues.Visible := false;
        EditBoxValues.Visible := true;
        EditBoxValues.Text:='';
    end;
end;

{ Returns true if the search key is filled with all necessary info,
  ie. no blank edit boxes, etc. }
function TSearchKeyFrame.HasNeededInfo: Boolean;
begin
    Result:=false;
    if ComboBoxSearchKeys.ItemIndex > -1 then begin
        if ((ComboBoxRelations.Visible) and (ComboBoxRelations.ItemIndex>-1)) or
           ((EditBoxRelations.Visible) and (EditBoxRelations.Text<>''))
        then
            if ((ComboBoxValues.Visible) and (ComboBoxValues.ItemIndex>-1)) or
               ((EditBoxValues.Visible) and (EditBoxValues.Text<>''))
            then
                Result:=true;
    end;
end;

{ Extracts the information from this keys fields and populates the
  var parameter (TSavedSearchKey) }
procedure TSearchKeyFrame.GetSearchKeyFromFields(var savedKey: TSavedSearchKey);
begin
    savedKey.FieldNameIndex := ComboBoxSearchKeys.ItemIndex;
    if (ComboBoxRelations.Visible) then begin
        savedKey.RelationIndex := ComboBoxRelations.ItemIndex;
        savedKey.Relation := '';
    end
    else begin
        savedKey.RelationIndex := -1;
        savedKey.Relation := EditBoxRelations.Text;
    end;
    if (ComboBoxValues.Visible) then begin
        savedKey.ValueIndex := ComboBoxValues.ItemIndex;
        savedKey.Value := '';
    end
    else begin
        savedKey.ValueIndex := -1;
        savedKey.Value := EditBoxValues.Text;
    end;
end;

{ Forms the search string from the fields on this form
  This string should be put together with other ones to
  produce the IMAP search string that will be sent to
  the server
  @throws Exception if there is not enough data or if some
          data is not well formatted (e.g. Date) }
function TSearchKeyFrame.GetSearchString: String;
var s: String; v: String;
begin
    s:='';
    if HasNeededInfo then begin

        if Pos(' ',EditBoxValues.Text)>0 then
            // Value contains spaces, enclose in quotes
            v:='"'+EditBoxValues.Text+'"'
        else
            // No spaces, no need for quotes
            v:=EditBoxValues.Text;

        case ComboBoxSearchKeys.ItemIndex of
            KEY_IDX_SUBJECT:
                begin
                    if ComboBoxRelations.ItemIndex > 0  then s:='NOT ';
                    s := s + 'SUBJECT ' + v;
                end;
            KEY_IDX_BODY:
                begin
                    if ComboBoxRelations.ItemIndex > 0  then s:='NOT ';
                    s := s + 'BODY ' + v;
                end;
            KEY_IDX_ENTIRE_MESSAGE:
                begin
                    if ComboBoxRelations.ItemIndex > 0  then s:='NOT ';
                    s := s + 'TEXT '+ v;
                end;
            KEY_IDX_SENDER:
                begin
                    if ComboBoxRelations.ItemIndex > 0  then s:='NOT ';
                    s := s + 'FROM ' + v;
                end;
            KEY_IDX_RECIPIENT:
                begin
                    if ComboBoxRelations.ItemIndex > 0  then s:='NOT ';
                    s:=s+'OR TO ' + v + ' OR CC ' + v + ' BCC ' + v;
                end;
            KEY_IDX_CORRESPONDENT:
                begin
                    //@todo should expand with CC, BCC and FROM
                    if ComboBoxRelations.ItemIndex > 0  then s:='NOT ';
                    s:=s+'OR TO '+v+' OR CC '+v+' OR BCC '+v+' FROM '+v;
                end;
            KEY_IDX_HEADER_FIELD:
                begin
                    s:='HEADER "'+EditBoxRelations.Text+'" '+v;
                end;
            KEY_IDX_DATE:
                // 'is','is before', 'is after'
                begin
                    case ComboBoxRelations.ItemIndex of
                        0: s:='SENTON ';
                        1: s:='SENTBEFORE ';
                        2: s:='SENTSINCE ';
                    end;
                    s:= s+ getRFCDate(EditBoxValues.Text);
                end;
            KEY_IDX_SIZE:
                begin
                    if ComboBoxRelations.ItemIndex = 0 then s:='LARGER '
                    else s:='SMALLER ';
                    s:=s+EditBoxValues.Text;
                end;
            KEY_IDX_STATUS:
                begin
                    if ComboBoxRelations.ItemIndex > 0  then s:='UN'; // no space here
                    case ComboBoxValues.ItemIndex of
                        // 'seen', 'flagged', 'deleted', 'draft', 'replied'
                        0: s := s + 'SEEN';
                        1: s := s + 'FLAGGED';
                        2: s := s + 'DELETED';
                        3: s := s + 'DRAFT';
                        4: s := s + 'ANSWERED';
                        5: begin // stupid IMAP rfc, there is no UNRECENT
                                if Length(s)>0 then s:='NOT ';
                                s:=s+'RECENT';
                           end;
                    else
                        devLog.Error('Unknown index in ComboBoxRelations '+IntToStr(ComboBoxRelations.ItemIndex));
                    end;
                end;
            KEY_IDX_KEYWORD_FLAG:
                begin
                    s:='KEYWORD "'+EditBoxRelations.Text+'" '+v;
                end;
        else
            devLog.error('Unknown key index: '+IntToStr(ComboBoxSearchKeys.ItemIndex));
        end;
    end
    else begin
        devLog.Warn('No valid info');
        // No need to raise exception, it is sufficient that we are not returning a string
        // raise Exception.Create('You did not provide enough information.');
    end;
    Result:=s;
end;

{ Resets the content - first combo index to 0 }
procedure TSearchKeyFrame.Reset;
begin
    ComboBoxSearchKeys.ItemIndex:=0;
    ComboBoxSearchKeysChange(Self);
end;

{ Sets the values to the components }
procedure TSearchKeyFrame.SetValues(pSavedSearchKey: TSavedSearchKey);
begin
    if pSavedSearchKey <> nil then begin

        ComboBoxSearchKeys.ItemIndex := pSavedSearchKey.FieldNameIndex;
        ComboBoxSearchKeysChange(Self);

        if (ComboBoxRelations.Visible) then
            ComboBoxRelations.ItemIndex := pSavedSearchKey.RelationIndex
        else
            EditBoxRelations.Text := pSavedSearchKey.Relation;

        if (ComboBoxValues.Visible) then
            ComboBoxValues.ItemIndex := pSavedSearchKey.ValueIndex
        else
            EditBoxValues.Text := pSavedSearchKey.Value;
    end;
end;

{ Converts the user entered string (format dd/mm/yyyy) to
  the stripped RFC822 date supported by IMAP searches }
function TSearchKeyFrame.getRFCDate(dateStr: String): String;
var parts: TStringList; day,month,year: Integer;
begin
    parts:=TStringList.Create;
    Split(dateStr,'/',parts);
    if (parts.count <> 3) then
        raise EInvalidData.Create('Wrong date format');
    try
        day := StrToInt(parts[0]);
        month := StrToInt(parts[1]);
        year := StrToInt(parts[2]);
        if (day<1) or (day>31) then raise Exception.Create('Invalid day');
        if (month<1) or (month>12) then raise Exception.Create('Invalid month');
        // add 20xx if only two digits were provided
        if (year div 100 = 0) then year:=2000+year;
        Result := Format('%d-%s-%d', [day, RFCMonthNames[month], year]);
    finally
        parts.Free;
    end;
end;

end.