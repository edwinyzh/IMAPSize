{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: CustomThread.pas,v 1.6 2004/03/31 23:27:39 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

{ Helper class for getting the list of folders in an account }

unit FolderListProcessor;

interface

uses IMAPSend, IMAPConnection, classes, VecLog, SplitFns, sysutils, namespace;

type
    TFolderListProcessor = class
    private
        mPersonalNamespace: String;  // The personal namespace. '' if it doesn't exist
        function GetSeparator(var connInfo: TConnInfo): Char;
        function GetNamespaceString(var connInfo: TConnInfo): String;
        function GetFullFolderName(listReplyLine: String): String;
        function GetParentFolderName(fName: String): String;
        function ShouldIncludeFolder(folderName: String; ns: TNamespace; includeShared, includePublic: Boolean): Boolean;
    public
        mSeparator: Char;
        function GetMailboxes(var connInfo: TConnInfo; var mboxes: TStringList; subscribedOnly: Boolean): Boolean;
        function GetPersonalNamespace: String;
    end;

implementation

uses RegExpr, GlobalConstants;

{ Make the call to the server and place into 'mboxes' the actual
  lines representing mailboxes (removes any server reply not listing a mailbox)
  Also stores the mailbox separator }
function TFolderListProcessor.GetMailboxes(var connInfo: TConnInfo; var mboxes: TStringList; subscribedOnly: Boolean): Boolean;
var fullReply: TStringList; i,p1,p2,num: Integer; listCommand, fullFolderName, listWildcard: String;
    gotSeparator: Boolean; re: TRegExpr; ns: TNamespace; fName, parentName: String;
begin
    re:=TRegExpr.Create;
    re.Expression:=NUM_LITERAL_RE;

    ns:=TNamespace.Create(GetNamespaceString(connInfo));
    if connInfo.account.RootFolder<>'' then ns.SetRootFolder(connInfo.account.RootFolder);
    ParseNamespace(ns);

    if ns.hasPersonalNS then devLog.Info('Account has personal namespace: YES') else devLog.Info('Account has personal namespace: NO');
    if ns.hasSharedNS then devLog.Info('Account has shared namespace: YES') else devLog.Info('Account has shared namespace: NO');
    if ns.hasPublicNS then devLog.Info('Account has public namespace: YES') else devLog.Info('Account has public namespace: NO');
    mPersonalNamespace:=ns.personalNSPrefix;

    // Get separator
    mSeparator := GetSeparator(connInfo);
    if mSeparator=' ' then mSeparator:=ns.hierarchySeparator;
    connInfo.account.FolderSeparator:=mSeparator;
    devLog.Info('Separator is ['+mSeparator+']');

    mboxes.Clear;
    if subscribedOnly then listCommand:='LSUB' else listCommand:='LIST';
    listWildcard:=ns.limitedNamespace;
    if connInfo.imap.IMAPCommand( listCommand + ' "" "' + listWildcard + '"' )='OK' then begin
        fullReply := connInfo.imap.FullResult;
        for i:=0 to fullReply.Count-1 do begin
            if ( Pos('* ' + listCommand + ' (',fullReply[i]) = 1) then begin
                fullReply[i]:=TrimRight(fullReply[i]);
                // Check if the folder name is a string literal (as oposed to a quoted string)
                if re.Exec(fullReply[i]) then begin
                    num := StrToInt(copy(fullReply[i], re.MatchPos[0]+1, re.MatchLen[0]-2));
                    // Get num characters from the next line
                    fullFolderName:=Copy(fullReply[i+1],0,num);
                end
                else fullFolderName:=GetFullFolderName(fullReply[i]);
                if ShouldIncludeFolder(fullFolderName,ns,connInfo.account.IncludeShared,connInfo.account.IncludePublic) then
                    // Some servers return duplicate folders (Hamster 2.0.6.0)
                    if (mboxes.IndexOf(fullFolderName)=-1) then begin
                        // Escape any \ characters
                        fName:=StringReplace(fullFolderName,'\','\\',[rfReplaceAll]);
                        // Add to mboxes
                        mboxes.add(fName);
                    end;

            end;
        end;
    end
    else begin
        devLog.Error('Server replied negatively to the list command. Can''t list mailboxes.');
    end;

    re.Free;
    ns.Free;
    if subscribedOnly then Result:=true  // allow LSUB count of 0
    else Result:=mboxes.Count>0;
end;

{ Returns the personal namespace retreived during 'GetMailboxes' }
function TFolderListProcessor.GetPersonalNamespace: String;
begin
    Result:=mPersonalNamespace;
end;

{ Extracts the IMAP folder separator. Returns ' ' if the separator could not be extracted from LIST "" ""}
function TFolderListProcessor.GetSeparator(var connInfo: TConnInfo): Char;
var reply: String; r: Char;
begin
    r:=' ';
    // LIST "" "" returns the mailbox separator (e.g. S: * LIST (\Noselect) "/" "" )
    if connInfo.imap.IMAPCommand( 'LIST "" ""' )='OK' then begin
        if connInfo.imap.FullResult.Count>0 then begin
            reply:=connInfo.imap.FullResult[0];
            if (Pos('"',reply)>0) then
                r := reply[Pos('"',reply)+1]
            else devLog.Warn('Could not extract mailbox separator');
        end
    end
    else begin
        devLog.Warn('Server did not return the mailbox separator');
    end;
    Result:=r;
end;

function TFolderListProcessor.GetNamespaceString(var connInfo: TConnInfo): String;
var fullReply: TStringList; namespaceResponse: String;
begin
    SetLength(namespaceResponse,0);
    if (connInfo.imap.FindCap('NAMESPACE') <> '') then begin
        devLog.Info('Server supports NAMESPACE');
        if connInfo.imap.IMAPCommand('NAMESPACE')='OK' then begin
            try
                fullReply := connInfo.imap.FullResult;
                namespaceResponse:=fullReply[0];
            except
                devLog.Warn('Error has occurred while parsing the NAMESPACE response. Will ignore namespace info');
            end;
        end
        else begin
            devLog.Warn('Server sais it supports NAMESPACE, but it doesn''t recognize the command');
        end;
    end;
    if (namespaceResponse='') then devLog.Info('Server doesn''t support NAMESPACE');
    Result:=namespaceResponse;
end;

{ Extracts the folder name from a reply to the List command.
  E.g. LIST (\HasNoChildren) "." "INBOX.personal.dad" -> INBOX.personal.dad
  The Mailbox name is quoted if spaces are allowed, otherwise it can appear
  unquoted }
function TFolderListProcessor.GetFullFolderName(listReplyLine: String): String;
var mbox: String; separator: Char; parts: TStringList;
begin
    mbox:='Unknown';
    if listReplyLine[Length(listReplyLine)] = '"' then separator := '"'
    else separator := ' ';
    parts := TStringList.Create;
    try
        Split(listReplyLine,separator,parts);
        mbox := parts[parts.Count-1];
    finally
        parts.Free;
        Result:=mbox;
    end;
end;

function TFolderListProcessor.GetParentFolderName(fName: String): String;
var parentName: String; parts: TStringList; i: Integer;
begin
    parentName := '';
    parts := TStringList.Create;
    try
        Split(fName,mSeparator,parts);
        if parts.Count>1 then begin
            for i:=0 to parts.Count-2 do parentName:=parentName+parts[i]+mSeparator;
            parentName:=Copy(parentName,0,Length(parentName)-1);
        end;
    finally
        parts.Free;
        Result:=parentName;
    end;
end;

{ This method returns true if the folder should be included in the retrieval }
function TFolderListProcessor.ShouldIncludeFolder(folderName: String; ns: TNamespace; includeShared, includePublic: Boolean): Boolean;
var include: Boolean; i: Integer; personalPrefixWithoutSeparator: String;

    function isSharedFolder: Boolean;
    begin
        if ns.hasSharedNS then
            Result:= Copy(folderName,0,Length(ns.sharedNSPrefix)) = ns.sharedNSPrefix
        else
            Result:=false;
    end;

    function isPublicFolder: Boolean;
    var found: Boolean;
    begin
        found:=false;
        if ns.hasPublicNS then begin
            // Check all public namespaces
            i:=0;
            while not found and (i<ns.publicNSPrefixes.count) do begin
                if (Copy(folderName,0,Length(ns.publicNSPrefixes[i])) = ns.publicNSPrefixes[i]) then found:=true;
                Inc(i);
            end;
        end;
        Result:=found;
    end;

begin
    include:=false;
    if ns.namespaceExists then begin
        // First check if foldername belongs to the personal namespace
        // Have to check the personal prefix without the separator
        personalPrefixWithoutSeparator := Copy(ns.personalNSPrefix,0,Length(ns.personalNSPrefix)-1);
        if (Copy(folderName,0,Length(personalPrefixWithoutSeparator)) = personalPrefixWithoutSeparator) then begin
            if ns.hasPersonalNS then include:=true
            else begin
                // If the personal namespace doesn't exist, we have to check if this folder is a shared or public one
                if not isSharedFolder and not isPublicFolder then include:=true
                else if isSharedFolder and includeShared then include:=true
                else if isPublicFolder and includePublic then include:=true;
            end;
        end
        else if includeShared and ns.hasSharedNS and isSharedFolder then
            include:=true
        else if includePublic and ns.hasPublicNS and isPublicFolder then begin
            include:=true;
        end;
    end
    else begin
        // If the namespaces are not defined, all folders should be included
        include:=true;
    end;

    Result:=include;
end;



end.
