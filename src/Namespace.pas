unit Namespace;

{ This unit contains the TNamespace class which encapsulates all of the namespace info
  and also contains the functionality for parsing namespace responses }

interface

uses classes, sysutils, veclog;

const
    STATE_START = 0;                    // Starting state, assumes the NAMESPACE command has been skipped
    STATE_PERSONAL_GOING = 1;           // Processing personal namespace
    STATE_PERSONAL_MULTIPLE_INTER = 2;  // Helper state between two private namespaces
    STATE_SHARED_IS_NEXT = 3;           // Waiting for the begining of shared namespace
    STATE_SHARED_GOING = 4;             // Processing shared namespace
    STATE_PUBLIC_IS_NEXT = 5;           // Waiting for the begining of public namespace
    STATE_PUBLIC_GOING = 6;             // Processing public namespace
    STATE_PUBLIC_MULTIPLE_INTER = 7;    // Helper state between two public namespaces
    STATE_DONE = 8;                     // Final state
    STATE_ERROR = 99;                   // Error state

type
    // Encapsulates all the namespace information
    TNamespace = class
        namespaceStr: String;
        namespaceExists: Boolean;
        personalNSPrefix: String;
        sharedNSPrefix: String;
        publicNSPrefixes: TStringList;
        hasPersonalNS, hasSharedNS, hasPublicNS: Boolean;
        hierarchySeparator: Char;   // The hierarchy separator for the personal namespace
        rootFolder: String;         // User defined root folder. Used only if the user wants to limit the namespace
        limitedNamespace: String;   // The final wildcard used for LIST/LSUB commands (also based on the user defined "root folder")
        // Following two are a hack to allow #mhinbox and #mh private namespaces
        // These should be stored in personalNSPrefixes: TStringList, but no time now to
        // modify the logic in CheckerThread and FolderListProcessor
        hasmhinbox: Boolean;    // True if #mhinbox private namespace is present
        hasmh: Boolean;         // True if #mh/ private namespace is present
    public
        constructor Create(nmStr: String);
        destructor Destroy; override;
        procedure SetRootFolder(inRootFolder: String);
    end;


function parseNamespace(var ns: TNamespace): Boolean;

implementation

constructor TNamespace.Create(nmStr: String);
begin
    namespaceStr:=nmStr;
    namespaceExists := (nmStr<>'');
    SetLength(personalNSPrefix,0);
    SetLength(sharedNSPrefix,0);
    publicNSPrefixes:=TStringList.Create;
    hasPersonalNS:=false;
    hasSharedNS:=false;
    hasPublicNS:=false;
    SetLength(limitedNamespace,0);
    hasmhinbox:=false;
    hasmh:=false;
    hierarchySeparator:=' ';
end;

destructor TNamespace.Destroy;
begin
    publicNSPrefixes.Free;
end;

{ Allows the user to limit the namespace }
procedure TNamespace.SetRootFolder(inRootFolder: String);
begin
    rootFolder:=inRootFolder;
end;

function parseNamespace(var ns: TNamespace): Boolean;
var state, i, openBraceCount: Integer; token: String;
begin
    if ns.namespaceStr<>'' then begin
        // Check if this is a valid NAMESPACE response
        i:=Pos('NAMESPACE',ns.namespaceStr);
        if i>0 then begin
            i := i + 10;
            state:=STATE_START;
            token:='';
            openBraceCount:=0;
            while ((i<=Length(ns.namespaceStr)) and (state<STATE_DONE) and (state<>STATE_ERROR)) do begin
                // Form1.Log.Lines.Add('State '+ IntToStr(state)+ ' Char=['+ns.namespaceStr[i]+']');
                if ns.namespaceStr[i]='(' then Inc(openBraceCount);
                if ns.namespaceStr[i]=')' then Dec(openBraceCount);
                case state of
                    STATE_START:
                        if (ns.namespaceStr[i]='N') and (ns.namespaceStr[i+1]='I') and (ns.namespaceStr[i+2]='L') then begin
                            state:=STATE_SHARED_IS_NEXT;
                            i:=i+2;
                        end
                        else if (ns.namespaceStr[i]='(') and (ns.namespaceStr[i+1]='(') and (ns.namespaceStr[i+2]='"') then begin
                            state:=STATE_PERSONAL_GOING;
                            i:=i+2;
                        end;
                    STATE_PERSONAL_GOING:
                        if ns.namespaceStr[i]='"' then begin
                            ns.personalNSPrefix:=token;
                            if token<>'' then ns.hasPersonalNS:=true;
                            SetLength(token,0);
                            state:=STATE_SHARED_IS_NEXT;
                            // Extract the separator. Example string:
                            // NAMESPACE (("" "/")
                            if (ns.namespaceStr[i+2]='"') and (ns.namespaceStr[i+4]='"') then
                                ns.hierarchySeparator:=ns.namespaceStr[i+3]
                        end
                        else token:=token+ns.namespaceStr[i];
                    STATE_SHARED_IS_NEXT:
                        // Stay in this state until you don't find a NIL or a (("
                        if (ns.namespaceStr[i]='N') and (ns.namespaceStr[i+1]='I') and (ns.namespaceStr[i+2]='L') then begin
                            state:=STATE_PUBLIC_IS_NEXT;
                            i:=i+2;
                        end
                        else if (ns.namespaceStr[i]='(') and (ns.namespaceStr[i+1]='(') and (ns.namespaceStr[i+2]='"') then begin
                            state:=STATE_SHARED_GOING;
                            i:=i+2;
                        end;
                    STATE_SHARED_GOING:
                        if ns.namespaceStr[i]='"' then begin
                            ns.sharedNSPrefix:=token;
                            if token<>'' then ns.hasSharedNS:=true;
                            SetLength(token,0);
                            state:=STATE_PUBLIC_IS_NEXT;
                        end
                        else token:=token+ns.namespaceStr[i];
                    STATE_PUBLIC_IS_NEXT:
                        // Stay in this state until you don't find a NIL or a (("
                        if (ns.namespaceStr[i]='N') and (ns.namespaceStr[i+1]='I') and (ns.namespaceStr[i+2]='L') then begin
                            state:=STATE_DONE;
                            i:=i+2;
                        end
                        else if (ns.namespaceStr[i]='(') and (ns.namespaceStr[i+1]='(') and (ns.namespaceStr[i+2]='"') then begin
                            state:=STATE_PUBLIC_GOING;
                            i:=i+2;
                        end;
                    STATE_PUBLIC_GOING:
                        if ns.namespaceStr[i]='"' then begin
                            if token<>'' then begin
                                ns.publicNSPrefixes.Add(token);
                                ns.hasPublicNS:=true;
                            end;
                            SetLength(token,0);
                            state:=STATE_PUBLIC_MULTIPLE_INTER;
                        end
                        else token:=token+ns.namespaceStr[i];
                    STATE_PUBLIC_MULTIPLE_INTER:
                        // In this state we check if there are any public namespaces left
                        if ns.namespaceStr[i]=')' then begin
                            if openBraceCount=0 then state:=STATE_DONE
                            else state:=STATE_PUBLIC_MULTIPLE_INTER;    // stay in this state
                        end
                        else if (ns.namespaceStr[i]='(') and (ns.namespaceStr[i+1]='"') then begin
                            state:=STATE_PUBLIC_GOING;     // start a new public namespace
                            i:=i+1;
                        end;
                    STATE_DONE: begin end;
                    STATE_ERROR: begin end;
                    else begin
                        // Unknown state
                        state := STATE_ERROR;
                    end;
                end;
                Inc(i);
            end;

            // #mhinbox and #mh/ personal namespace hack. See declaration above for details
            if (Pos('#mhinbox',ns.namespaceStr)>0) then ns.hasmhinbox:=true;
            if (Pos('#mh/',ns.namespaceStr)>0) then ns.hasmh:=true;

        end
        else begin
            // NAMESPACE string not found, error
            state:=STATE_ERROR;
        end;
    end
    else begin
        // Namespace string is empty, no prefixes of any kind
        state:=STATE_DONE;
    end;

    // Set the limited namespace. This will be used as the wildcard in the LIST/LSUB command
    // If there is a root folder specified by the user, set to <rootfolder>*, otherwise to *
    // Basically, always set to <rootfolder>*, whether or not root folder is specified or not
    if not ns.hasPersonalNS then begin
        // This is for cases NAMESPACE (("" "/")... as with UW IMAP
        if ns.rootFolder<>'' then begin
            ns.limitedNamespace:=ns.rootFolder;
            devLog.Info('No personal namespace, root folder specified');
        end;
    end;
    ns.limitedNamespace := ns.limitedNamespace + '*';
    devLog.Info('Limited namespace set to '+ns.limitedNamespace);
    Result:=(state=STATE_DONE);
end;

end.
