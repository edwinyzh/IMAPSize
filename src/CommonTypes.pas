{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id$
|==============================================================================|
| Copyright 2003-2006 Ivan Vecanski                                            |
| =============================================================================}

unit CommonTypes;

interface

type

    TBackupFolderInfo = record
        remoteFolder: String;
        localFolder: String;
        numMessages: Integer;
        lastUpdated: String;
        newRemoteName: String; // Used to allow the user to change the name of folders
    end;
    PBackupFolderInfo = ^TBackupFolderInfo;
    TListOfBackupFolderInfos = array of TBackupFolderInfo;

    TMboxMessageInfo = record
        lineNum: String;
        fromLine: String;
        subject: String;
    end;
    PMboxMessageInfo = ^TMboxMessageInfo;
    TListOfMboxMessageInfo = array of TMboxMessageInfo;

implementation

end.
 