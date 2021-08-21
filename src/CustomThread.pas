{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: CustomThread.pas,v 1.6 2004/03/31 23:27:39 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit CustomThread;

interface

uses Classes, windows;

type

TThreadType = (thtNone, thtIMAPWorker, thtChecker, thtConverter);
//Note: thtNone is used for creating non-TCustomThread-ed requests (login,logout)

TMessageLevel = (mlvInfo, mlvWarn, mlvError);

TThreadInfo = record
    ID: Integer;
    ThreadType: TThreadType;
    Done: Boolean;  // True if the thread has finished its task (successfully or not,
                    // terminated or not). It is used in determining the shutdown mechanism
                    // !!! NEVER set this directly (instead call markDone in the ThreadMgr)
                    // Fix this bad design...
    WasTerminated: Boolean;  // true if the thread was terminated
    requestID : Integer;    // ID of the request managing this thread. Should be -1 if not defined...
end;

{ An ordinary thread with some additional info }
TCustomThread = class(TThread)
  public
    ThreadInfo: TThreadInfo;
    constructor Create(CreateSuspended: Boolean; threadType: TThreadType);
end;

procedure CopyThreadInfo(var toInfo, fromInfo: TThreadInfo);

implementation

constructor TCustomThread.Create(CreateSuspended: Boolean; threadType: TThreadType);
begin
    Inherited Create(CreateSuspended);
    FreeOnTerminate := false;
    ThreadInfo.ThreadType := threadType;
    ThreadInfo.Done := false;
    ThreadInfo.WasTerminated := false;
    ThreadInfo.requestId := -1; // default value
end;

procedure CopyThreadInfo(var toInfo, fromInfo: TThreadInfo);
begin
    toInfo.ID := fromInfo.ID;
    toInfo.ThreadType := fromInfo.ThreadType;
    toInfo.Done := fromInfo.Done;
    toInfo.WasTerminated := fromInfo.WasTerminated;
end;

end.