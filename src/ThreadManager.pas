{==============================================================================|
| Project : IMAPSize                                                           |
|==============================================================================|
| $Id: ThreadManager.pas,v 1.8 2004/03/31 23:27:32 Ivan Exp $
|==============================================================================|
| Copyright 2003-2004 Ivan Vecanski                                            |
| =============================================================================}

unit ThreadManager;

{ Unit that manages thread invocations. Manages all app threads except LoginThreads }

interface

uses VecLog, Log,
     sysutils, classes, windows,
     CustomThread, CheckerThread, IMAPWorker, IMAPConnection, Converter;

type

EThreadNotFound = class(Exception);

TThreadManager = class
  private
    fIdCounter: Integer; // universal id for all thread types (custom threads and login threads)
    fThreads: TList;     // list of TCustomThread
    // Counters of active threads
    fActiveCheckerThreadCounter: Integer;
    fActiveIMAPWorkerCounter: Integer;
    fActiveConverterThreadCounter: Integer;
    procedure _markDone(index: Integer);
    procedure _removeThread(index: Integer);
    procedure _terminateThread(index: Integer);
    function getThreadIndex(id: Integer): Integer;
    function getNextId: Integer;
    procedure dumpContents(title: String);
    procedure dumpActiveContents;
    procedure logThreadNotFound;
  public
    constructor Create;
    destructor Destroy; override;
    function createCheckerThread: TCheckerThread;
    function createIMAPWorkerThread: TIMAPWorker;
    function createConverterThread: TConverterThread;
    function createThread(threadType: TThreadType): TCustomThread;
    function canCreateThread(threadType: TThreadType): Boolean;
    procedure markDone(id: Integer);
    function areAnyThreadsRunning: Boolean;
    function getThreadInfo(id: Integer): TThreadInfo;
    function getThread(id: Integer): TCustomThread;
    procedure removeThread(id: Integer);
    procedure terminateThread(id: Integer);
    procedure terminateAllThreads;
  published
    property ActiveCheckerThreadCounter: Integer read fActiveCheckerThreadCounter;
    property ActiveIMAPWorkerCounter: Integer read fActiveIMAPWorkerCounter;
    property ActiveConverterThreadCounter: Integer read fActiveConverterThreadCounter;
end;


implementation

uses Main;

constructor TThreadManager.Create;
begin
    fIdCounter:=0;
    fThreads:=TList.Create;
    fThreads.Capacity:=15;   // initial capacity
    fActiveCheckerThreadCounter:=0;
    fActiveIMAPWorkerCounter:=0;
    fActiveConverterThreadCounter:=0;
end;

destructor TThreadManager.Destroy;
var i: Integer;
begin
    devLog.Trace('TThreadManager.Destroy');
    devLog.Trace('Have '+IntToStr(fThreads.Count)+' threads to free');
    for i:=fThreads.Count-1 downto 0 do begin
        devLog.Trace('Freeing thread '+IntToStr(i));
        if fThreads[i]<>nil then begin
            _removeThread(i);
        end;
    end;
    devLog.Trace('TThreadManager.Destroy - Individual threads freed');
    fThreads.Free;
    devLog.Trace('TThreadManager.Destroy - fThreads freed');
end;

{ Returns true if the thread of the specified type can be created }
function TThreadManager.canCreateThread(threadType: TThreadType): Boolean;
begin

    if threadType = thtConverter then result:=true   // always allow
    else
        result:= ((fActiveCheckerThreadCounter=0) and (fActiveIMAPWorkerCounter=0));
    { This can't work properly without a connection pool
    case threadType of
        thtChecker: result:= ((fActiveCheckerThreadCounter=0) and (fActiveIMAPWorkerCounter=0) and (fActiveLoginThreadCounter=0));
        // We can invoke the IMAPWorker thread if the checker thread is running...
        thtIMAPWorker: result:= ((fActiveIMAPWorkerCounter=0) and (fActiveLoginThreadCounter=0));
        thtIMAPLogin: result:= ((fActiveCheckerThreadCounter=0) and (fActiveIMAPWorkerCounter=0) and (fActiveLoginThreadCounter=0));
    end;
    }
end;

{ private method that creates a thread of the specified type }
function TThreadManager.createThread(threadType: TThreadType): TCustomThread;
var newThread: TCustomThread;
begin
    case threadType of
        thtChecker: begin
                        newThread := TCheckerThread.Create(true);
                        Inc(fActiveCheckerThreadCounter);
                    end;
        thtIMAPWorker: begin
                        newThread := TIMAPWorker.Create(true);
                        Inc(fActiveIMAPWorkerCounter);
                    end;
        thtConverter: begin
                        newThread := TConverterThread.Create(true);
                        Inc(fActiveConverterThreadCounter);
                    end
    else devLog.Error('Unknown thread type');
    end;
    // add it to the list
    newThread.ThreadInfo.ID:=GetNextID;
    fThreads.Add(newThread);
    Result:=newThread;
end;

function TThreadManager.createCheckerThread(): TCheckerThread;
begin
    Result := createThread(thtChecker) as TCheckerThread;
end;

function TThreadManager.createIMAPWorkerThread(): TIMAPWorker;
begin
    Result := createThread(thtIMAPWorker) as TIMAPWorker;
end;

function TThreadManager.createConverterThread(): TConverterThread;
begin
    Result := createThread(thtConverter) as TConverterThread;
end;

{ Marks a thread with the specified ID as Done and adjusts the active counter }
procedure TThreadManager.MarkDone(id: Integer);
var index: Integer;
begin
    devLog.Trace('Marking thread id='+IntToStr(id)+' as done');
    try
        index:=getThreadIndex(id);
        _MarkDone(index);
    except
        on EThreadNotFound do logThreadNotFound;
    end;
end;

procedure TThreadManager._MarkDone(index: Integer);
begin
    devLog.Trace('_Marking done - index: '+IntTostr(index));
    if fThreads[index]<>nil then begin
        if (not TCustomThread(fThreads[index]).ThreadInfo.Done) then begin
            TCustomThread(fThreads[index]).ThreadInfo.Done := true;
            // Decrement the active threads counter
            case TCustomThread(fThreads[index]).ThreadInfo.ThreadType of
                thtChecker: begin Dec(fActiveCheckerThreadCounter); end;
                thtIMAPWorker: begin Dec(fActiveIMAPWorkerCounter); end;
                thtConverter: begin Dec(fActiveConverterThreadCounter); end;
            end;
        end;
    end;
end;

{ Public method that terminates a thread with the given id }
procedure TThreadManager.terminateThread(id: Integer);
var index: Integer;
begin
    devLog.Trace('terminateThread '+IntToStr(id));
    try
        index:=getThreadIndex(id);
        _terminateThread(index);
    except
        on EThreadNotFound do logThreadNotFound;
    end;
end;

{ Private method that terminates (sets the terminate flag) the thread at the specified position in the queue }
procedure TThreadManager._terminateThread(index: Integer);
begin
    devLog.Trace('_terminateThread');
    TCustomThread(fThreads[index]).Terminate;
    TCustomThread(fThreads[index]).ThreadInfo.WasTerminated:=true;
    _MarkDone(index);
end;

{ Invokes termination of all threads. Used for abortion of all running operations }
procedure TThreadManager.terminateAllThreads;
var i: Integer;
begin
    devLog.Trace('terminating all threads');
    //dumpActiveContents;
    for i:=0 to fThreads.Count-1 do begin
        if fThreads[i]<>nil then
            _terminateThread(i);
    end;
    devLog.Trace('threads terminated');
    //dumpActiveContents;
end;

{ Removes the thread with the specified id from the queue }
procedure TThreadManager.removeThread(id: Integer);
var index: Integer;
begin
    devLog.Trace('Removing thread '+IntToStr(id));
    try
        index:=getThreadIndex(id);
        _removeThread(index);
    except
        on EThreadNotFound do logThreadNotFound;
    end;
end;

{ Removes the thread with the specified index in the queue from the queue }
procedure TThreadManager._removeThread(index: Integer);
begin
    if (not TCustomThread(fThreads[index]).ThreadInfo.Done) then begin
        // This should happen on application exit only,
        // in all other situations the thread will be marked as done prior to removal from the list
        devLog.Warn('_removeThread forcing thread termination');
        // Mark the thread done
        _MarkDone(index);
        // Force abrupt termination.
        TerminateThread ( TCustomThread(fThreads[index]).Handle );
    end
    else begin
        // Just make sure the thread has finished its business...
        // TCustomThread(fThreads[index]).WaitFor;
        // Force termination... This was needed for issue http://www.broobles.com/mantis/view.php?id=36
        // For TConverterThreads keep the WaitFor (causing problems described in http://www.broobles.com/mantis/view.php?id=39)
        // Ideally, would have to somehow make the hanging thread terminate (@todo)
        if (TCustomThread(fThreads[index]).threadInfo.threadType = thtConverter) then
            TCustomThread(fThreads[index]).WaitFor
        else begin
            _MarkDone(index);
            TerminateThread ( TCustomThread(fThreads[index]).Handle );
        end;
    end;
    if Assigned(TCustomThread(fThreads[index])) then TCustomThread(fThreads[index]).Free;
    fThreads.Delete(index);
    devLog.Trace('Thread at ['+IntToStr(index)+'] removed');
end;

{ Gets the thread information of the thread with the specified id.
  Returns nil if not found }
function TThreadManager.getThreadInfo(id: Integer): TThreadInfo;
var index: Integer;
begin
    index:=getThreadIndex(id);
    try
        result:=TCustomThread(fThreads[index]).ThreadInfo;
    except
        on EThreadNotFound do logThreadNotFound;
    end;
end;

{ Gets the thread with the specified id.
  Returns nil if not found }
function TThreadManager.getThread(id: Integer): TCustomThread;
var index: Integer;
begin
    //@todo check compiler warning (return value might be undefined)
    index:=getThreadIndex(id);
    try
        result:=TCustomThread(fThreads[index]);
    except
        on EThreadNotFound do logThreadNotFound;
    end;
end;

{ Retrieves the index in the array where the thread with
  the specified id is located.
  @raise EThreadNotFound if the thread is not found }
function TThreadManager.getThreadIndex(id: Integer): Integer;
var found: Boolean; i: Integer; msg: String;
begin
    found := false; i:=0;
    while ((not found) and (i<fThreads.Count)) do begin
        if (fThreads[i]<>nil) then begin
            if TCustomThread(fThreads[i]).ThreadInfo.Id = id then begin
                found := true;
            end;
            Inc(i);
        end;
    end;
    if found then result:=i-1
    else begin
        msg := 'Thread (ID='+IntToStr(id)+') was not found. OK on request shutdown.';
        devLog.Debug(msg);
        vcLog.flush;
        raise EThreadNotFound.Create(msg);
    end;
end;

{ Just logs a warning about an unexistent thread }
procedure TThreadManager.logThreadNotFound;
begin
    devLog.Debug('Can''t find a thread that is supposed to exist. This is OK if the thread was a login thread.');
end;

function TThreadManager.getNextId: Integer;
begin
    Inc(fIdCounter);
    Result := fIdCounter;
end;

{ Returns true if there are any threads which are not yet DONE (although they can exist) }
function TThreadManager.areAnyThreadsRunning: Boolean;
begin
    result:=
        (
                (fActiveCheckerThreadCounter>0)
            or  (fActiveIMAPWorkerCounter>0)
            or  (fActiveConverterThreadCounter>0)
        );
end;


{ Convenient method for dumping the contents of the queue }
procedure TThreadManager.dumpContents(title: String);
var tInfo: TThreadInfo; terStr, doneStr: String; i: Integer;
begin
    devLog.Trace('ThreadManager Contents - '+title);
    for i:=0 to fThreads.Count-1 do begin
        tInfo := TCustomThread(fThreads[i]).ThreadInfo;
        if tInfo.WasTerminated then terStr:='yes' else terStr:='no';
        if tInfo.Done then doneStr:='yes' else doneStr:='no';
        devLog.Trace(Format(' index=%2d id=%3d done=%3s terminated=%3s',[i,TCustomThread(fThreads[i]).ThreadInfo.Id,doneStr,terStr]));
    end;
    devLog.Trace('ThreadManager Dump done. Threads num='+IntToStr(fThreads.Count));
end;

procedure TThreadManager.dumpActiveContents;
begin
    devLog.Trace('====== Thread Activity: ');
    devLog.Trace('Checker: '+IntToStr(fActiveCheckerThreadCounter));
    devLog.Trace('IMAP:    '+IntToStr(fActiveIMAPWorkerCounter));
    devLog.Trace('Convert: '+IntToStr(fActiveConverterThreadCounter));
    devLog.Trace('=======================');
end;

end.