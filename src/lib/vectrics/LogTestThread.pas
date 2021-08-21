unit LogTestThread;

interface

uses
  Classes, SysUtils {$IFDEF MSWINDOWS} , Windows {$ENDIF};

type


TvLogTestThread = class(TThread)
private
    fSleepTime: Integer;
    fLogger: String;
    fName: String;
protected
    procedure Execute; override;
public
    procedure SetSleepTime(millis: Integer);
end;




implementation

uses Logging;

var
    ThreadInstanceCount: Integer;



{ Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure LogTestThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{$IFDEF MSWINDOWS}
type
  TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: PChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;
{$ENDIF}

{ LogTestThread }



procedure TvLogTestThread.SetSleepTime(millis: Integer);
begin
    fSleepTime := millis;
end;


procedure TvLogTestThread.Execute;
begin
    fLogger := 'vectrics.logging.test.thread_' + IntToStr(ThreadInstanceCount);
    TvLogger.GetInstance(fLogger).Level := lvTrace;
    fName := 'Thread_' + IntToStr(ThreadInstanceCount);
    Inc(ThreadInstanceCount);

    while (terminated = false) do begin
        sleep(fSleepTime);
        TvLogger.GetInstance(fLogger).Trace('Trace event from test thread. ' + fName);
        TvLogger.GetInstance(fLogger).Debug('Debug event from test thread. ' + fName);
        TvLogger.GetInstance(fLogger).Info('Info event from test thread. '   + fName);
        TvLogger.GetInstance(fLogger).Error('Error event from test thread. ' + fName);
        TvLogger.GetInstance(fLogger).Fatal('Fatal event from test thread. ' + fName);
    end;
end;



end.
