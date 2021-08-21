unit LoggingClient;

interface

uses Classes, LogPersist, Logging, SyncObjs, Forms, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdHTTP;


type

TVConnectionStatus = (csUnknown, csConnected);

TVSqlLogPersistenc = class(TLogPersistence)
end;


TVCommManager = class(TComponent)
end;


TVSynchThread = class(TThread)
private
    fPause : boolean;
    fActive : boolean;
    fHttpClient : TIdHTTP;
    fNeedsJobList : boolean;
    fRequestNow : boolean;
    fDelaySeconds : Integer;
    fConnectionStatus : TVConnectionStatus;
    fSynchRequestMutex : TCriticalSection;
    fDownloadedUsers : boolean;
    fCategoryUpdateSeconds : Integer;
    fTimeToCategoryUpdate : Integer;
    fSynchRequestList: TList;
    fIsWaitingForResponse: boolean;
    fIsSavingData: boolean;
    fIsBusy: boolean;

    function GetIsWaitingForResponse(): boolean;
    function GetIsSavingData(): boolean;
    function GetConnectionStatus(): TVConnectionStatus;
    function GetActive(): boolean;
    function GetIsBusy(): boolean;
    procedure SynchNow();
    procedure Cleanup(sender: TObject);
    function Encrypt(before: String): String;
    function EncodeHttp(data: String): String;
    function DecodeHttp(data: String): String;
//    function GetTimesheetUploadData(synchTimesheetList: TTimesheetSyncRecordList): String;

public
    constructor Create(iSuspend: boolean); overload;
    function GetMutex(): TCriticalSection;
    function GetHasDownloadedUsers(): boolean;
    procedure SetNeedsJobList();
end;


TVCommLogClient = class(TComponent)
end;


TVHttpLogClient = class(TComponent)
end;



implementation

uses SysUtils;


constructor TVSynchThread.Create(iSuspend: boolean);
begin
    inherited Create(iSuspend);
    fPause := false;
    fActive := false;
    fHttpClient := TIdHTTP.Create(Application);
    fNeedsJobList := false;
    fRequestNow := false;
    fDelaySeconds := 5;
    fConnectionStatus := csUnknown;

    fSynchRequestList := TList.Create();
    OnTerminate := Cleanup;
    fSynchRequestMutex := TCriticalSection.Create();
    fDownloadedUsers := false;
    fCategoryUpdateSeconds := 120;
    fTimeToCategoryUpdate := 0;
    fIsWaitingForResponse := false;
    fIsSavingData := false;
    fIsBusy := false;

end;


function TVSynchThread.GetMutex(): TCriticalSection;
begin
    result := fSynchRequestMutex;
end;


function TVSynchThread.GetHasDownloadedUsers(): boolean;
begin
    result := fDownloadedUsers;
end;


procedure TVSynchThread.SetNeedsJobList();
begin
    fRequestNow := true;
    fNeedsJobList := true;
end;


function TVSynchThread.GetIsWaitingForResponse(): boolean;
begin
    result := fIsWaitingForResponse;
end;

function TVSynchThread.GetIsSavingData(): boolean;
begin
    result := fIsSavingData;
end;


function TVSynchThread.GetConnectionStatus(): TVConnectionStatus;
begin
    result := fConnectionStatus;
end;


function TVSynchThread.GetActive(): boolean;
begin
    result := fActive;
end;


function TVSynchThread.GetIsBusy(): boolean;
begin
    result := fIsBusy;
end;


procedure TVSynchThread.SynchNow();
begin
    fRequestNow := true;
end;


procedure TVSynchThread.Cleanup(sender: TObject);
begin
    fHttpClient.Destroy();
    fSynchRequestMutex.Destroy();
end;


function TVSynchThread.Encrypt(before: String): String;
var
    after: String;
begin
    after := before;

    result := after;
end;


function TVSynchThread.EncodeHttp(data: String): String;
var
    newData: String;
    flags: TReplaceFlags;
begin
    flags := [rfReplaceAll];
    newData := data;
    newData := StringReplace(newData, '&', '$ampend;',  flags);
    newData := StringReplace(newData, '?', '$questend;', flags);
    newData := URLEncode(newData);
    result := newData;
end;


function TVSynchThread.DecodeHttp(data: String): String;
var
    newData: String;
    flags: TReplaceFlags;
begin
    newData := data;
    flags := [rfReplaceAll];
    newData := StringReplace(newData, '$ampend;', '&',  flags);
    newData := StringReplace(newData, '$questend;', '?',  flags);
    result := newData;
end;


function TVSynchThread.GetTimesheetUploadData(TTimesheetSyncRecordList *synchTimesheetList): String;
begin
    log.debug('GetTimesheetUploadData() -called');
    AnsiString dataOut = '';

    TTimesheetSyncRecordList.iterator iter;
    for (iter = synchTimesheetList->begin();
            iter != synchTimesheetList->end();
            iter++) begin
        TTimesheetSyncRecord *rec = *iter;
        dataOut += rec->GetData();
    end;

    if (dataOut.Length() > 0) begin
        dataOut = '&put-timesheets=' + EncodeHttp(Encrypt(dataOut));
    end;
    result := dataOut;
end;


function TVSynchThread.GetCategoryUploadData(): String;
begin
    log.debug('GetCategorUploadData() -called');
    AnsiString dataOut = '';

    if (fTimeToCategoryUpdate <= 0) begin
        fTimeToCategoryUpdate = fCategoryUpdateSeconds;
        std.set<Category *> *catList = Category.getCurrentCategories();


        std.set<Category *>.iterator iter;
        //TCategoryList *catList =  TWtObject.GetApplication()->GetLogManager()->GetCategoryList();


        for (iter = catList->begin(); iter != catList->end(); iter++) begin
            Category *category = *iter;
            if (category->getPriority() != Priority.NOTSET) begin
                dataOut += (AnsiString)'LOG-CATEGORY||' + category->getName().c_str()
                        + '||' + IntToStr(category->getPriority()).c_str() + (AnsiString)'\r\n';
            end;
        end;

        if (dataOut.Length() > 0) begin
            dataOut = '&put-log-cat=' + EncodeHttp(Encrypt(dataOut));
        end;
    end;

    return(dataOut);
end;


function TVSynchThread.GetLogUploadData(): String;
begin
    log.debug('GetLogUploadData() -called');
    AnsiString dataOut = '';

    long firstId = 0;
    if (TWtObject.GetApplication()->GetLogManager()->GetCommManager()->GetLastIdSent() > 0) begin
        firstId = TWtObject.GetApplication()->GetLogManager()->GetCommManager()->GetLastIdSent() + 1;
    end;
    dataOut = TWtObject.GetApplication()->GetLogManager()->GetCommManager()->GetLogData(firstId, -1);

    if (dataOut.Length() > 0) begin
        dataOut = '&put-log=' + EncodeHttp(Encrypt(dataOut));
    end;
    return(dataOut);
end;


function TVSynchThread.DetectServer(): String;
begin
    TMemoryStream *requestStream = new TMemoryStream();
    TMemoryStream *responseStream = new TMemoryStream();
    boolean success = SendHttpToServer(requestStream, responseStream);

    delete requestStream;
    delete responseStream;
    return(success);
end;


function TVSynchThread.SendHttpToServer(TStream *requestStream, TStream *responseStream): boolean;
begin
    boolean success = false;
    String cgi = 'http://' + TDataManager.GetInstance()->GetSynchServer() + '/timesheet/main/synch';
    try begin
        fHttpClient->Request->ContentType = 'application/x-www-form-urlencoded';
        fHttpClient->Request->ContentLength = requestStream->Size;

        fHttpClient->Post(cgi, requestStream, responseStream);
        fConnectionStatus = csConnected;
        success = true;
    end;
    catch (Exception &ex) begin
        log.debug(((AnsiString)'HTTP request failed.  Exception = ' + ex.Message).c_str());
        fConnectionStatus = csDisconnected;
    end;
    return(success);
end;


function TVSynchThread.SendRequest(params: String): boolean;
begin
    boolean success = false;
    boolean getUserList = false;
    boolean getEmployeeList = false;
    boolean messageFormShowing = false;
    AnsiString jobDataList = '';
    AnsiString jobStatusData = '';

    if (!Terminated) begin
        log.debug('TSynchManager.SendRequest() called.');

        AnsiString response = '';

        try begin
            fHttpClient->Request->ContentType = 'application/x-www-form-urlencoded';

            TMemoryStream *requestStream = new TMemoryStream();


            params = URLEncode(params);


            TTimesheetSyncRecordList *synchTimesheetList = new TTimesheetSyncRecordList();
            synchTimesheetList->SetOwnsMembers(true);

            TDataManager.GetInstance()->GetTimesheetsToUpload(synchTimesheetList);



            fSynchRequestMutex->Acquire();
            try begin
                // Get synch request items.
                TSynchRequestList.iterator iter;
                iter = fSynchRequestList.begin();
                while (iter != fSynchRequestList.end()) begin
                    TSynchRequest *request = *iter;
                    if (request->GetCommand() == 'get-device-users') begin
                        getUserList = true;
                    end;
                    else if (request->GetCommand() == 'get-job') begin
                        if (jobDataList.Length() > 0) begin
                            jobDataList += ',';
                        end;
                        jobDataList += request->GetProperties()->GetProperty('job');
                    end;
                    else if (request->GetCommand() == 'set-job-status') begin
                        AnsiString jobId = request->GetProperties()->GetProperty('job');
                        TJob *job = TDataManager.GetInstance()->GetJobList()->FindJob(jobId);
                        jobStatusData += 'JOB-STATUS||'+ job->GetId() + '||'
                            + TDataManager.BoolToString(job->GetTimeSheetsComplete()) + '||'
                            + TDataManager.BoolToString(job->GetTimeSheetsLocked()) +'\r\n';
                    end;
                    else if (request->GetCommand() == 'get-employee-list') begin
                        getEmployeeList = true;
                    end;
                    iter++;
                end;


            end;
            catch (Exception &ex) begin
            end;
            fSynchRequestMutex->Release();

            if (getUserList) begin
                params += '&get-device-users=true';
            end;
            if (getEmployeeList) begin
                params += '&get-employee-list=true';
                if (TGuiManager.GetInstance() != NULL)
                    TGuiManager.GetInstance()->ShowMessage('Downloading Employee List', 'Contacting Server...');
                messageFormShowing = true;
            end;
            if (jobDataList.Length() > 0) begin
                params += EncodeHttp('&get-job=' + jobDataList);
            end;

            params += GetLogUploadData();
            params += GetCategoryUploadData();

            params += GetTimesheetUploadData(synchTimesheetList);
            if (jobStatusData.Length() > 0) begin
                params += '&put-job-status=' + EncodeHttp(Encrypt(jobStatusData));
            end;


            requestStream->Write(params.c_str(), params.Length());

            TStringStream *responseStream = new TStringStream('');
            fHttpClient->Request->ContentLength = requestStream->Size;

            log.info('TVSynchThread - posting http request. Waiting for response...');
            fIsWaitingForResponse = true;

            success = SendHttpToServer(requestStream, responseStream);

            fIsWaitingForResponse = false;
            log.info('TVSynchThread - Received http response');

            AnsiString response = responseStream->DataString; //ReadString(responseStream->Size);
            //ResponseMemo->Text = response;

            delete requestStream;
            delete responseStream;

            // If job list received, clear job list request.
            if (response.Pos('CONFIRM_JOB_LIST_SENT') > 0) begin
                fNeedsJobList = false;
            end;

            if (messageFormShowing) begin
                if (TGuiManager.GetInstance() != NULL)
                    TGuiManager.GetInstance()->ShowMessage('Downloading Employee List', 'Received data from server, now saving...');
            end;

            fIsSavingData = true;
            TCommManager.GetInstance()->ParseServerResponse(response);
            fIsSavingData = false;

            fSynchRequestMutex->Acquire();
            fSynchRequestList.Clear();
            fSynchRequestMutex->Release();

            if ((getUserList) && (success)) begin
                fDownloadedUsers = true;
            end;

            TWtObject.GetApplication()->GetLogManager()->GetCommManager()->SetLastSendSucceeded(success);
            if (success == true) begin
                TDataManager.GetInstance()->RemoveUploadedTimesheets(synchTimesheetList);

            end;
            delete synchTimesheetList;


        end;
        catch (Exception &ex) begin
            log.error(((AnsiString)'Error in post request. Exception = ' + ex.Message).c_str());
        end;

        if (TGuiManager.GetInstance() != NULL) begin
            TGuiManager.GetInstance()->HideMessage();
        end;
    end;
    else begin
        log.info('TSynchManager.SendRequest() - termination in progress - aborting synch attempt.');
    end;

    return(success);
end;

procedure TVSynchThread.AddSynchRequest(request: TSynchRequest);
begin
    fSynchRequestMutex->Acquire();
    fSynchRequestList.Add(request);
    fSynchRequestMutex->Release();
end;

procedure TVSynchThread.SetPause(boolean value);
begin
    fPause = value;
end;

boolean TVSynchThread.GetPause() begin
    return(fPause);
end;

procedure __fastcall TVSynchThread.Execute()
begin
    log.debug('Execute() - called');

    fActive = true;
	while (!Terminated)  begin
        fIsBusy = false;
        long sleepTime = 500;
        long millisecondsExpired = 0;

        while ((!Terminated) && (fRequestNow == false) && (millisecondsExpired < fDelaySeconds * 1000) && (!TWtObject.GetApplication()->GetIsClosing()) ) begin
            Idglobal.Sleep(sleepTime);
            millisecondsExpired +=  sleepTime;
            fTimeToCategoryUpdate -= 1;
        end;


        if ((!Terminated) && (fPause == false) && (!TWtObject.GetApplication()->GetIsClosing())) begin

            fRequestNow = false;

            fIsBusy = true;

            AnsiString params = 'get-device-status=true';

            // Every action requires a device param.
            params +=  '&device=' + TDataManager.GetInstance()->GetDeviceId();

            if (fNeedsJobList == true) begin
                params += '&get-job-list=true';
            end;

            if (TDataManager.GetInstance()->GetAuthenticatedUser()) begin
                AnsiString user = TDataManager.GetInstance()->GetAuthenticatedUser()->GetUserName();
                params += '&user=' + user;
            end;


            log.info('TSynchManager.Execute() - checking for connection to server.');
            boolean success = DetectServer();

            if (success) begin
                fDelaySeconds = 10;
                log.info('TSynchManager.Execute() - performing synch after sleep.');
                success = SendRequest(params);
            end;
            else begin
                fDelaySeconds = 30;
                log.info((string)'TSynchManager.Execute() - no connection to server, will try again later. Number of seconds til next check: ' + IntToStr(fDelaySeconds).c_str());
            end;

            fIsBusy = false;
        end;
    end;;
    log.info('TSynchManager.Execute() - finished.');
    fActive = false;

end;;


procedure TVSynchThread.SetHttpClient(TIdHTTP *httpClient) begin
    fHttpClient = httpClient;
end;






TCommManager.TCommManager() begin
    fHistoryListMutex = new TCriticalSection();
    historyList = new TStringList();
    fSynchThread = new TVSynchThread(true);
    fLastJobListUpdate = TDate.INFINITY;
end;

TConnectionStatus TCommManager.GetConnectionStatus() begin
    return(fSynchThread.GetConnectionStatus());
end;

procedure TCommManager.SetHttpConnection(TIdHTTP *iHttpConnection) begin
    httpConnection = iHttpConnection;
end;

procedure TCommManager.DownloadJobList() begin
    fSynchThread.SetNeedsJobList();
end;


TDateTime TCommManager.GetLastJobListUpdate() begin
    return(fLastJobListUpdate);
end;

procedure TCommManager.Pause() begin
    fSynchThread.SetPause(true);
end;

procedure TCommManager.Resume() begin
    fSynchThread.SetPause(false);
end;

procedure TCommManager.Start() begin
    if (fSynchThread.GetActive() == false) begin
        fSynchThread.Resume();
    end;
end;

procedure TCommManager.Stop() begin
    log.info('TCommManager.Stop() called.  Stopping synch communication thread.');

    try begin
        if (fSynchThread.GetActive()) begin
            fSynchThread.Terminate();
            log.info('TCommManager.Stop() called.  Stopping synch communication thread.');
            fSynchThread.WaitFor();
        end;
    end;
    catch (Exception &ex) begin
        log.error(((AnsiString)'TCommManager.Stop() - error stopping SynchThread: ' + ex.Message).c_str());
    end;

    log.info('TCommManager.Stop() finished.');
end;


TCommManager.~TCommManager() begin
    Stop();

    // Wait til thread is finished before deleting.
    log.info('TCommManager.Destructor() - waiting for SynchThread to finish current operation.');

    try begin
        log.info('TCommManager.Destructor() - deleting SynchThread');
        delete fSynchThread;
    end;
    catch (Exception &ex) begin
        log.info(((AnsiString)'TCommManager.Destructor() - error deleting SynchThread: ' + ex.Message).c_str());
    end;

    log.info('TCommManager.Destructor() - deleting History List');
    delete historyList;
end;


TCommManager *TCommManager.GetInstance() begin
    if (Instance == NULL) begin
        Instance = new TCommManager();
    end;
    return(Instance);
end;


boolean TCommManager.IsRequestInProgress() begin
    return(fSynchThread.GetIsBusy());
end;

boolean TCommManager.IsSavingData() begin
    return(fSynchThread.GetIsSavingData());
end;

boolean TCommManager.IsWaitingForResponse() begin
    return(fSynchThread.GetIsWaitingForResponse());
end;

procedure TCommManager.DownloadUserList() begin
    TSynchRequest *synchRequest = new TSynchRequest();
    synchRequest.SetCommand('get-device-users');
    fSynchThread.AddSynchRequest(synchRequest);
    fSynchThread.SynchNow();

end;


procedure TCommManager.DownloadEmployeeList() begin
    TGuiManager.GetInstance().ShowMessage('Downloading Master Employee List', 'Contacting server...');
    TSynchRequest *synchRequest = new TSynchRequest();
    synchRequest.SetCommand('get-employee-list');
    fSynchThread.AddSynchRequest(synchRequest);
    fSynchThread.SynchNow();

end;


procedure TCommManager.CheckStatus() begin
    fSynchThread.SynchNow();

end;


procedure TCommManager.SendJobStatus(TJob *job) begin
    TSynchRequest *synchRequest = new TSynchRequest();
    synchRequest.SetCommand('set-job-status');
    synchRequest.GetProperties().SetProperty('job', job.GetId());
    fSynchThread.AddSynchRequest(synchRequest);
end;


AnsiString TCommManager.GetJobWatchListData() begin
    AnsiString data = '';
    TJobList *jobList = TDataManager.GetInstance().GetJobList();
    TJobList.iterator iter;
    for (iter = jobList.begin(); iter != jobList.end(); iter++) begin
        TJob *job = *iter;
        data += 'WATCHED-JOB||' + job.GetId();
    end;
    return(data);
end;


procedure TCommManager.DownloadJobData(TJob *iJob) begin
    //DownloadAllEmployees();
    log.info('TCommManager.DownloadJobData() - starting');

    TSynchRequest *synchRequest = new TSynchRequest();
    synchRequest.SetCommand('get-job');
    synchRequest.GetProperties().SetProperty('job', iJob.GetId());
    fSynchThread.AddSynchRequest(synchRequest);
    fSynchThread.SynchNow();

    iJob.SetDateRetrievedData(Now());

    log.info('TCommManager.DownloadJobData() - finished');

end;




AnsiString TCommManager.GetLastDataIn() begin
    return(dataIn);
end;



procedure TCommManager.ParseUserLine(AnsiString line) begin
    AnsiString userName = TDataManager.GetField(line, 1);
    TUser *user = TDataManager.GetInstance().GetUserList().FindUser(userName);
    if (user == NULL) begin
        user = new TUser();
        TDataManager.GetInstance().GetUserList().Add(user);
    end;
    user.ParseRecord(line);

end;


procedure TCommManager.ParseEmployeeLine(AnsiString line) begin
    log.debug('Parsing employee line');

    TEmployee *emp = NULL;

    AnsiString empId = TDataManager.GetField(line, 1);

    emp = TDataManager.GetInstance().FindEmployee(empId);

    if (emp == NULL) begin
        emp = new TEmployee();
        emp.ParseRecord(line);
        emp.Save();
    end;
    else begin
        emp.ParseRecord(line);
        emp.Update();
    end;
end;


procedure TCommManager.ParseJobEmployeeLine(AnsiString line) begin

    AnsiString jobId = TDataManager.GetField(line, 1);
    AnsiString userName = TDataManager.GetField(line, 2);

    TJob *job = TDataManager.GetInstance().GetJobList().FindJob(jobId);
    TEmployee *emp = TDataManager.GetInstance().FindEmployee(userName);
    TJobEmployee *jobEmp = new TJobEmployee(job, emp);

    if (job == NULL) begin
        AnsiString msg = 'Error parsing employee for job.  Job not found.  Job = '
              + job.GetId();
    end;
    else begin
        if (emp == NULL) begin
            historyList.Add('Error during retrieval of Job Employees - could not find employee: ' + userName + '  Job = ' + job.GetDescription());

            AnsiString msg = 'Error parsing employee for job.  Employee not found.  User name = '
                + userName +
                '   Job = ' + job.GetId();
            log.error(msg.c_str());
        end;
        else begin

            job.GetJobEmployeeList().Add(jobEmp);

            AnsiString msg = 'Adding employee to job.  User name = '
                + userName +
                '   Job = ' + job.GetId();
            log.info(msg.c_str());
            fLastJobListUpdate = Now();
        end;

        jobEmp.ParseRecord(line);

        job.SetDateRetrievedData(Now());
    end;
end;


procedure TCommManager.ParseTimesheetLine(AnsiString line) begin
    TTimesheet *newEntry = TTimesheet.ParseRecord(line);
    TDataManager.GetInstance().SaveTimesheet(newEntry);

end;


boolean TCommManager.GetHasDownloadedUsers() begin
    return(fSynchThread.GetHasDownloadedUsers());
end;


procedure TCommManager.ParseJobLine(AnsiString line) begin
    AnsiString jobId = TDataManager.GetField(line, 1);
    boolean newJob = false;

    // FInd existing job.
    TJob *job = TDataManager.GetInstance().GetJobList().FindJob(jobId);

    if (job == NULL) begin
        // If job not on this device, then create it.
        job = new TJob();
        newJob = true;
    end;

    job.SetId(jobId);
    job.ParseRecord(line);


    if (newJob) begin
        TDataManager.GetInstance().GetJobList().Add(job);
    end;
    fLastJobListUpdate = Now();
end;


procedure TCommManager.ParseStatusLine(AnsiString line) begin
    AnsiString activeStr = TDataManager.GetField(line, 1);


    if (activeStr.LowerCase().Trim() == 'inactive') begin
        TDataManager.GetInstance().SetDeviceActive(false);
        TDataManager.GetInstance().SetDeviceKnown(true);

        // Log the user off when device is disabled.
        TDataManager.GetInstance().SetAuthenticatedUser(NULL);

    end;
    else if (activeStr.LowerCase().Trim() == 'no-device') begin
        TDataManager.GetInstance().SetDeviceKnown(false);
        TDataManager.GetInstance().SetAuthenticatedUser(NULL);
    end;
    else begin
        TDataManager.GetInstance().SetDeviceActive(true);
        TDataManager.GetInstance().SetDeviceKnown(true);
    end;
end;


procedure TCommManager.ParseServerResponse(AnsiString response) begin
    TDataManager.GetInstance().GetMutex().Acquire();

    boolean recordsAdded = false;
    int curPos = 1;
    int nextPos = response.Pos('\r\n');
    int employeesAdded = 0;
    int timesheetsAdded = 0;
    int jobsAdded = 0;
    if (nextPos == 0)
        nextPos = response.Length();
    while ((curPos < response.Length()) && (nextPos > 0)) begin
        AnsiString curLine = response.SubString(curPos, nextPos - curPos);
        AnsiString recordType = TDataManager.GetField(curLine, 0);
        if (curLine.Pos('DEVICE-STATUS') == 1) begin
            ParseStatusLine(curLine);
        end;
        else if (curLine.Pos('EMPLOYEE') == 1) begin
            recordsAdded = true;
            employeesAdded++;
            ParseEmployeeLine(curLine);
        end;
        else if (curLine.Pos('TIMESHEET') == 1) begin
            timesheetsAdded++;
            recordsAdded = true;
            ParseTimesheetLine(curLine);
        end;
        else if (curLine.Pos('JOB-EMP') == 1) begin
            recordsAdded = true;
            ParseJobEmployeeLine(curLine);
        end;
        else if (curLine.Pos('JOB') == 1) begin
            recordsAdded = true;
            jobsAdded++;
            ParseJobLine(curLine);
        end;
        else if (curLine.Pos('USER') == 1) begin
            recordsAdded = true;
            ParseUserLine(curLine);
        end;
        else if (curLine.Pos('JOB-LIST') == 1) begin
            recordsAdded = true;
            ParseJobLine(curLine);
        end;
        else if (curLine.Pos('LOG_CONFIRM') == 1) begin
            recordsAdded = true;
            ParseJobLine(curLine);
        end;
        else begin
            AddHistoryEntry('Error - unknown record type received: ' + curLine);
            log.info(((AnsiString)'TCommManager.ParseServerResponse() - Unknown record type received: ' + curLine).c_str());
        end;

        curPos = nextPos + 2;
        int num =  1 + response.Length() - curPos;
        AnsiString remLine = response.SubString(curPos, num);
        nextPos = curPos + remLine.Pos('\r\n') - 1;
        if (nextPos == 0)
            nextPos = response.Length();
    end;
    TDataManager.GetInstance().GetMutex().Release();

    if (recordsAdded) then begin
        TDataManager.GetInstance().SaveUserList();

        //if (employeesAdded > 0) begin
        //    TDataManager.GetInstance().SaveEmployeeList();
        //end;

        if (jobsAdded > 0) then begin
            TDataManager.GetInstance().SaveJobList();
        end;

        TDataManager.GetInstance().SaveJobEmployeeList();

    end;
end;


procedure TCommManager.AddHistoryEntry(AnsiString text);
begin
    fHistoryListMutex.Acquire();

        historyList.Add(text);

    fHistoryListMutex.Release();
end;


TCriticalSection *TCommManager.GetMutex();
begin
    return(fSynchThread.GetMutex());
end;


TStringList *TCommManager.GetHistoryList() begin
    fHistoryListMutex.Acquire();
        TStringList *copy = new TStringList();
        copy.AddStrings(historyList);
    fHistoryListMutex.Release();
    return(copy);
end;







end.
