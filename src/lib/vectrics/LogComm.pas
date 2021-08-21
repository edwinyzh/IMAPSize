unit LogComm;

interface
uses Logging, IdMessageClient, IdSMTP, IdMessage, IdBaseComponent, IdComponent,
    Classes, SysUtils, SYncObjs;





type
TVSmtpAppender = class(TVAppender)
public
    constructor Create(owner: TComponent); override;
    destructor Destroy(); override;
protected
    procedure Append(event: TVLoggingEvent); override;
    procedure IdSMTPStatus(ASender: TObject;
            const AStatus: TIdStatus; const AStatusText: String);
    procedure IdSMTPDisconnected(Sender: TObject);
    procedure IdSMTPConnected(Sender: TObject);

private
    fErrorList: TStringList;
    fMutex: TCriticalSection;
    fIdSmtp: TIdSmtp;
    fMessage: TIdMessage;
    fDateFormat : String;
    fSmtpPassword: String;
    fSmtpUserName: String;
    fSmtpHost: String;
    fFromAddress: String;
    fFromName: String;
    fRecipientName: String;
    fRecipientAddress: String;
    procedure SendEmail();
published
    property DateFormat: String read fDateFormat write fDateFormat;
    property Message: TIdMessage read fMessage;
    property UserName: String read fSmtpUserName write fSmtpUserName;
    property Password: String read fSmtpPassword write fSmtpPassword;
    property Host: String read fSmtpHost write fSmtpHost;
    property RecipientName: String read fRecipientName write fRecipientName;
    property RecipientAddress: String read fRecipientAddress write fRecipientAddress;
    property FromAddress: String read fFromAddress write fFromAddress;
    property FromName: String read fFromName write fFromName;
end;

procedure Register;

implementation

constructor TVSmtpAppender.Create(owner: TComponent);
begin
    inherited Create(owner);
    self.Asynchronous := true;
    fMessage := TIdMessage.Create(self);
    fMutex := TCriticalSection.Create();
    fDateFormat := 'mm-dd-yyyy h:nn:ss';
    fErrorList := TStringList.Create();
end;


destructor TVSmtpAppender.Destroy();
begin
    fMutex.Acquire();
    fMutex.Destroy();
    fMessage.Destroy();
    fErrorList.Destroy();
    inherited;
end;

procedure TVSmtpAppender.Append(event: TVLoggingEvent);
var
    dateStr: String;
    dateFormat: String;
    exceptionMessage: String;
    i: integer;
begin
    fErrorList.Clear();
    if (Length(fRecipientAddress) <= 0) then
        fErrorList.Add('Recipient Address not given.');
    if (Length(fFromAddress) <= 0) then
        fErrorList.Add('Sender Address not given.');
    if (Length(fSmtpHost) <= 0) then
        fErrorList.Add('Host not given.');
    if (Length(fSmtpUserName) <= 0) then
        fErrorList.Add('SMTP User not given.');

    fMessage.ContentType := 'text';
    fMessage.From.Address := FromAddress;
    fMessage.From.Name := FromName;
    fMessage.Recipients.EMailAddresses := fRecipientAddress;
    fMessage.Subject := 'Logging Event from: ' + event.Logger;

    fMessage.ClearBody;
    fMessage.Body.Add('Logger: ');
    fMessage.Body.Add(event.GetLogger());
    fMessage.Body.Add('');
    fMessage.Body.Add('');

    DateTimeToString(dateStr, dateFormat, event.GetTime());

    fMessage.Body.Add('Time:');
    fMessage.Body.Add(dateStr);
    fMessage.Body.Add('');
    fMessage.Body.Add('');

    fMessage.Body.Add('Severity:');
    fMessage.Body.Add(TVLevel.ToString(event.Level));
    fMessage.Body.Add('');
    fMessage.Body.Add('');

    fMessage.Body.Add('Message:');
    fMessage.Body.Add(event.GetMessage());
    fMessage.Body.Add('');
    fMessage.Body.Add('');

    if (Length(event.GetException()) > 0) then begin
        fMessage.Body.Add('Exception:' + #9 + event.GetException());
    end;

    if (fErrorList.Count > 0) then begin
        exceptionMessage := 'Incomplete parameters:';
        for i := 0 to fErrorList.Count - 1 do begin
            exceptionMessage := exceptionMessage + CR_CHAR + EOL_CHAR + String(fErrorList[i]);
        end;
        if (Assigned(OnError)) then begin
            OnError(self, exceptionMessage);
        end
        else begin
            raise Exception.Create(exceptionMessage);
        end;
    end
    else begin
        SendEmail();
    end;

end;


procedure TVSmtpAppender.SendEmail();

begin
    fMutex.Acquire();
    fIdSmtp := TIdSmtp.Create(self);

    fIdSmtp.Host := fSmtpHost; //'mail.webpipe.net';
    fIdSmtp.UserName := fSmtpUserName; //'mmoore@agilux.com';
    fIdSmtp.Password := fSmtpPassword;

    fIdSmtp.ReadTimeout := 1000;

    fIdSmtp.OnConnected := IdSmtpConnected;
    fIdSmtp.OnDisconnected := IdSmtpDisconnected;
    fIdSmtp.OnStatus := IdSMTPStatus;

    try
        fIdSmtp.Connect();
        try
            fIdSmtp.Send(fMessage);
        except
            on ex: Exception do begin
                if (Assigned(OnError)) then begin
                    OnError(self, 'Exception sending e-mail: ' + ex.Message);
                end;
            end;
        end;
        fIdSmtp.Disconnect();
    except
        on ex: Exception do begin
            fIdSmtp.Destroy();
            fIdSmtp := nil;
            if (Assigned(OnError)) then begin
                OnError(self, 'Exception connecting to SMTP server: ' + ex.Message);
            end;
        end;
    end;

    if (fIdSmtp <> nil) then begin
        fIdSmtp.Destroy;
    end;

    fMutex.Release();
end;


procedure TVSmtpAppender.IdSMTPConnected(Sender: TObject);
begin
    if (Length(fSmtpPassword) > 0) then begin
        fIdSmtp.Authenticate;
    end;
end;


procedure TVSmtpAppender.IdSMTPDisconnected(Sender: TObject);
begin
end;


procedure TVSmtpAppender.IdSMTPStatus(ASender: TObject;
  const AStatus: TIdStatus; const AStatusText: String);
begin
end;


procedure Register;
begin
  RegisterComponents('Logging', [TVSmtpAppender]);
end;


end.
