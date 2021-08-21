unit GetConnectionResponseInfo;

interface

type
    { Record used to communicate information between the request manager
      and the connection pool cregarding the retrieval of a connection }
    TGetConnectionResponseInfo = record
        requestId: Integer;     // Id of the request requesting a new connection
        connId: Integer;        // The connection ID of the assigned connection (-1 if none)
        success: Integer;       // Additional information about the state of the connection request (ord(TLoginResult))
    end;
    PGetConnectionResponseInfo = ^TGetConnectionResponseInfo;

implementation

end.
 