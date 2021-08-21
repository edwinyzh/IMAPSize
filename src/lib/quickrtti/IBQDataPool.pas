unit IBQDataPool;

interface
uses sysutils,classes,guids, Db, IBCustomDataSet,
  IBQuery, IBDatabase, IBSQL ;

TIBRTTIEnabler = class(TRTTIEnabler)
 private
   fIBSQL:TIBSQL;
   fufield:String; fuseguids:boolean; foncreateobj:TOnDBRTTINeedObjectID;
 protected
   property  Cache;
 public
   property Value;
   procedure CreateObject; override;
   procedure LoadObject; override;
   procedure UpdateObject; override;
   procedure DeleteObject; override;
 published
   property RTTIObject;
   property Database:TIBDatabase read fdb write fdb;

   property createSQL:String read fcsql write fcsql;
   property deleteSQL:String read fdsql write fdsql;
   property updateSQL:String read fusql write fusql;
   property LoadSQL:String read flsql write flsql;

   property UniqueIDField:string read fufield write fufield; {which field is the UID?}
   property UseGUIDs:Boolean read fuseguids write fuseguids; {should I create GUIDs when creating objects?}
   property OnDBRTTINeedObjectID:TOnDBRTTINeedObjectID read foncreateobj write foncreateobj;
 end;

implementation

procedure TIBRTTIEnabler.CreateObject;
begin
{}
end;

procedure TIBRTTIEnabler.LoadObject;
begin
{}
end;

procedure TIBRTTIEnabler.UpdateObject;
begin
{}
end;

procedure TIBRTTIEnabler.DeleteObject;
begin
{}
end;


end.
