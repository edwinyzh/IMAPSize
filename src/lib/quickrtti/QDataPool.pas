unit QDataPool;

interface
uses classes,sysutils,QuickRTTI;
{
The idea is that you can interact with a database rather easily by means of your objects.
an object TPerson and a table "PERSON" can be mapped to each other.

the DBRTTIEnabler doesn't specify the storage, so you can adapt this to your
favorite flavor of RDB/OODB, whatever...

I'll probably create my ISAPI transport based on this very class.. seeing as how I pretty much
have to implement this basic functionality every time.
}

type

 TOnDBRTTINeedObjectID = Procedure (Sender:TObject;RTTIObject:TPersistent;var ObjectID:String);

 TDBRTTIEnabler = class(TRTTIEnabler)
 private
   fufield:String; fuseguids:boolean; foncreateobj:TOnDBRTTINeedObjectID;
 protected
   property  Cache;
 public
   property Value;
   procedure CreateObject; virtual;abstract;
   procedure LoadObject; virtual;abstract;
   procedure UpdateObject;  virtual;abstract;
   procedure DeleteObject;  virtual;abstract;
 published
   property RTTIObject;
   property UniqueIDField:string read fufield write fufield; {which field is the UID?}
   property UseGUIDs:Boolean read fuseguids write fuseguids; {should I create GUIDs when creating objects?}
   property OnDBRTTINeedObjectID:TOnDBRTTINeedObjectID read foncreateobj write foncreateobj;
 end;



implementation

end.
