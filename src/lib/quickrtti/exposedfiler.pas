unit exposedfiler;

interface
uses classes,typinfo,sysutils,controls;

type
{class used by TExposedFiler to store property definitions}

TExposed = class(TObject)
 private
  fname:String;
 published
     property Name:String read fname write fname;
 end;

TExposedDefinition = class(TExposed)
  private
     fread:TReaderProc;
     fwrite: TWriterProc;
     fhas:boolean;
  published
     property ReadData:TReaderProc read fread write fread;
     property WriteData:TWriterProc read fwrite write fwrite;
     property HasData:boolean read fhas write fhas;
  end;

TExposedBinaryDefinition = class(TExposed)
  private
     fread,
     fwrite: TStreamProc;
     fhas:boolean;
  published
     property ReadStream:TStreamProc read fread write fread;
     property WriteStream:TStreamProc read fwrite write fwrite;
     property HasData:boolean read fhas write fhas;
  end;


{
a TFiler that can expose properties, AND values of a component. used by RTTI enabler to
decide which data to store .. already storing the published... so store everything else.
}

TExposedReader= class(TReader)
  private
   flist:TList;
   finternalstream:tmemorystream;
  public
   constructor Create;{On Purpose! tfiler normally wants a stream}
   destructor Destroy;override;
   procedure DefineProperty(const Name: string;
      ReadData: TReaderProc; WriteData: TWriterProc;
      HasData: Boolean); override;
    procedure DefineBinaryProperty(const Name: string;
      ReadData, WriteData: TStreamProc;
      HasData: Boolean); override;
published
   function ItemCount:integer;
   function Items(index:integer):TExposed;
   function ItemName(index:integer):String;
   procedure SaveItemToStream (Name:String;S:TStream);
   procedure LoadItemFromStream (Name:String;S:TStream);
end;

TExposedWriter= class(TWriter)
  private
   flist:TList;
   finternalstream:tmemorystream;
  public
   constructor Create;{On Purpose! tfiler normally wants a stream}
   destructor Destroy;override;
   procedure DefineProperty(const Name: string;
      ReadData: TReaderProc; WriteData: TWriterProc;
      HasData: Boolean); override;
    procedure DefineBinaryProperty(const Name: string;
      ReadData, WriteData: TStreamProc;
      HasData: Boolean); override;
published
   function ItemCount:integer;
   function Items(index:integer):TExposed;
   function ItemName(index:integer):String;
   procedure SaveItemToStream (Name:String;S:TStream);
   procedure LoadItemFromStream (Name:String;S:TStream);
end;

implementation

{TExposedProperties.}


constructor TExposedReader.Create;
begin
  finternalstream:=tmemorystream.create;
  inherited create(finternalStream,4096);
  flist:=Tlist.create;
end;

destructor TExposedReader.Destroy;
begin
 try
  while flist.count>0 do
   begin
     TObject(flist[0]).free;
     flist.delete(0);
   end;
 finally
     inherited destroy;
 end;
end;

function TExposedReader.ItemCount:integer;
begin
 result:=flist.count;
end;

function TExposedReader.Items(index:integer):TExposed;
begin
  result:=TExposed(flist[index]);
end;

function TExposedReader.ItemName(index:integer):String;
begin

 result:=Items(index).name;
end;

procedure TExposedReader.LoadItemFromStream (Name:String;S:TStream);
var i,max:integer;      ED:TExposedDefinition;  EBD:TExposedBinaryDefinition;
    R:TReader;
begin
 max:=flist.count-1;
 R:=TReader.create(s,4096);
 for i:= 0 to max do
  begin
    if TExposed(flist[i]) is TExposedDefinition then
     begin
      ED:=TExposedDefinition(flist[i]);
      if UPPERCASE(ED.Name)=UPPERCASE(Name) then
       begin
        ED.ReadData(r);
       end;
     end;
    if TExposed(flist[i]) is TExposedBinaryDefinition then
     begin
      EBD:=TExposedBinaryDefinition(flist[i]);
      if UPPERCASE(EBD.Name)=UPPERCASE(Name) then
       begin
        EBD.ReadStream(s);
       end;
     end;
  end;
 R.free;
end;

procedure TExposedReader.SaveItemToStream (Name:String;S:TStream);
var i,max:integer;      ED:TExposedDefinition;  EBD:TExposedBinaryDefinition;
    W:TWriter;
begin
 max:=flist.count-1;
w:=TWriter.create(s,4096);
 for i:= 0 to max do
  begin
    if TExposed(flist[i]) is TExposedDefinition then
     begin
      ED:=TExposedDefinition(flist[i]);
      if UPPERCASE(ED.Name)=UPPERCASE(Name) then
       begin
        ED.WriteData(w);
       end;
     end;
    if TExposed(flist[i]) is TExposedBinaryDefinition then
     begin
      EBD:=TExposedBinaryDefinition(flist[i]);
      if UPPERCASE(EBD.Name)=UPPERCASE(Name) then
       begin
        EBD.WriteStream(s);
       end;
     end;
  end;
w.free;
end;


procedure TExposedReader.DefineProperty(const Name: string; ReadData: TReaderProc; WriteData: TWriterProc; HasData: Boolean);
var
 ED:TExposedDefinition;
begin
 ED:=TExposedDefinition.create;
 ED.name:=name;
 ED.readdata:=ReadData;
 ED.WriteData:=WriteData;
 ED.HasData:=HasData;
 flist.add(ED);
end;

procedure TExposedReader.DefineBinaryProperty(const Name: string; ReadData, WriteData: TStreamProc; HasData: Boolean);
var
  EBD:TExposedBinaryDefinition;
begin
 EBD:=TExposedBinaryDefinition.create;
 EBD.name:=name;
 EBD.readstream:=ReadData;
 EBD.Writestream:=WriteData;
 EBD.HasData:=HasData;
 flist.add(EBD);
end;


constructor TExposedWriter.Create;
begin
  finternalstream:=tmemorystream.create;
  inherited create(finternalStream,4096);
  flist:=Tlist.create;
end;

destructor TExposedWriter.Destroy;
begin
 try
  while flist.count>0 do
   begin
     TObject(flist[0]).free;
     flist.delete(0);
   end;
 finally
     inherited destroy;
 end;
end;

function TExposedWriter.ItemCount:integer;
begin
 result:=flist.count;
end;

function TExposedWriter.Items(index:integer):TExposed;
begin
  result:=TExposed(flist[index]);
end;

function TExposedWriter.ItemName(index:integer):String;
begin

 result:=Items(index).name;
end;

procedure TExposedWriter.LoadItemFromStream (Name:String;S:TStream);
var i,max:integer;      ED:TExposedDefinition;  EBD:TExposedBinaryDefinition;
    R:TReader;
begin
 max:=flist.count-1;
 R:=TReader.create(s,4096);
 for i:= 0 to max do
  begin
    if TExposed(flist[i]) is TExposedDefinition then
     begin
      ED:=TExposedDefinition(flist[i]);
      if UPPERCASE(ED.Name)=UPPERCASE(Name) then
       begin
        ED.ReadData(r);
       end;
     end;
    if TExposed(flist[i]) is TExposedBinaryDefinition then
     begin
      EBD:=TExposedBinaryDefinition(flist[i]);
      if UPPERCASE(EBD.Name)=UPPERCASE(Name) then
       begin
        EBD.ReadStream(s);
       end;
     end;
  end;
 R.free;
end;

procedure TExposedWriter.SaveItemToStream (Name:String;S:TStream);
var i,max:integer;      ED:TExposedDefinition;  EBD:TExposedBinaryDefinition;
    W:TWriter;
begin
 max:=flist.count-1;
w:=TWriter.create(s,4096);
 for i:= 0 to max do
  begin
    if TExposed(flist[i]) is TExposedDefinition then
     begin
      ED:=TExposedDefinition(flist[i]);
      if UPPERCASE(ED.Name)=UPPERCASE(Name) then
       begin
        if Ed.hasdata then ED.WriteData(w);
       end;
     end;
    if TExposed(flist[i]) is TExposedBinaryDefinition then
     begin
      EBD:=TExposedBinaryDefinition(flist[i]);
      if UPPERCASE(EBD.Name)=UPPERCASE(Name) then
       begin
       if Ed.hasdata then EBD.WriteStream(s);
       end;
     end;
  end;
w.free;
end;


procedure TExposedWriter.DefineProperty(const Name: string; ReadData: TReaderProc; WriteData: TWriterProc; HasData: Boolean);
var
 ED:TExposedDefinition;
begin
 ED:=TExposedDefinition.create;
 ED.name:=name;
 ED.readdata:=ReadData;
 ED.WriteData:=WriteData;
 ED.HasData:=HasData;
 flist.add(ED);
end;

procedure TExposedWriter.DefineBinaryProperty(const Name: string; ReadData, WriteData: TStreamProc; HasData: Boolean);
var
  EBD:TExposedBinaryDefinition;
begin
 EBD:=TExposedBinaryDefinition.create;
 EBD.name:=name;
 EBD.readstream:=ReadData;
 EBD.Writestream:=WriteData;
 EBD.HasData:=HasData;
 flist.add(EBD);
end;


end.
