unit DFormsRTTI;

interface
uses classes,controls,typinfo,sysutils,Middlex,QuickRTTI,exposedfiler ,dialogs;

type

{Why, you may ask, did I not just include all this in the normal middlex unit, or built into TRTTIEnabler?
well, after I build the exposedfiler unit, I was thinking that there were just too many units here to include for every project,
when you wouldn't even be using them!... I mean, defined properties really only show up (mostly) in
TControls... most everyday projects won't use them. Plus, its still experimental.

If, however, everyone starts running out and using defined properties to store streamable data.. well,
then I guess I'll make it part of QuickRTTI altogether. fair enough?

Bugs: TTreeview doesn't work ... hmm, I'm not sure why, but it created two timer components in its component list,
methinks it was a bit of a hack... I've tried several other controls and it seems ok.

Mike - Aug 2, 2001.
}

TDeFormRTTI = class (TCustomXMLRTTI) {modified middlex (added form handling)}
 private
   fNode:TmiddleXNode;
   {DP_MOD}
    fbinhex:boolean; EX:TExposedWriter;
   {/DP_MOD}
 protected
   function outputXML :String; override;
   function outputSchema :String; override;
   procedure inputXML (sXML:String); override;
   procedure inputSchema (sSchema:String); override;
 public
    property Node:TMiddleXNode read fnode write fnode;
    property Value;
    constructor create;override;
    destructor destroy;override;
 published
  {DP_MOD}
   function DefinedCount:integer;
   function DefinedNames (index:integer):String;
   function DefinedIndex (name:String):integer;
   procedure SaveDefinedToStream(index:integer;s:TStream);
   function  SaveDefinedToString(index:integer):STring;
   procedure LoadDefinedFromString(index:integer;Data:STring);
   procedure LoadDefinedFromStream(index:integer;s:TStream);
   property  BinHexDefined:boolean read fbinhex write fbinhex;
   procedure SetObject (o:TPersistent);override;
  {/DP_MOD}
   property RTTIObject;
   property InTagProperties;
   property ShowType;
   property ObjectID;
   property XML:String read outputXML write inputXML;
   property TagName;
   property TagClassType;
   property FindTypes;
 end;

implementation

constructor TDeFormRTTI.create;
begin
  inherited create;
     Node:=TmiddleXNode.create;
     EX:=TExposedWriter.create;
end;

destructor TDeFormRTTI.destroy;
begin
  try
   {}
    Node.Free;
    EX.free;
  finally
    inherited destroy;
  end;
end;

function TDeFormRTTI.outputSchema :String;
begin
 result:='<Not Supported/>'
end;

procedure TDeFormRTTI.inputSchema (sSchema:String);
begin
 {Ignored}
end;

procedure TDeFormRTTI.SetObject (o:TPersistent);
begin
 try
 if o is TComponent then
   begin
    EX.free;
    EX:=TExposedWriter.create;
    EX.WriteComponent(TComponent(o));
   end;
 except on E:Exception do
 {ignore} showmessage(o.classname);
 end;
  inherited SetObject(o);
end;

function TDeFormRTTI.DefinedCount:integer;
begin
  result:=EX.ItemCount ;
end;


function TDeFormRTTI.DefinedNames (index:integer):String;
begin
 result:=EX.ItemName(index);
end;

function TDeFormRTTI.DefinedIndex (name:String):integer;
var max,i,iout:integer;
begin
 max:=ex.itemcount-1;
 result:=-1;
 iout:=-1;
 i:=0;
 while ((i<=max) AND (iout=-1)) do
  begin
   if Uppercase(EX.Items(i).name)=uppercase(name) then
      iout:=i;
   inc(i);
  end;
  result:=iout;
end;

function  TDeFormRTTI.SaveDefinedToString(index:integer):STring;
var h:tmemorystream; outhold:tstringlist;
begin
if index>-1 then
begin
  h:=tmemorystream.create;
  outhold:=tstringlist.create;
  SaveDefinedToStream (index,h);
  h.seek(0,0);
  outhold.loadfromstream(h);
  h.free;
  result:=trim(outhold.text);
  outhold.free;
end;
end;

procedure TDeFormRTTI.SaveDefinedToStream(index:integer;s:TStream);
var hs:Tmemorystream;c:Char; o:String;si,smax:integer;
begin
if EX.ItemName(index)<>'' then
if not(fbinhex) then
 begin
   EX.SaveItemToStream(EX.ItemName(index),s);
 end else
  begin
    hs:=tmemorystream.create;
    EX.SaveItemToStream(EX.ItemName(index),hs);
   smax:=hs.size;
   hs.seek(0,0);
   for si := 0 to smax do
    begin
    hs.Read(c,1);
    o:= inttohex(ord(c),2);
    s.write(o[1],2);
    end;
   hs.free;
  end;
end;

procedure TDeFormRTTI.LoadDefinedFromString(index:integer;Data:STring);
var h:tmemorystream; hold:String;
begin
if index>-1 then
begin
  h:=tmemorystream.create;
  hold:=trim(Data);
  h.Write(hold[1],length(hold));
  h.seek(0,0);
  LoadDefinedFromStream (index,h);
  h.free;

end;
end;

procedure TDeFormRTTI.LoadDefinedFromStream(index:integer;s:TStream);
var si,smax:integer; c,c1,c2:char; hs:Tmemorystream;
begin
if not(fbinhex) then
 begin
   EX.LoadItemFromStream(EX.ItemName(index),s)
 end
 else
  begin
   hs:=tmemorystream.create;
   smax:=s.size;
   hs.seek(0,0);
   s.seek(0,0);
   si:=1;
   while si<=smax-1 do
    begin
    s.Read(c1,1);
    s.Read(c2,1);    
    c:= char(byte(strtoint('$'+c1+c2)));
    hs.write(c,1);
    si:=si+2;
    end;
    hs.seek(0,0);
   EX.LoadItemFromStream(EX.ItemName(index),hs);
   hs.free;
  end;
end;


function TDeFormRTTI.outputXML :String;
var i,j,k,max:integer; typname:ttypekind;  q,q2:TDeFormRTTI; holdstream:tmemorystream;
  s:TStrings;L:TList; C:TCollection;holdtags,cname,thisprop,outhold,holdtag:String; ftags:TStringlist;
  clname:String; compobj,compchild:TComponent;  cnode:TMIddlexnode;
begin
 outhold:='';
 uniquestring(outhold);
 {Thanks to for catching to error. I added a property "TagClassType" if the item is unassigned}

 if showtype then
   outhold:='<'+tagname+' TYPE="'+TagClassType+'"'
   else
   outhold:='<'+tagname;


 if objectid<>'' then outhold:=outhold+' ID="'+objectid+'"';
   if intagproperties<>'' then
   begin
    ftags:=tstringlist.create;
     holdtags:=intagproperties;
     while pos(',',holdtags)<>0 do
     begin
       ftags.add (trim(copy(holdtags,1,pos(',',holdtags)-1)));
       delete(holdtags,1,pos(',',holdtags));
     end;
     if holdtags<>'' then ftags.add(trim(holdtags));
   end;

 if intagproperties<>'' then
 begin
  for i:= 0 to propertycount-1 do
  if ftags.indexof(propertynames(i))>-1 then
  begin
  thisprop:=propertynames(i);
  typname :=  self.propertyTypes (i);
  if typname<>tkclass then
   outhold:=outhold+' '+thisprop+'="'+GetValue(thisprop)+'" ';
  end;
 end;

 // The above line allows us to have collections of items.. like tlist
 outhold:=outhold+'>'+#13+#10;
 for i:= 0 to propertycount-1 do
 begin
 thisprop:=propertynames(i);
 typname :=  self.propertyTypes (i);
  if typname<>tkclass then
    begin
    if intagproperties='' then
     outhold:=outhold+' <'+thisprop+'>'+GetValue(thisprop)+'</'+thisprop+'>'+#13+#10
     else
     if ftags.indexof(propertynames(i))=-1 then
            outhold:=outhold+' <'+thisprop+'>'+GetValue(thisprop)+'</'+thisprop+'>'+#13+#10
    end
   else
   begin
            
       Cache.rttiobject:=TPersistent(Childobject(thisprop));
       Cache.TagName := thisprop;
       TDeFormRTTI(Cache).binhexdefined:=fbinhex;
       outhold:=outhold + TDeFormRTTI(Cache).XML;
   end;

 end;

   if  RTTIObject is TStrings then
   begin
    s:=TStrings(self.rttiobject);
    for k:= 0 to s.Count-1 do
    outhold:=outhold+'<LINE INDEX="'+inttostr(k)+'">' + s[k] + '</LINE>';
   end;

   if  RTTIObject is TCollection then
   begin
    c:=Tcollection(self.rttiobject);
    max:= c.count-1;
      holdtag:=c.itemclass.classname;
      delete(holdtag,1,1);
    for k:= 0 to max do
     begin
      {create and output any internal items}
      if C.Items[k] is TPersistent then
        begin
          cache.rttiobject:=TPersistent(C.Items[k]) ;
          cache.tagname:= holdtag;
       TDeFormRTTI(Cache).binhexdefined:=fbinhex;
          outhold:=outhold+ TDeFormRTTI(Cache).xml;
        end;
     end;
   end;
   if  RTTIObject is TComponent then
   begin
    compobj:=Tcomponent(self.rttiobject);
    {Actually, for this version, I am only reading a defined property called Data,
    conveniently cooresponding the TImages' image data :) .. I will modify this to present a
    more familiar tree of properties for any defined properties NOT already published.}
  if DefinedCount>0 then
    begin
    outhold:=outhold+#13+#10+'<_DEFINED>';
      max:=DefinedCount-1;
      for k:= 0 to max do
      begin
      if self.indexof(DefinedNames(k)) = -1 then
            outhold:=outhold+#13+#10+'<'+DefinedNames(k)+'>'+ SaveDefinedToString(k)+'</'+DefinedNames(k)+'>';
      end;
    outhold:=outhold+#13+#10+'</_DEFINED>'+#13+#10;
    end;

    max:= compobj.componentcount-1;
    if max>-1 then outhold:=outhold+'<_COMPONENTS>';
    for k:= 0 to max do
     if assigned(Compobj.components[k]) then
     begin
         cache.tagname:= trim(Compobj.components[k].name);
         if cache.tagname='' then cache.tagname:='COMPONENT'+inttostr(k);
         cache.rttiobject:= Compobj.components[k] ;
         TDeFormRTTI(Cache).binhexdefined:=fbinhex;
         outhold:=outhold+ TDeFormRTTI(Cache).xml;
     end;
    if max>-1 then  outhold:=outhold+'</_COMPONENTS>';
    end;
 outhold:=outhold+'</'+tagname+'>'+#13+#10;  
 result:= outhold;
end;


procedure TDeFormRTTI.inputXML (sXML:String);
var max,i,j,k,cmax:integer;ftags,s:TStringlist;
    holdclassname,holdtags,thisprop,holdtag,dname:String; Child,cnode:TMiddlexNode;
    c:TCollection; ci:TCollectionItem; holdchildXML:String;
    idx:integer; compobj,compchild:TComponent; cidx:integer;

begin
 Node.ShortTags := false;
 Node.XML:=sXML;
 if assigned(RTTIObject) then
  begin
   {find any properties that might be "intag"}
   if intagproperties<>'' then
   begin
    ftags:=tstringlist.create;
     holdtags:=intagproperties;
     while pos(',',holdtags)<>0 do
     begin
       ftags.add (trim(copy(holdtags,1,pos(',',holdtags)-1)));
       delete(holdtags,1,pos(',',holdtags));
     end;
     if holdtags<>'' then ftags.add(trim(holdtags));
   end;

   { read through properties and pull appropriate values}
   max:=propertyCount-1;
for i:= 0 to max do
 begin
   thisprop:= propertynames(i);

   idx:=Node.IndexByName(thisprop);
 if idx>-1 then
 if assigned(RTTIObject) then
  begin
    Child:= Node.ChildNode(idx) ;
    if self.propertyTypes(i) <> tkClass then
   begin
      if intagproperties='' then
        SetValue(thisprop,Child.Body)
      else
        begin
        if ftags.indexof(thisprop)=-1 then
          SetValue(thisprop,Child.Body)
          else
          SetValue(thisprop,Child.Properties.Values[thisprop])

        end;
   end
   else
    if assigned(TPersistent(childobject(thisprop))) then
    begin
       {class property}
     Cache.RTTIObject:= TPersistent(childobject(thisprop)); 
       TDeFormRTTI(Cache).binhexdefined:=fbinhex;
     Cache.TagName := thisprop;
     idx:=Node.IndexByName(thisprop);

     if assigned(Cache.rttiobject) then
      if idx>-1 then
       begin
        Child:=Node.childnode(idx);
        holdchildXML:= Child.xml;
        TDeFormRTTI(Cache).xml:=holdchildxml;
       end;
    end;
  end;
end; {cycle thru properties}
if assigned(RTTIOBJECT) then
 if  RTTIObject is TStrings then
   begin
    TStrings(RTTIObject).clear;
    s:=tstringlist.create;
    for j:= 0 to node.ChildNodeCount -1 do
    if assigned(node.childnode(j)) then
     begin
        s.add(node.childnode(j).body);
     end;
    TStrings(RTTIObject).assign (s);
    s.free;
   end;

   if  RTTIObject is TCollection then
   begin
    c:=Tcollection(self.rttiobject);
    c.Clear;
     holdtag:=c.itemclass.classname;
     delete(holdtag,1,1);
    cmax:=node.childnodecount-1;
    for k:= 0 to cmax do
     if assigned(node.childnode(k)) then
       begin
           Child:=node.childnode(k);
           holdclassname:=Child.properties.values['TYPE'];
           ci:= NewRegisteredCollectionItem(holdclassname,c);

           cache.rttiobject:=ci ;
           cache.tagname:=holdtag;     
           TDeFormRTTI(Cache).binhexdefined:=fbinhex;
           TDeFormRTTI(Cache).XML:=node.childnode(k).xml ;
       end;
   end;

   if  RTTIObject is TComponent then
   begin
    compobj:=Tcomponent(self.rttiobject);
    {defined properties}
    cidx:=node.IndexByName ('_DEFINED');
    if cidx>-1 then
   {  begin
     cnode:=node.ChildNode(cidx);
     loadDefinedFromString (DefinedIndex('Data'),cnode.body);
     end;}
     cnode:=node.ChildNode(cidx);
     if DefinedCount>0 then
    begin
      max:=DefinedCount-1;
      for k:= 0 to max do
      begin
      dname:= DefinedNames(k);
      if self.indexof(dname) = -1 then
          if cnode.IndexByName(dname)>-1 then
           begin  
              loadDefinedFromString (DefinedIndex(dname),cnode.ChildNode(cnode.IndexByName(dname)).body)
           end;
      end;
    end;

    {components}
    cidx:=node.IndexByName ('_COMPONENTS');
    if cidx>-1 then
    begin
       cnode:=node.ChildNode(cidx);
       cmax:=cnode.childnodecount-1;
    for k:= 0 to cmax do
     if assigned(cnode.childnode(k)) then
       begin
           Child:=cnode.childnode(k);
           holdclassname:=Child.properties.values['TYPE'];
           compchild:= NewRegisteredComponent(holdclassname,compobj);
           if (compchild is TCOntrol) and (compobj is TWinCOntrol) then
             begin
                TControl(compchild).parent:=TWincontrol(compobj);
             end;
           compchild.name:=child.tag;
           holdtag:=CHild.Tag ;
           cache.rttiobject:=compchild;      
           TDeFormRTTI(Cache).binhexdefined:=fbinhex;
           cache.tagname:=child.tag;
           TDeFormRTTI(Cache).XML:=cnode.childnode(k).xml ;
           {Grab those define props}
       end;
     end{ cidx>-1};

   end;


  end;
end;

end.
