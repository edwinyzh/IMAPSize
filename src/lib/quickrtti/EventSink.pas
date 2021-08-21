unit EventSink;

interface


TOnContainedEvent = procedure (Sender:TObject; EventName:String; );



TEventSink = class(TCollectionItem) ;
  private
   fonevent:TContainedEvent;
  published
   property OnCOntainedEvent:TOnContainedEvent read fonevent write fevent;
  end;

TNotifyEventSink = class(TEventSink)
  public
   procedure Event(Sender:TObject);
  end;


 TCanResizeEvent
 TCloseEvent
 TCloseQueryEvent
 TConstrainedResizeEvent
 TContextPopupEvent
 TDockDropEvent
 TDockOverEvent
 TDragDropEvent
 TDragOverEvent
 TEndDragEvent
 TGetSiteInfoEvent
 THelpEvent
 TNotifyEvent
 TKeyEvent
 TKeyPressEvent
 TMouseEvent
 TMouseMoveEvent
 TMouseWheelEvent
 TMouseWheelUpDownEvent
 TMouseWheelUpDownEvent
 TShortCutEvent
 TStartDockEvent
 TUnDockEvent
 TContextPopupEvent
 TDragDropEvent
 TDragOverEvent
 TEndDragEvent
 TStartDragEvent
 


TEventContainer = class(TCollectionItem)
  private
    fsinks:TCollection
    fObj:TPersistent;
  published
    property SinkObject:TPersistent read fobj write SetObject;
  end;

implementation

end.
