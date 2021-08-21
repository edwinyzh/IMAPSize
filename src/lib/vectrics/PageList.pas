{

    Vectrics Logging - distributed logging for Java
    Copyright (C) 2003 -  Mike Moore

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

}

unit PageList;

{$ObjExportAll On}


interface

uses Classes;

type TPageResult = (prOk, prBof, prEof);


type TvPageList = class(TStringList)
public
  procedure DeleteMembers();
private

end;

implementation


procedure TvPageList.DeleteMembers();
var
  i: Integer;
  obj: TObject;
begin
  for i := 0 to Count - 1 do begin
    obj := TObject(Objects[i]);
    if (obj <> nil) then begin
      obj.Destroy();
    end;
  end;
  Clear;
end;


end.


