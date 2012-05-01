program insert01;

{$mode objfpc}{$H+}

uses
  heaptrc,
  LWSJSONDataObjects, SysUtils, FPJSON, PQConnection;

resourcestring
  SCouldNotInsert = 'ERROR: Could not insert.';
  SSuccessfullyInserted = '-- Successfully inserted! --';

var
  j: TJSONObject;
  q: TLWSJDOQuery;
  conn: TLWSJDOConnection;
begin
  conn := TLWSJDOConnection.Create('db.cfg');
  q := TLWSJDOQuery.Create(conn, 'jdo_demo');
  j := TJSONObject.Create;
  try
    conn.StartTrans;
    try
      q.AddField('ftstr', ftStr);
      q.AddField('ftbool', ftBool);
      q.AddField('ftdate', ftDate);
      q.AddField('ftfloat', ftFloat);
      q.AddField('ftint', ftInt);
      j.Add('ftstr', 'CHIMBICA');
      j.Add('ftbool', True);
      j.Add('ftdate', Now);
      j.Add('ftfloat', 1.5);
      j.Add('ftint', 123);
      if q.Insert(j) then
        WriteLn(SSuccessfullyInserted)
      else
        WriteLn(SCouldNotInsert);
      conn.Commit;
    except
      conn.Rollback;
      WriteLn(SCouldNotInsert);
      raise;
    end;
  finally
    j.Free;
    q.Free;
    conn.Free;
  end;
end.

