program update02;

{$mode objfpc}{$H+}

uses
  heaptrc,
  LWSJSONDataObjects,
  SysUtils,
  FPJSON,
  PQConnection;

resourcestring
  SCouldNotUpdate = 'ERROR: Could not update.';
  SSuccessfullyUpdated = '-- Successfully updated! --';

var
  a: TJSONArray;
  j, j2: TJSONObject;
  q: TLWSJDOQuery;
  conn: TLWSJDOConnection;
begin
  conn := TLWSJDOConnection.Create('db.cfg');
  q := TLWSJDOQuery.Create(conn, 'jdo_demo');
  a := TJSONArray.Create;
  j := TJSONObject.Create;
  j2 := TJSONObject.Create;
  try
    conn.StartTrans;
    try
      q.AddField('id', ftInt, True);
      q.AddField('ftstr', ftStr);
      q.AddField('ftbool', ftBool);
      q.AddField('ftdate', ftDate);
      q.AddField('ftfloat', ftFloat);
      q.AddField('ftint', ftInt);
      j.Add('id', 1);
      j.Add('ftstr', 'TOBALDO');
      j.Add('ftbool', False);
      j.Add('ftdate', Now - 1);
      j.Add('ftfloat', 0.5);
      j.Add('ftint', 789);
      a.Add(j);
      j2.Add('id', 2);
      j2.Add('ftstr', 'MARICOTA');
      j2.Add('ftbool', True);
      j2.Add('ftdate', Now + 2);
      j2.Add('ftfloat', 9.5);
      j2.Add('ftint', 012);
      a.Add(j2);
      if q.Update(a) then
        WriteLn(SSuccessfullyUpdated)
      else
        WriteLn(SCouldNotUpdate);
      conn.Commit;
    except
      conn.Rollback;
      WriteLn(SCouldNotUpdate);
      raise;
    end;
  finally
    a.Free;
    q.Free;
    conn.Free;
  end;
end.

