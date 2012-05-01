program delete02;

{$mode objfpc}{$H+}

uses
  heaptrc,
  LWSJSONDataObjects,
  SysUtils,
  FPJSON,
  PQConnection;

resourcestring
  SCouldNotDelete = 'ERROR: Could not delete.';
  SSuccessfullyDeleted = '-- Successfully deleted! --';

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
      j.Add('id', 1);
      a.Add(j);
      j2.Add('id', 2);
      a.Add(j2);
      if q.Delete(a) then
        WriteLn(SSuccessfullyDeleted)
      else
        WriteLn(SCouldNotDelete);
      conn.Commit;
    except
      conn.Rollback;
      WriteLn(SCouldNotDelete);
      raise;
    end;
  finally
    a.Free;
    q.Free;
    conn.Free;
  end;
end.

