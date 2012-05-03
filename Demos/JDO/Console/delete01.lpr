program delete01;

{$mode objfpc}{$H+}

uses
  heaptrc,
  LWSJDO,
  SysUtils,
  FPJSON,
  PQConnection;

resourcestring
  SCouldNotDelete = 'ERROR: Could not delete.';
  SSuccessfullyDeleted = '-- Successfully deleted! --';

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
      q.AddField('id', ftInt, True);
      j.Add('id', 1);
      if q.Delete(j) then
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
    j.Free;
    q.Free;
    conn.Free;
  end;
end.

