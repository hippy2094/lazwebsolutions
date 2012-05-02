program delete03;

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
  q: TLWSJDOQuery;
  conn: TLWSJDOConnection;
begin
  conn := TLWSJDOConnection.Create('db.cfg');
  q := TLWSJDOQuery.Create(conn, 'jdo_demo');
  a := TJSONArray.Create;
  try
    conn.StartTrans;
    try
      q.AddField('id', ftInt, True);
      a.Add(1);
      a.Add(2);
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
