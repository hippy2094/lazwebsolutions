program open01;

{$mode objfpc}{$H+}

uses
  heaptrc,
  LWSJSONDataObjects,
  SysUtils,
  FPJSON,
  SQLite3ConnDef;

var
  q: TLWSJDOQuery;
  conn: TLWSJDOConnection;
begin
  conn := TLWSJDOConnection.Create('db.cfg');
  q := TLWSJDOQuery.Create(conn, 'jdo_demo');
  try
    conn.StartTrans;
    try
      q.Open;
      WriteLn(q.AsJSON);
      conn.Commit;
    except
      conn.Rollback;
      raise;
    end;
  finally
    q.Free;
    conn.Free;
  end;
end.

