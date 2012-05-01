program like01;

{$mode objfpc}{$H+}

uses
  heaptrc,
  LWSJSONDataObjects,
  LWSJSONDataObjectsConsts,
  SysUtils,
  FPJSON,
  PQConnection;

var
  q: TLWSJDOQuery;
  conn: TLWSJDOConnection;
begin
  conn := TLWSJDOConnection.Create('db.cfg');
  q := TLWSJDOQuery.Create(conn, 'jdo_demo');
  try
    conn.StartTrans;
    try
      q.OrderByPK := False;
      q.Like('o', 'ftstr', [loCaseInsensitive, loPartialKey]);
      if q.Open('order by ftstr desc') then
        WriteLn(q.AsJSON)
      else
        WriteLn('No record found');
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

