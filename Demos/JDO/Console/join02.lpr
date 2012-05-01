program join02;

{$mode objfpc}{$H+}

uses
  heaptrc,
  LWSJSONDataObjects,
  SysUtils,
  FPJSON,
  PQConnection;

const
  SQL_JOIN =
    'left join jdo_demo_detail on jdo_demo_detail.jdodemoid = jdo_demo.id';

var
  q: TLWSJDOQuery;
  conn: TLWSJDOConnection;
begin
  conn := TLWSJDOConnection.Create('db.cfg');
  q := TLWSJDOQuery.Create(conn, 'jdo_demo');
  try
    conn.StartTrans;
    try
      q.TableAlias := q.TableName;
      if q.Open(SQL_JOIN) then
        WriteLn(q.AsJSON)
      else
        WriteLn('Empty table.');
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

