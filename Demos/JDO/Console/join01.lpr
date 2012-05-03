program join01;

{$mode objfpc}{$H+}

uses
  heaptrc,
  LWSJDO,
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
      q.AddField('ftstr', ftStr);
      q.AddField('jdo_demo_detail.ftstr', ftStr);
      if q.Open(SQL_JOIN) then
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

