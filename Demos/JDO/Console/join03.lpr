program join03;

{$mode objfpc}{$H+}

uses
  heaptrc,
  LWSJDO,
  LWSJDOConsts,
  SysUtils,
  FPJSON,
  PQConnection;

const
  SQL_JOIN =
    'left join jdo_demo_detail on jdo_demo_detail.jdodemoid = jdo_demo.id';

var
  q: TLWSJDOQuery;
  conn: TLWSJDODataBase;
begin
  conn := TLWSJDODataBase.Create('db.cfg');
  q := TLWSJDOQuery.Create(conn, 'jdo_demo');
  try
    conn.StartTrans;
    try
      q.TableAlias := q.TableName;
      q.AddField(q.TableAlias + DT + AK, ftStr);
      q.AddField('jdo_demo_detail.*', ftStr);
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

