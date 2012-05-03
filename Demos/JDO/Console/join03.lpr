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
  db: TLWSJDODataBase;
begin
  db := TLWSJDODataBase.Create('db.cfg');
  q := TLWSJDOQuery.Create(db, 'jdo_demo');
  try
    db.StartTrans;
    try
      q.TableAlias := q.TableName;
      q.AddField(q.TableAlias + DT + AK, ftStr);
      q.AddField('jdo_demo_detail.*', ftStr);
      if q.Open(SQL_JOIN) then
        WriteLn(q.AsJSON)
      else
        WriteLn('No record found');
      db.Commit;
    except
      db.Rollback;
      raise;
    end;
  finally
    q.Free;
    db.Free;
  end;
end.

