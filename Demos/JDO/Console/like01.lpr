program like01;

{$mode objfpc}{$H+}

uses
  heaptrc,
  LWSJDO,
  LWSJDOConsts,
  SysUtils,
  FPJSON,
  PQConnection;

var
  q: TLWSJDOQuery;
  db: TLWSJDODataBase;
begin
  db := TLWSJDODataBase.Create('db.cfg');
  q := TLWSJDOQuery.Create(db, 'jdo_demo');
  try
    db.StartTrans;
    try
      q.OrderByPK := False;
      q.Like('o', 'ftstr', [loCaseInsensitive, loPartialKey]);
      if q.Open('order by ftstr desc') then
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

