program delete03;

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
  a: TJSONArray;
  q: TLWSJDOQuery;
  db: TLWSJDODataBase;
begin
  db := TLWSJDODataBase.Create('db.cfg');
  q := TLWSJDOQuery.Create(db, 'jdo_demo');
  a := TJSONArray.Create;
  try
    db.StartTrans;
    try
      q.AddField('id', ftInt, True);
      a.Add(1);
      a.Add(2);
      if q.Delete(a) then
        WriteLn(SSuccessfullyDeleted)
      else
        WriteLn(SCouldNotDelete);
      db.Commit;
    except
      db.Rollback;
      WriteLn(SCouldNotDelete);
      raise;
    end;
  finally
    a.Free;
    q.Free;
    db.Free;
  end;
end.

