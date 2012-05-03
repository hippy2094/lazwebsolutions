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
  db: TLWSJDODataBase;
begin
  db := TLWSJDODataBase.Create('db.cfg');
  q := TLWSJDOQuery.Create(db, 'jdo_demo');
  j := TJSONObject.Create;
  try
    db.StartTrans;
    try
      q.AddField('id', ftInt, True);
      j.Add('id', 1);
      if q.Delete(j) then
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
    j.Free;
    q.Free;
    db.Free;
  end;
end.

