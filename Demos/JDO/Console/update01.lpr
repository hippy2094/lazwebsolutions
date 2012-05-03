program update01;

{$mode objfpc}{$H+}

uses
  heaptrc,
  LWSJDO,
  SysUtils,
  FPJSON,
  PQConnection;

resourcestring
  SCouldNotUpdate = 'ERROR: Could not update.';
  SSuccessfullyUpdated = '-- Successfully updated! --';

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
      q.AddField('ftstr', ftStr);
      q.AddField('ftbool', ftBool);
      q.AddField('ftdate', ftDate);
      q.AddField('ftfloat', ftFloat);
      q.AddField('ftint', ftInt);
      j.Add('id', 1);
      j.Add('ftstr', 'TOBALDO');
      j.Add('ftbool', False);
      j.Add('ftdate', Now - 1);
      j.Add('ftfloat', 0.5);
      j.Add('ftint', 789);
      if q.Update(j) then
        WriteLn(SSuccessfullyUpdated)
      else
        WriteLn(SCouldNotUpdate);
      db.Commit;
    except
      db.Rollback;
      WriteLn(SCouldNotUpdate);
      raise;
    end;
  finally
    j.Free;
    q.Free;
    db.Free;
  end;
end.

