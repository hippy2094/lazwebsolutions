program open01;

{$mode objfpc}{$H+}

uses
  heaptrc,
  LWSJDO,
  SysUtils,
  FPJSON,
  PQConnection;

var
  q: TLWSJDOQuery;
  j: TJSONObject;
  db: TLWSJDODataBase;
begin
  db := TLWSJDODataBase.Create('db.cfg');
  q := TLWSJDOQuery.Create(db, 'jdo_demo');
  try
    db.StartTrans;
    try
      if q.Open then // Or your SQL filter, e.g:  q.Open('id in (1, 2)');
      begin
        j := q[0];
        WriteLn('Objects: ', q.Count);
        WriteLn('Item 0: ', j.AsJSON);
        WriteLn('Item 1: ', q[1].AsJSON);
        WriteLn(
          'id: ', j['id'].AsInt64,
          ' ftstr: ', j['ftstr'].AsString,
          ' ftbool: ', j['ftbool'].AsString,
          ' ftdate: ', j['ftdate'].AsString,
          ' ftfloat: ', j['ftfloat'].AsFloat,
          ' ftint: ', j['ftint'].AsInt64);
        WriteLn('First object: ', q.First.AsJSON);
        WriteLn('Last object: ', q.Last.AsJSON);
        WriteLn('AsJSON: ', q.AsJSON);
      end
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

