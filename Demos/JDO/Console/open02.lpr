program open02;

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
  conn: TLWSJDODataBase;
begin
  conn := TLWSJDODataBase.Create('db.cfg');
  q := TLWSJDOQuery.Create(conn, 'jdo_demo');
  try
    conn.StartTrans;
    try
      q.AddField('ftstr', ftStr);
      q.AddField('ftdate', ftDate);
      if q.Open then // Or your SQL filter, e.g:  q.Open('id in (1, 2)');
      begin
        j := q[0];
        WriteLn('Objects: ', q.Count);
        WriteLn('Item 0: ', j.AsJSON);
        WriteLn('Item 1: ', q[1].AsJSON);
        WriteLn(
          'ftstr: ', j['ftstr'].AsString,
          ' ftdate: ', j['ftdate'].AsString);
        WriteLn('First object: ', q.First.AsJSON);
        WriteLn('Last object: ', q.Last.AsJSON);
        WriteLn('AsJSON: ', q.AsJSON);
      end
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

