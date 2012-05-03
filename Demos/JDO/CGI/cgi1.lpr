program cgi1;

{$mode objfpc}{$H+}

uses
  LWSCGI,
  LWSJDO,
  PQConnection;

resourcestring
  SCouldNotInsert = 'ERROR: Could not insert.';
  SSuccessfullyInserted = '-- Successfully inserted! --';

type

  { TCGI }

  TCGI = class(TLWSCGI)
  private
    FConn: TLWSJDODataBase;
    FQuery: TLWSJDOQuery;
  protected
    procedure Init; override;
    procedure Finit; override;
    procedure Request; override;
  end;

  procedure TCGI.Init;
  begin
    FConn := TLWSJDODataBase.Create('db.cfg');
    FQuery := TLWSJDOQuery.Create(FConn, 'jdo_demo');
  end;

  procedure TCGI.Finit;
  begin
    FQuery.Free;
    FConn.Free;
  end;

  procedure TCGI.Request;
  begin
    FConn.StartTrans;
    try
      FQuery.AddField('ftstr', ftStr);
      FQuery.AddField('ftbool', ftBool);
      FQuery.AddField('ftdate', ftDate);
      FQuery.AddField('ftfloat', ftFloat);
      FQuery.AddField('ftint', ftInt);
      if FQuery.Insert(Fields) then
        Contents.Text := SSuccessfullyInserted
      else
        Contents.Text := SCouldNotInsert;
      FConn.Commit;
    except
      FConn.Rollback;
      raise;
    end;
  end;

begin
  with TCGI.Create do
    try
      Run;
    finally
      Free;
    end;
end.

