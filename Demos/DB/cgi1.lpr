program cgi1;

{$mode objfpc}{$H+}

uses
  LWSCGI,
  JDO,
  PQConnection;

resourcestring
  SCouldNotInsert = 'ERROR: Could not insert.';
  SSuccessfullyInserted = '-- Successfully inserted! --';

type

  { TCGI }

  TCGI = class(TLWSCGI)
  private
    FDB: TJDODataBase;
    FQuery: TJDOQuickQuery;
  protected
    procedure Init; override;
    procedure Finit; override;
    procedure Request; override;
  end;

  procedure TCGI.Init;
  begin
    FDB := TJDODataBase.Create('db.cfg');
    FQuery := TJDOQuickQuery.Create(FDB, 'jdo_demo');
    FQuery.FreeObjects := False;
  end;

  procedure TCGI.Finit;
  begin
    FDB.Free;
  end;

  procedure TCGI.Request;
  begin
    FQuery.AddField('ftstr', ftStr);
    FQuery.AddField('ftbool', ftBool);
    FQuery.AddField('ftdate', ftDate);
    FQuery.AddField('ftfloat', ftFloat);
    FQuery.AddField('ftint', ftInt);
    if FQuery.Insert(Fields) then
      Contents.Text := SSuccessfullyInserted
    else
      Contents.Text := SCouldNotInsert;
  end;

begin
  with TCGI.Create do
    try
      Run;
    finally
      Free;
    end;
end.

