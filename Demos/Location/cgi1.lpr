program cgi1;

{$mode objfpc}{$H+}

uses
  LWSCGI;

type

  { TCGI }

  TCGI = class(TLWSCGI)
  protected
    procedure Respond; override;
  end;

  procedure TCGI.Respond;
  begin
    Location := 'https://www.google.com/';
  end;

begin
  with TCGI.Create do
    try
      Run;
    finally
      Free;
    end;
end.
