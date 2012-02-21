program cgi1;

{$mode objfpc}{$H+}

uses
  LWSCGI;

type

  { TCGI }

  TCGI = class(TLWSCGI)
  protected
    procedure DoResponse; override;
  end;

  procedure TCGI.DoResponse;
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
