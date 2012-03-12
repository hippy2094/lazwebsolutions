program cgi1;

{$mode objfpc}{$H+}

uses
  LWSCGI,
  LWSConsts,
  SysUtils;

type

  { TCGI }

  TCGI = class(TLWSCGI)
  protected
    procedure FillHeaders; override;
    procedure Respond; override;
  end;

  procedure TCGI.FillHeaders;
  begin
    CacheControl := LWS_HTTP_CACHE_CONTROL_PUBLIC + ', ' +
      LWS_HTTP_CACHE_CONTROL_MAX_AGE + '10';
    inherited;
  end;

  procedure TCGI.Respond;
  begin
    Contents.Add('<!DOCTYPE HTML>');
    Contents.Add('<html lang="en-US">');
    Contents.Add('<head>');
    Contents.Add('	<meta charset="UTF-8">');
    Contents.Add('	<title>CacheControl</title>');
    Contents.Add('</head>');
    Contents.Add('<body>');
    Contents.Add(FormatDateTime('hh:nn:ss', Now));
    Contents.Add('</body>');
    Contents.Add('</html>');
  end;

begin
  with TCGI.Create do
    try
      Run;
    finally
      Free;
    end;
end.

