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
    procedure DoFillHeaders; override;
    procedure DoResponse; override;
  end;

  procedure TCGI.DoFillHeaders;
  begin
    CacheControl := LWS_HTTP_CACHE_CONTROL_PUBLIC + ', ' +
      LWS_HTTP_CACHE_CONTROL_MAX_AGE + '10';
    inherited;
  end;

  procedure TCGI.DoResponse;
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

