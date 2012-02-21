program cgi1;

{$mode objfpc}{$H+}

uses
  LWSCGI,
  LWSCookies,
  SysUtils,
  DateUtils;

type

  { TCGI }

  TCGI = class(TLWSCGI)
  private
    FCookies: TLWSCookies;
  protected
    procedure DoFillHeaders; override;
    procedure DoPopulateProperties; override;
    procedure DoResponse; override;
  public
    destructor Destroy; override;
  end;

  destructor TCGI.Destroy;
  begin
    FCookies.Free;
    inherited Destroy;
  end;

  procedure TCGI.DoFillHeaders;
  begin
    Headers.Text := FCookies.Header;
    inherited;
  end;

  procedure TCGI.DoPopulateProperties;
  begin
    inherited;
    FCookies := TLWSCookies.Create(HTTPCookie);
  end;

  procedure TCGI.DoResponse;
  begin
    Contents.Add('<!DOCTYPE HTML>');
    Contents.Add('<html lang="en-US">');
    Contents.Add('<head>');
    Contents.Add('	<meta charset="UTF-8">');
    Contents.Add('	<title>Cookies</title>');
    Contents.Add('</head>');
    Contents.Add('<body>');
    if FCookies.IndexOfName('welcome', True) = -1 then
    begin
      Contents.Add('	Welcome!');
      FCookies.SetCookie('welcome', 'none', IncHour(Now, 3));
    end
    else
      Contents.Add('	Welcome again!');
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

