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
    procedure FillHeaders; override;
    procedure FillProperties; override;
    procedure Respond; override;
  public
    destructor Destroy; override;
  end;

  destructor TCGI.Destroy;
  begin
    FCookies.Free;
    inherited Destroy;
  end;

  procedure TCGI.FillHeaders;
  begin
    Headers.Text := FCookies.Header;
    inherited;
  end;

  procedure TCGI.FillProperties;
  begin
    inherited;
    FCookies := TLWSCookies.Create(HTTPCookie);
  end;

  procedure TCGI.Respond;
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
      FCookies.SetCookie('welcome', 'none', IncHour(Now, 10));
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

