program cgi1;

{$mode objfpc}{$H+}

uses
  LWSConsts,
  LWSCGI,
  SysUtils;

type

  { TCGI }

  TCGI = class(TLWSCGI)
  protected
    procedure ShowException(var E: Exception); override;
    procedure Respond; override;
  end;

  procedure TCGI.ShowException(var E: Exception);
  begin
    Contents.LoadFromFile('error.html');
    Contents.Text := Format(Contents.Text, [E.Message]);
  end;

  procedure TCGI.Respond;
  begin
    if (PathInfo = '') or (PathInfo = '/home') then
      Contents.LoadFromFile('home.html')
    else
    if PathInfo = '/download' then
      Contents.LoadFromFile('download.html')
    else
    if PathInfo = '/about' then
      Contents.LoadFromFile('about.html')
    else
    begin
      StatusCode := LWS_HTTP_STATUS_CODE_NOT_FOUND;
      ReasonPhrase := LWS_HTTP_REASON_PHRASE_NOT_FOUND;
      Contents.LoadFromFile('404.html');
    end;
  end;

begin
  with TCGI.Create do
    try
      Charset := LWS_HTTP_CHARSET_UTF_8;
      Run;
    finally
      Free;
    end;
end.
