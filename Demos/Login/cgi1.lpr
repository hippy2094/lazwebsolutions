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
    procedure Request; override;
    procedure Respond; override;
  end;

  procedure TCGI.Request;
  var
    VUser, VPass: ShortString;
  begin
    VUser := Fields['usertxt'].AsString;
    VPass := Fields['passtxt'].AsString;
    if (VUser <> ES) and (VPass <> ES) then
    begin
      Contents.LoadFromFile('welcome.html');
      Contents.Text := Format(Contents.Text, [VUser, VPass]);
      Exit;
    end;
    Contents.LoadFromFile('login.html');
    Contents.Add('<hr />Login fail!');
  end;

  procedure TCGI.Respond;
  begin
    if PathInfo = '' then
      Contents.Text :=
        'Forbidden! Please use the ' +
        '<a href="http://localhost/cgi-bin/cgi/login">login</a> page.'
    else
    if PathInfo = '/login' then
      Contents.LoadFromFile('login.html')
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
      Run;
    finally
      Free;
    end;
end.

