program cgi1;

{$mode objfpc}{$H+}

uses
  LWSConsts,
  LWSCGI;

const
  CETAG = 'e34db6e768884b30af72e353bfc83e6a';

type

  { TCGI }

  TCGI = class(TLWSCGI)
  protected
    procedure DoFillHeaders; override;
    procedure DoResponse; override;
  end;

  procedure TCGI.DoFillHeaders;
  begin
    ETag := CETAG;
    inherited;
  end;

  procedure TCGI.DoResponse;
  begin
    Contents.Add('<!DOCTYPE HTML>');
    Contents.Add('<html lang="en-US">');
    Contents.Add('<head>');
    Contents.Add('	<meta charset="UTF-8">');
    Contents.Add('	<title>ETag</title>');
    Contents.Add('</head>');
    Contents.Add('<body>');
    if HTTPIfNoneMatch = CETAG then
    begin
      StatusCode := LWS_HTTP_STATUS_CODE_NOT_MODIFIED;
      ReasonPhrase := LWS_HTTP_REASON_PHRASE_NOT_MODIFIED;
    end
    else
      Contents.Add('	Hello world!');
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

