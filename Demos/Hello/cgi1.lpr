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
    Contents.Add('<!DOCTYPE HTML>');
    Contents.Add('<html lang="en-US">');
    Contents.Add('<head>');
    Contents.Add('	<meta charset="UTF-8">');
    Contents.Add('	<title>Hello</title>');
    Contents.Add('</head>');
    Contents.Add('<body>');
    Contents.Add('	Hello world! :)');
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
