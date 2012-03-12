program cgi1;

{$mode objfpc}{$H+}

uses
  LWSCGI,
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
    LastModified := StrToDateTime('01/01/2000');
    inherited;
  end;

  procedure TCGI.Respond;
  begin
    Contents.Add('<!DOCTYPE HTML>');
    Contents.Add('<html lang="en-US">');
    Contents.Add('<head>');
    Contents.Add('	<meta charset="UTF-8">');
    Contents.Add('	<title>LastModified</title>');
    Contents.Add('</head>');
    Contents.Add('<body>');
    Contents.Add('	Hello world!');
    Contents.Add('</head>');
    Contents.Add('<body>');
  end;

begin
  with TCGI.Create do
    try
      Run;
    finally
      Free;
    end;
end.

