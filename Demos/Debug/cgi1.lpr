program cgi1;

{$I lazwebsolutions.inc}

uses
{$IFDEF DEBUG}
  LWSDebugger,
{$ENDIF}
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
    Contents.Add('	<title>Debugger</title>');
    Contents.Add('</head>');
    Contents.Add('<body>');
    Contents.Add('	Hello world!');
    Contents.Add('</body>');
    Contents.Add('</html>');
  end;

begin
{$IFDEF DEBUGHEAP}
  SetHeapTraceOutput(LWS_DEBUGHEAP_FILENAME);
{$ENDIF}
  with TCGI.Create do
    try
      Run;
    finally
      Free;
    end;
end.
