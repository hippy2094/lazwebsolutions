program cgi1;

{$mode objfpc}{$H+}

uses
  LWSConsts,
  LWSCGI;

type

  { TCGI }

  TCGI = class(TLWSCGI)
  protected
    procedure FillHeaders; override;
  end;

  procedure TCGI.FillHeaders;
  begin
    AddContentDisposition(LWS_HTTP_CONTENT_TYPE_IMAGE_PNG,
      '/home/your-home/lazwebsolutions/Demos/ContentDisposition/lws.png',
      LWS_HTTP_CONTENT_DISPOSITION_INLINE);
  end;

begin
  with TCGI.Create do
    try
      Run;
    finally
      Free;
    end;
end.

