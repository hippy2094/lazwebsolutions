program cgi1;

{$mode objfpc}{$H+}

uses
  LWSConsts,
  LWSCGI;

type

  { TCGI }

  TCGI = class(TLWSCGI)
  protected
    procedure Respond; override;
  end;

  procedure TCGI.Respond;
  begin
    Contents.Add('23');
    Contents.Add('This is the data in the first chunk');
    Contents.Add('1A');
    Contents.Add('and this is the second one');
    Contents.Add('A');
    Contents.Add('1234567890');
    Contents.Add('0');
  end;

begin
  with TCGI.Create do
    try
      ContentType := LWS_HTTP_CONTENT_TYPE_TEXT_PLAIN;
      TransferEncoding := LWS_HTTP_TRANSFER_ENCODING_CHUNKED;
      Run;
    finally
      Free;
    end;
end.

