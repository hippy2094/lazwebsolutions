program cgi1;

{$mode objfpc}{$H+}

uses
  LWSCGI,
  LWSUtils,
  LWSConsts,
  ZStream;

const
  CONTENT =
    '<!DOCTYPE HTML>' + LF +
    '<html lang="en-US">' + LF +
    '<head>' + LF +
    '	<meta charset="UTF-8">' + LF +
    '	<title>Deflate</title>' + LF +
    '</head>' + LF +
    '<body>' + LF +
    '	Hello world!' + LF +
    '</body>' + LF +
    '</html>';

type

  { TCGI }

  TCGI = class(TLWSCGI)
  private
    FDeflate: Boolean;
  protected
    procedure DoFillHeaders; override;
    procedure DoResponse; override;
  end;

  procedure TCGI.DoFillHeaders;
  begin
    if FDeflate then
      ContentEncoding := LWS_HTTP_CONTENT_ENCODING_DEFLATE;
    inherited;
  end;

  procedure TCGI.DoResponse;
  var
    S: string;
    VDeflate: TCompressionStream;
  begin
    FDeflate := ceDeflate in LWSGetAcceptEncodingSet(HTTPAcceptEncoding);
    if FDeflate then
    begin
      VDeflate := TCompressionStream.Create(clMax, Contents, True);
      try
        S := CONTENT;
        VDeflate.Write(Pointer(S)^, Length(S));
      finally
        VDeflate.Free;
      end;
    end
    else
      Contents.Text := CONTENT;
  end;

begin
  with TCGI.Create do
    try
      Run;
    finally
      Free;
    end;
end.

