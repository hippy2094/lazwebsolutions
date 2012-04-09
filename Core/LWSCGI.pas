(*
  LazWebSolutions, CGI unit
  Copyright (C) 2012-2014 Silvio Clecio, Luciano Souza.

  https://github.com/silvioprog/lazwebsolutions

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LWSCGI;

{$I lazwebsolutions.inc}

interface

uses
{$IFDEF DEBUG}
  LWSDebugger,
{$ENDIF}
  LWSConsts, LWSClasses, LWSUtils, LWSMessages, IOStream, SysUtils, Classes,
  FPJSON, JSONParser;

type
  ELWSCGI = class(Exception);

  { TLWSCGI }

  TLWSCGI = class
  private
    FAuthType: ShortString;
    FCacheControl: string;
    FCharset: ShortString;
    FContentEncoding: ShortString;
    FDocumentRoot: string;
    FDomain: string;
    FEnvironmentVariables: TStrings;
    FETag: string;
    FExpires: TDateTime;
    FGatewayInterface: ShortString;
    FHaltOnError: Boolean;
    FHeaderContentType: ShortString;
    FHTTPAcceptEncoding: ShortString;
    FHTTPCookie: string;
    FHTTPIfNoneMatch: string;
    FHTTPReferer: string;
    FInputStream: TStream;
    FLastModified: TDateTime;
    FOutputStream: TStream;
    FContentLength: Int64;
    FContents: TLWSMemoryStream;
    FContentType: ShortString;
    FFields: TJSONObject;
    FLocation: string;
    FParams: TJSONObject;
    FHeaders: TLWSMemoryStream;
    FPathInfo: string;
    FPathTranslated: string;
    FQueryString: string;
    FReasonPhrase: ShortString;
    FRemoteAddr: ShortString;
    FRemoteHost: ShortString;
    FRemoteIdent: ShortString;
    FRemotePort: ShortString;
    FRemoteUser: ShortString;
    FRequestMethod: ShortString;
    FRequestURI: string;
    FScriptFileName: string;
    FScriptName: string;
    FSendContentLength: Boolean;
    FServerAddr: ShortString;
    FServerAdmin: ShortString;
    FServerName: string;
    FServerPort: ShortString;
    FServerProtocol: ShortString;
    FServerSoftware: string;
    FShowExceptionAsHTML: Boolean;
    FShowNotFound: Boolean;
    FStatusCode: Word;
    FTransferEncoding: ShortString;
    FUserAgent: string;
    procedure InternalShowException;
    procedure WriteOutput;
    procedure ReadInput;
    procedure SetLocation(const AValue: string);
  protected
    procedure Init; virtual;
    procedure Finit; virtual;
    procedure FillFields(AData: TMemoryStream); virtual;
    procedure FillHeaders; virtual;
    procedure FillParams; virtual;
    procedure FillProperties; virtual;
    procedure FillingProperties(var AName, AValue: string); virtual;
    procedure FillUploads(AData: TMemoryStream); virtual;
    procedure ShowException(var E: Exception); virtual;
    procedure Request; virtual;
    procedure Respond; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Run;
    procedure AddContentDisposition(const AContentType: ShortString;
      const AFileName: TFileName = ES;
      const ADispositionType: ShortString = LWS_HTTP_CONTENT_DISPOSITION_ATTACHMENT;
      const AContentDescription: ShortString = ES;
      const AModificationDate: TDateTime = NullDate);
    property AuthType: ShortString read FAuthType;
    property CacheControl: string read FCacheControl write FCacheControl;
    property Charset: ShortString read FCharset write FCharset;
    property ContentEncoding: ShortString read FContentEncoding
      write FContentEncoding;
    property ContentLength: Int64 read FContentLength;
    property Contents: TLWSMemoryStream read FContents write FContents;
    property ContentType: ShortString read FContentType write FContentType;
    property DocumentRoot: string read FDocumentRoot;
    property Domain: string read FDomain write FDomain;
    property EnvironmentVariables: TStrings read FEnvironmentVariables
      write FEnvironmentVariables;
    property ETag: string read FETag write FETag;
    property Expires: TDateTime read FExpires write FExpires;
    property Fields: TJSONObject read FFields write FFields;
    property GatewayInterface: ShortString read FGatewayInterface;
    property HaltOnError: Boolean read FHaltOnError write FHaltOnError;
    property Headers: TLWSMemoryStream read FHeaders write FHeaders;
    property HTTPAcceptEncoding: ShortString read FHTTPAcceptEncoding;
    property HTTPCookie: string read FHTTPCookie;
    property HTTPIfNoneMatch: string read FHTTPIfNoneMatch;
    property HTTPReferer: string read FHTTPReferer;
    property LastModified: TDateTime read FLastModified write FLastModified;
    property Location: string read FLocation write SetLocation;
    property Params: TJSONObject read FParams write FParams;
    property PathInfo: string read FPathInfo;
    property PathTranslated: string read FPathTranslated;
    property QueryString: string read FQueryString;
    property ReasonPhrase: ShortString read FReasonPhrase write FReasonPhrase;
    property RemoteAddr: ShortString read FRemoteAddr;
    property RemoteHost: ShortString read FRemoteHost;
    property RemoteIdent: ShortString read FRemoteIdent;
    property RemotePort: ShortString read FRemotePort;
    property RemoteUser: ShortString read FRemoteUser;
    property RequestMethod: ShortString read FRequestMethod;
    property RequestURI: string read FRequestURI;
    property ScriptFileName: string read FScriptFileName;
    property ScriptName: string read FScriptName;
    property ServerAddr: ShortString read FServerAddr;
    property ServerAdmin: ShortString read FServerAdmin;
    property ServerName: string read FServerName;
    property ServerPort: ShortString read FServerPort;
    property ServerProtocol: ShortString read FServerProtocol;
    property ServerSoftware: string read FServerSoftware;
    property SendContentLength: Boolean read FSendContentLength
      write FSendContentLength;
    property ShowExceptionAsHTML: Boolean read FShowExceptionAsHTML
      write FShowExceptionAsHTML;
    property StatusCode: Word read FStatusCode write FStatusCode;
    property ShowNotFound: Boolean read FShowNotFound write FShowNotFound;
    property TransferEncoding: ShortString read FTransferEncoding
      write FTransferEncoding;
    property UserAgent: string read FUserAgent;
  end;

  TLWSCGIClass = class of TLWSCGI;

implementation

{ TLWSCGI }

constructor TLWSCGI.Create;
begin
  FContents := TLWSMemoryStream.Create;
  FHeaders := TLWSMemoryStream.Create;
  FEnvironmentVariables := TStringList.Create;
  FFields := nil;
  FParams := nil;
  FHeaders.LineBreakString := CRLF;
  FHaltOnError := False;
  FContentType := LWS_HTTP_CONTENT_TYPE_TEXT_HTML;
  FCharset := LWS_HTTP_CHARSET_UTF_8;
  FExpires := NullDate;
  FLastModified := NullDate;
  FStatusCode := LWS_HTTP_STATUS_CODE_OK;
  FShowNotFound := True;
  FShowExceptionAsHTML := True;
  FReasonPhrase := LWS_HTTP_REASON_PHRASE_OK;
end;

destructor TLWSCGI.Destroy;
begin
  if Assigned(FFields) then
    FreeAndNil(FFields);
  if Assigned(FParams) then
    FreeAndNil(FParams);
  FContents.Free;
  FEnvironmentVariables.Free;
  FHeaders.Free;
  inherited Destroy;
end;

{$HINTS OFF}
procedure TLWSCGI.FillingProperties(var AName, AValue: string);
begin
end;

procedure TLWSCGI.FillUploads(AData: TMemoryStream);
begin
end;
{$HINTS ON}

procedure TLWSCGI.AddContentDisposition(const AContentType: ShortString;
  const AFileName: TFileName; const ADispositionType: ShortString;
  const AContentDescription: ShortString; const AModificationDate: TDateTime);
var
  VHeaders: string;
begin
{$IFDEF DEBUG}
  LWSSendMethodEnter('TLWSCGI.AddContentDisposition');
{$ENDIF}
  if not FileExists(AFileName) then
    ELWSCGI.Create(SLWSFIleNotFound);
  VHeaders := LWS_HTTP_HEADER_CONTENT_TYPE + AContentType + CRLF +
    LWS_HTTP_HEADER_CONTENT_DISPOSITION + ADispositionType;
  if AFileName <> ES then
  begin
    VHeaders += '; filename=' + DQ + ExtractFileName(AFileName) + DQ;
    if AModificationDate <> NullDate then
      VHeaders += '; modification-date=' + DQ +
        LWSDateTimeToGMT(AModificationDate) + DQ;
    if AContentDescription <> ES then
      VHeaders += CRLF + LWS_HTTP_HEADER_CONTENT_DESCRIPTION +
        AContentDescription;
    if FileExists(AFileName) then
      FContents.LoadFromFile(AFileName);
  end;
  FHeaders.Add(VHeaders);
{$IFDEF DEBUG}
  LWSSendMethodExit('TLWSCGI.AddContentDisposition');
{$ENDIF}
end;

procedure TLWSCGI.FillFields(AData: TMemoryStream);
var
  S: string;
  VJSONParser: TJSONParser;
begin
{$IFDEF DEBUG}
  LWSSendMethodEnter('TLWSCGI.FillFields');
{$ENDIF}
  SetLength(S, FContentLength);
  AData.Position := 0;
  AData.Read(Pointer(S)^, FContentLength);
  VJSONParser := TJSONParser.Create(LWSParamStringToJSON(S, '=', '&'));
  try
    FFields := TJSONObject(VJSONParser.Parse);
  finally
    VJSONParser.Free;
  end;
{$IFDEF DEBUG}
  LWSSendMethodExit('TLWSCGI.FillFields');
{$ENDIF}
end;

procedure TLWSCGI.FillProperties;
var
  I: Integer;
  S, VName, VValue: string;
begin
{$IFDEF DEBUG}
  LWSSendMethodEnter('TLWSCGI.FillProperties');
{$ENDIF}
  for I := 1 to LWS_CGI_ENV_COUNT do
  begin
    S := GetEnvironmentString(I);
    if S <> ES then
    begin
      FEnvironmentVariables.Add(S);
      LWSGetVariableNameValue(S, VName, VValue);
      FillingProperties(VName, VValue);
      if VName = LWS_SRV_ENV_CONTENT_LENGTH then
      begin
        FContentLength := StrToInt64(VValue);
        if FShowNotFound and (FContentLength = 0) then
        begin
          FStatusCode := LWS_HTTP_STATUS_CODE_LENGTH_REQUIRED;
          FReasonPhrase := LWS_HTTP_REASON_PHRASE_LENGTH_REQUIRED;
          raise ELWSCGI.Create(SLWSLengthRequiredError);
        end;
      end;
      if VName = LWS_SRV_ENV_AUTH_TYPE then
        FAuthType := VValue;
      if VName = LWS_SRV_ENV_CONTENT_TYPE then
        FContentType := VValue;
      if VName = LWS_SRV_ENV_DOCUMENT_ROOT then
        FDocumentRoot := VValue;
      if VName = LWS_SRV_ENV_GATEWAY_INTERFACE then
        FGatewayInterface := VValue;
      if VName = LWS_CLT_ENV_HTTP_ACCEPT_ENCODING then
        FHTTPAcceptEncoding := VValue;
      if VName = LWS_CLT_ENV_HTTP_HTTP_COOKIE then
        FHTTPCookie := VValue;
      if VName = LWS_CLT_ENV_HTTP_IF_NONE_MATCH then
        FHTTPIfNoneMatch := VValue;
      if VName = LWS_CLT_ENV_HTTP_REFERER then
        FHTTPReferer := VValue;
      if VName = LWS_SRV_ENV_PATH_INFO then
        FPathInfo := VValue;
      if VName = LWS_SRV_ENV_PATH_TRANSLATED then
        FPathTranslated := VValue;
      if VName = LWS_SRV_ENV_QUERY_STRING then
        FQueryString := VValue;
      if VName = LWS_SRV_ENV_REMOTE_ADDR then
        FRemoteAddr := VValue;
      if VName = LWS_SRV_ENV_REMOTE_HOST then
        FRemoteHost := VValue;
      if VName = LWS_SRV_ENV_REMOTE_IDENT then
        FRemoteIdent := VValue;
      if VName = LWS_SRV_ENV_REMOTE_USER then
        FRemoteUser := VValue;
      if VName = LWS_SRV_ENV_REMOTE_PORT then
        FRemotePort := VValue;
      if VName = LWS_SRV_ENV_REQUEST_METHOD then
        FRequestMethod := VValue;
      if VName = LWS_SRV_ENV_REQUEST_URI then
        FRequestURI := VValue;
      if VName = LWS_SRV_ENV_SERVER_ADDR then
        FServerAddr := VValue;
      if VName = LWS_SRV_ENV_SERVER_ADMIN then
        FServerAdmin := VValue;
      if VName = LWS_SRV_ENV_SCRIPT_FILENAME then
        FScriptFileName := VValue;
      if VName = LWS_SRV_ENV_SCRIPT_NAME then
        FScriptName := VValue;
      if VName = LWS_SRV_ENV_SERVER_NAME then
        FServerName := VValue;
      if VName = LWS_SRV_ENV_SERVER_PORT then
        FServerPort := VValue;
      if VName = LWS_SRV_ENV_SERVER_PROTOCOL then
        FServerProtocol := VValue;
      if VName = LWS_SRV_ENV_SERVER_SOFTWARE then
        FServerSoftware := VValue;
      if VName = LWS_CLT_ENV_HTTP_USER_AGENT then
        FUserAgent := VValue;
    end;
  end;
  if FRequestMethod = ES then
    raise ELWSCGI.Create(SLWSNoREQUEST_METHODPassedFromServerError);
{$IFDEF DEBUG}
  LWSSendMethodExit('TLWSCGI.FillProperties');
{$ENDIF}
end;

procedure TLWSCGI.Init;
begin
end;

procedure TLWSCGI.Finit;
begin
end;

procedure TLWSCGI.FillParams;
var
  VJSONParser: TJSONParser;
begin
{$IFDEF DEBUG}
  LWSSendMethodEnter('TLWSCGI.FillParams');
{$ENDIF}
  if FQueryString <> ES then
  begin
    VJSONParser := TJSONParser.Create(
      LWSParamStringToJSON(FQueryString, '=', '&'));
    try
      FParams := TJSONObject(VJSONParser.Parse);
    finally
      VJSONParser.Free;
    end;
  end;
{$IFDEF DEBUG}
  LWSSendMethodExit('TLWSCGI.FillParams');
{$ENDIF}
end;

procedure TLWSCGI.ReadInput;
var
  VData: TMemoryStream;
begin
{$IFDEF DEBUG}
  LWSSendMethodEnter('TLWSCGI.ReadInput');
{$ENDIF}
  VData := TMemoryStream.Create;
  FInputStream := TIOStream.Create(iosInput);
  try
    VData.CopyFrom(FInputStream, FContentLength);
    if Pos(LWS_HTTP_CONTENT_TYPE_APP_X_WWW_FORM_URLENCODED,
      LowerCase(FContentType)) <> 0 then
      FillFields(VData)
    else
    if Pos(LWS_HTTP_CONTENT_TYPE_MULTIPART_FORM_DATA,
      LowerCase(FContentType)) <> 0 then
      FillUploads(VData);
  finally
    FInputStream.Free;
    VData.Free;
  end;
{$IFDEF DEBUG}
  LWSSendMethodExit('TLWSCGI.ReadInput');
{$ENDIF}
end;

procedure TLWSCGI.WriteOutput;
{$IFDEF DEBUG}
var
  VOutput: TStream;
{$ENDIF}
begin
{$IFDEF DEBUG}
  LWSSendMethodEnter('TLWSCGI.WriteOutput');
  VOutput := TMemoryStream.Create;
{$ENDIF}
  FOutputStream := TIOStream.Create(iosOutPut);
  try
    FHeaders.Position := 0;
    FOutputStream.CopyFrom(FHeaders, FHeaders.Size);
    FOutputStream.Write(CRLF, 2);
    if FTransferEncoding = LWS_HTTP_TRANSFER_ENCODING_CHUNKED then
      FContents.Add(ES);
    FContents.Position := 0;
    FOutputStream.CopyFrom(FContents, FContents.Size);
{$IFDEF DEBUG}
    FHeaders.Position := 0;
    VOutput.CopyFrom(FHeaders, FHeaders.Size);
    VOutput.Write(CRLF, 2);
    FContents.Position := 0;
    VOutput.CopyFrom(FContents, FContents.Size);
    LWSSendStream(VOutput);
{$ENDIF}
  finally
    FOutputStream.Free;
{$IFDEF DEBUG}
    VOutput.Free;
    LWSSendMethodExit('TLWSCGI.WriteOutput', LF);
{$ENDIF}
  end;
end;

procedure TLWSCGI.Request;
begin
end;

procedure TLWSCGI.Respond;
begin
end;

procedure TLWSCGI.InternalShowException;
var
  VHeaders: string;
begin
  VHeaders := LWS_HTTP_HEADER_STATUS + IntToStr(FStatusCode) + SP +
    FReasonPhrase + CRLF + LWS_HTTP_HEADER_CONTENT_TYPE +
    FHeaderContentType + CRLF;
  FHeaders.Text := VHeaders;
end;

procedure TLWSCGI.SetLocation(const AValue: string);
begin
  if AValue <> FLocation then
  begin
    FLocation := AValue;
    FStatusCode := LWS_HTTP_STATUS_CODE_TEMPORARY_REDIRECT;
    FReasonPhrase := LWS_HTTP_REASON_PHRASE_TEMPORARY_REDIRECT;
  end;
end;

procedure TLWSCGI.ShowException(var E: Exception);
begin
  FStatusCode := LWS_HTTP_STATUS_CODE_INTERNAL_SERVER_ERROR;
  FReasonPhrase := LWS_HTTP_REASON_PHRASE_INTERNAL_SERVER_ERROR;
{$IFDEF DEBUG}
  E.Message := E.Message + CRLF + CRLF + '------- Call stack -------' + CRLF +
    CRLF + LWSDumpExceptionCallStack;
{$ENDIF}
  if FShowExceptionAsHTML then
    E.Message := StringReplace(E.Message, LF, BR, [rfReplaceAll]);
  FContents.Text := E.Message;
end;

procedure TLWSCGI.FillHeaders;
var
  VHeaders: string;
begin
{$IFDEF DEBUG}
  LWSSendMethodEnter('TLWSCGI.FillHeaders');
{$ENDIF}
  VHeaders := LWS_HTTP_HEADER_STATUS + IntToStr(FStatusCode) + SP +
    FReasonPhrase + CRLF;
  if Trim(FLocation) <> ES { 30x status } then
  begin
    VHeaders += LWS_HTTP_HEADER_LOCATION + FLocation;
    FHeaders.Add(VHeaders);
  end
  else
  begin
    if FExpires <> NullDate then
    begin
      if FExpires = -1 then
        VHeaders += LWS_HTTP_HEADER_EXPIRES + '-1' + CRLF
      else
        VHeaders += LWS_HTTP_HEADER_EXPIRES + LWSDateTimeToGMT(FExpires) + CRLF;
    end;
    VHeaders += LWS_HTTP_HEADER_CONTENT_TYPE + FHeaderContentType;
    if FCharset <> ES then
      VHeaders += '; charset=' + FCharset;
    if FCacheControl <> ES then
      VHeaders += CRLF + LWS_HTTP_HEADER_CACHE_CONTROL + FCacheControl;
    if FSendContentLength and (FStatusCode <> LWS_HTTP_STATUS_CODE_NOT_FOUND) then
      VHeaders += CRLF + LWS_HTTP_HEADER_CONTENT_LENGTH + IntToStr(FContents.Size);
    if FContentEncoding <> ES then
      VHeaders += CRLF + LWS_HTTP_HEADER_CONTENT_ENCODING + FContentEncoding;
    if FETag <> ES then
      VHeaders += CRLF + LWS_HTTP_HEADER_ETAG + FETag;
    if FLastModified <> NullDate then
      VHeaders += CRLF + LWS_HTTP_HEADER_LAST_MODIFIED +
        LWSDateTimeToGMT(FLastModified);
    if FTransferEncoding <> ES then
      VHeaders += CRLF + LWS_HTTP_HEADER_TRANSFER_ENCODING + FTransferEncoding;
    VHeaders += CRLF + LWS_HTTP_HEADER_X_POWERED_BY + LWS;
    FHeaders.Add(VHeaders);
  end;
{$IFDEF DEBUG}
  LWSSendMethodExit('TLWSCGI.FillHeaders');
{$ENDIF}
end;

procedure TLWSCGI.Run;
begin
{$IFDEF DEBUG}
  LWSSendBegin('TLWSCGI.Run', 'Initializing ...');
{$ENDIF}
  try
    if FContentType = ES then
      raise ELWSCGI.Create(LWS_CONTENT_TYPE_CANT_BE_EMPTY_ERR);
    FHeaderContentType := FContentType;
    Init;
    FillProperties;
    FillParams;
    if (FContentLength > 0) and (FContentType <> ES) then
    begin
      ReadInput;
      Request;
    end
    else
      Respond;
    FillHeaders;
    Finit;
    WriteOutput;
  except
    on E: Exception do
      try
        ShowException(E);
      finally
        InternalShowException;
        WriteOutput;
        if FHaltOnError then
          Halt;
      end;
  end;
{$IFDEF DEBUG}
  LWSSendEnd('TLWSCGI.Run', 'Sucess!');
{$ENDIF}
end;

end.

