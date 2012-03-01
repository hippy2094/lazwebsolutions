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
  LWSConsts, LWSUtils, LWSMessages, IOStream, SysUtils, Classes, FPJSON,
  JSONParser;

type
  ELWSCGI = class(Exception);

  TLWSCGIShowExceptionEvent = procedure(var E: Exception) of object;

  TLWSCGIPopulatingPropEvent = procedure(var AName, AValue: string) of object;

  TLWSCGIPopulateFieldsEvent = procedure(AData: TMemoryStream) of object;

  TLWSCGIPopulateUploadsEvent = procedure(AData: TMemoryStream) of object;

  { TLWSCGIMemory }

  TLWSCGIMemory = class(TMemoryStream)
  private
    FLineBreakString: ShortString;
    function GetText: string;
    procedure SetText(const AValue: string);
  public
    constructor Create;
    function AsString: string;
    procedure Add(const AString: string);
    procedure Put(const AString: string);
    property LineBreakString: ShortString read FLineBreakString
      write FLineBreakString;
    property Text: string read GetText write SetText;
  end;

  { TLWSCGI }

  TLWSCGI = class
  private
    FAuthType: ShortString;
    FCacheControl: string;
    FCharset: ShortString;
    FContentEncoding: ShortString;
    FDocumentRoot: string;
    FEnvironmentVariables: TStrings;
    FETag: string;
    FExpires: TDateTime;
    FGatewayInterface: ShortString;
    FHaltOnError: Boolean;
    FHeaderContentType: ShortString;
    FHTTPAcceptEncoding: ShortString;
    FHTTPCookie: string;
    FHTTPIfNoneMatch: string;
    FInputStream: TStream;
    FLastModified: TDateTime;
    FOnPopulateFields: TLWSCGIPopulateFieldsEvent;
    FOnPopulateParams: TNotifyEvent;
    FOnPopulateProperties: TNotifyEvent;
    FOnPopulateUploads: TLWSCGIPopulateUploadsEvent;
    FOnPopulatingProperties: TLWSCGIPopulatingPropEvent;
    FOutputStream: TStream;
    FContentLength: Int64;
    FContents: TLWSCGIMemory;
    FContentType: ShortString;
    FFields: TJSONObject;
    FLocation: string;
    FOnRequest: TNotifyEvent;
    FOnResponse: TNotifyEvent;
    FOnFillHeaders: TNotifyEvent;
    FOnShowException: TLWSCGIShowExceptionEvent;
    FParams: TJSONObject;
    FHeaders: TLWSCGIMemory;
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
    procedure SetLocation(const AValue: string);
  protected
    procedure DoPopulatingProperties(var AName, AValue: string); virtual;
    procedure DoRequest; virtual;
    procedure DoResponse; virtual;
    procedure DoFillHeaders; virtual;
    procedure DoShowException(var E: Exception); virtual;
    procedure DoPopulateParams; virtual;
    procedure DoPopulateFields(AData: TMemoryStream); virtual;
    procedure DoPopulateUploads(AData: TMemoryStream); virtual;
    procedure DoPopulateProperties; virtual;
    procedure Init; virtual;
    procedure Finit; virtual;
    procedure ReadInput; virtual;
    procedure WriteOutput; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Run; virtual;
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
    property Contents: TLWSCGIMemory read FContents;
    property ContentType: ShortString read FContentType write FContentType;
    property DocumentRoot: string read FDocumentRoot;
    property EnvironmentVariables: TStrings read FEnvironmentVariables;
    property ETag: string read FETag write FETag;
    property Expires: TDateTime read FExpires write FExpires;
    property Fields: TJSONObject read FFields;
    property GatewayInterface: ShortString read FGatewayInterface;
    property HaltOnError: Boolean read FHaltOnError write FHaltOnError;
    property Headers: TLWSCGIMemory read FHeaders;
    property HTTPAcceptEncoding: ShortString read FHTTPAcceptEncoding;
    property HTTPCookie: string read FHTTPCookie;
    property HTTPIfNoneMatch: string read FHTTPIfNoneMatch;
    property LastModified: TDateTime read FLastModified write FLastModified;
    property Location: string read FLocation write SetLocation;
    property Params: TJSONObject read FParams;
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
    property OnFillHeaders: TNotifyEvent read FOnFillHeaders write FOnFillHeaders;
    property OnPopulateFields: TLWSCGIPopulateFieldsEvent read FOnPopulateFields
      write FOnPopulateFields;
    property OnPopulateParams: TNotifyEvent read FOnPopulateParams
      write FOnPopulateParams;
    property OnPopulatingProperties: TLWSCGIPopulatingPropEvent
      read FOnPopulatingProperties write FOnPopulatingProperties;
    property OnPopulateProperties: TNotifyEvent
      read FOnPopulateProperties write FOnPopulateProperties;
    property OnPopulateUploads: TLWSCGIPopulateUploadsEvent read FOnPopulateUploads
      write FOnPopulateUploads;
    property OnResponse: TNotifyEvent read FOnResponse write FOnResponse;
    property OnRequest: TNotifyEvent read FOnRequest write FOnRequest;
    property OnShowException: TLWSCGIShowExceptionEvent
      read FOnShowException write FOnShowException;
  end;

implementation

{ TLWSCGIMemory }

function TLWSCGIMemory.GetText: string;
begin
  SetLength(Result, Size);
  Position := 0;
  Read(Pointer(Result)^, Size);
end;

procedure TLWSCGIMemory.SetText(const AValue: string);
begin
  Clear;
  Write(Pointer(AValue)^, Length(AValue));
end;

constructor TLWSCGIMemory.Create;
begin
  FLineBreakString := LineEnding;
end;

procedure TLWSCGIMemory.Add(const AString: string);
var
  S: string;
begin
  S := AString + FLineBreakString;
  Position := Size;
  Write(Pointer(S)^, Length(S));
end;

procedure TLWSCGIMemory.Put(const AString: string);
begin
  Write(Pointer(AString)^, Length(AString));
end;

function TLWSCGIMemory.AsString: string;
var
  VOldPos: Int64;
begin
  try
    VOldPos := Position;
    SetLength(Result, Size);
    Position := 0;
    Read(Pointer(Result)^, Size);
  finally
    Position := VOldPos;
  end;
end;

{ TLWSCGI }

constructor TLWSCGI.Create;
begin
  FContents := TLWSCGIMemory.Create;
  FHeaders := TLWSCGIMemory.Create;
  FEnvironmentVariables := TStringList.Create;
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
  if Assigned(FParams) then
    FreeAndNil(FParams);
  if Assigned(FFields) then
    FreeAndNil(FFields);
  FContents.Free;
  FEnvironmentVariables.Free;
  FFields.Free;
  FHeaders.Free;
  inherited Destroy;
end;

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

procedure TLWSCGI.DoPopulateFields(AData: TMemoryStream);
var
  S: string;
  VJSONParser: TJSONParser;
begin
{$IFDEF DEBUG}
  LWSSendMethodEnter('TLWSCGI.PopulateFields');
{$ENDIF}
  SetLength(S, FContentLength);
  AData.Position := 0;
  AData.Read(Pointer(S)^, FContentLength);
  VJSONParser := TJSONParser.Create(LWSParamStringToJSON(S, '=', '&'));
  try
    FFields := TJSONObject(VJSONParser.Parse);
    if Assigned(FOnPopulateFields) then
      FOnPopulateFields(AData);
  finally
    VJSONParser.Free;
  end;
{$IFDEF DEBUG}
  LWSSendMethodExit('TLWSCGI.PopulateFields');
{$ENDIF}
end;

procedure TLWSCGI.DoPopulateUploads(AData: TMemoryStream);
begin
  if Assigned(FOnPopulateUploads) then
    FOnPopulateUploads(AData);
end;

procedure TLWSCGI.DoPopulateProperties;
var
  I: Integer;
  S, VName, VValue: string;
begin
{$IFDEF DEBUG}
  LWSSendMethodEnter('TLWSCGI.PopulateProperties');
{$ENDIF}
  for I := 1 to LWS_CGI_ENV_COUNT do
  begin
    S := GetEnvironmentString(I);
    if S <> ES then
    begin
      FEnvironmentVariables.Add(S);
      LWSGetVariableNameValue(S, VName, VValue);
      DoPopulatingProperties(VName, VValue);
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
      if VName = LWS_SRV_ENV_SERVER_ADDR then;
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
  if Assigned(FOnPopulateProperties) then
    FOnPopulateProperties(Self);
{$IFDEF DEBUG}
  LWSSendMethodExit('TLWSCGI.PopulateProperties');
{$ENDIF}
end;

procedure TLWSCGI.Init;
begin
end;

procedure TLWSCGI.Finit;
begin
end;

procedure TLWSCGI.DoPopulateParams;
var
  VJSONParser: TJSONParser;
begin
{$IFDEF DEBUG}
  LWSSendMethodEnter('TLWSCGI.PopulateParams');
{$ENDIF}
  if FQueryString <> ES then
  begin
    VJSONParser := TJSONParser.Create(
      LWSParamStringToJSON(FQueryString, '=', '&'));
    try
      FParams := TJSONObject(VJSONParser.Parse);
      if Assigned(FOnPopulateParams) then
        FOnPopulateParams(Self);
    finally
      VJSONParser.Free;
    end;
  end;
{$IFDEF DEBUG}
  LWSSendMethodExit('TLWSCGI.PopulateParams');
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
      DoPopulateFields(VData)
    else
    if Pos(LWS_HTTP_CONTENT_TYPE_MULTIPART_FORM_DATA,
      LowerCase(FContentType)) <> 0 then
      DoPopulateUploads(VData);
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

procedure TLWSCGI.DoRequest;
begin
  if Assigned(FOnRequest) then
    FOnRequest(Self);
end;

procedure TLWSCGI.DoResponse;
begin
  if Assigned(FOnResponse) then
    FOnResponse(Self);
end;

procedure TLWSCGI.InternalShowException;
var
  VHeaders: string;
begin
  if FStatusCode = LWS_HTTP_STATUS_CODE_OK then
  begin
    FStatusCode := LWS_HTTP_STATUS_CODE_INTERNAL_SERVER_ERROR;
    FReasonPhrase := LWS_HTTP_REASON_PHRASE_INTERNAL_SERVER_ERROR;
  end;
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

procedure TLWSCGI.DoPopulatingProperties(var AName, AValue: string);
begin
  if Assigned(FOnPopulatingProperties) then
    FOnPopulatingProperties(AName, AValue);
end;

procedure TLWSCGI.DoShowException(var E: Exception);
var
  VError: string;
begin
  VError := E.Message
{$IFDEF DEBUG}
    + CRLF + CRLF + '------- Call stack -------' + CRLF + CRLF +
    LWSDumpExceptionCallStack
{$ENDIF};
  if FShowExceptionAsHTML then
    FContents.Text := StringReplace(VError, LF, BR, [rfReplaceAll])
  else
    FContents.Text := VError;
  if Assigned(FOnShowException) then
    FOnShowException(E);
end;

procedure TLWSCGI.DoFillHeaders;
var
  VHeaders: string;
begin
{$IFDEF DEBUG}
  LWSSendMethodEnter('TLWSCGI.DoFillHeaders');
{$ENDIF}
  VHeaders := LWS_HTTP_HEADER_STATUS + IntToStr(FStatusCode) + SP +
    FReasonPhrase + CRLF;
  if Trim(FLocation) <> ES { 30x status } then
  begin
    VHeaders += LWS_HTTP_HEADER_LOCATION + FLocation;
    FHeaders.Text := VHeaders + CRLF;
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
  if Assigned(FOnFillHeaders) then
    FOnFillHeaders(Self);
{$IFDEF DEBUG}
  LWSSendMethodExit('TLWSCGI.DoFillHeaders');
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
    DoPopulateProperties;
    DoPopulateParams;
    if (FContentLength > 0) and (FContentType <> ES) then
    begin
      ReadInput;
      DoRequest;
    end
    else
      DoResponse;
    DoFillHeaders;
    WriteOutput;
    Finit;
  except
    on E: Exception do
      try
        DoShowException(E);
      finally
        InternalShowException;
        WriteOutput;
        Finit;
        if FHaltOnError then
          Halt;
      end;
  end;
{$IFDEF DEBUG}
  LWSSendEnd('TLWSCGI.Run', 'Sucess!');
{$ENDIF}
end;

end.

