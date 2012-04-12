(*
  LazWebSolutions, Utils unit
  Copyright (C) 2012-2014 Silvio Clecio, Luciano Souza.

  https://github.com/silvioprog/lazwebsolutions

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LWSUtils;

{$I lazwebsolutions.inc}

interface

uses
  LWSConsts, FPJSON, SysUtils, Classes, MD5, SHA1;

type
  TLWSHTTPAcceptEncodingSet = set of (ceDeflate, ceGzip, ceSdch, ceXGzip);

{ Dump exception call stack.
  (see: http://wiki.lazarus.freepascal.org/Logging_exceptions) }
function LWSDumpExceptionCallStack: string;
{ Encode string to URI. }
function LWSURIEncode(const AString: string): string;
{ Decode URI to string. }
function LWSURIDecode(const AString: string): string;
{ Include path delimiter in an URL. }
function LWSIncludeURLPathDelimiter(const AURL: string): string;
{ Exclude path delimiter in an URL. }
function LWSExcludeURLPathDelimiter(const AURL: string): string;
{ Get variable name. }
function LWSGetVariableName(const AString: string): string;
{ Get variable value. }
function LWSGetVariableValue(const AString: string): string;
{ Get variable name and value. }
procedure LWSGetVariableNameValue(const AString: string;
  out AName, AValue: string);{$IFDEF LWSINLINE}inline;{$ENDIF}
{ Convert parametrized string to JSON. }
function LWSParamStringToJSON(const AParamString: string;
  const AValueSeparator, ADelimiter: Char;
  const AUseURIDecode: Boolean = True): TJSONStringType;
{ TDateTime to GMT. }
function LWSDateTimeToGMT(const ADateTime: TDateTime): string;
{ File date. }
function LWSFileDate(const AFileName: TFileName): TDateTime;
{ Retrieves the number of milliseconds that have elapsed since the
  system was started. }
function LWSGetTickCount: DWord;
{ Trim cookie. }
function LWSTrimCookies(const ACookies: string): string;
{$IFDEF LWSINLINE}inline;{$ENDIF}
{ Milli second to datetime. }
function MSToDT(const AMSec: Double): TDateTime;
{$IFDEF LWSINLINE}inline;{$ENDIF}
function LWSMilliSecondToDateTime(const AMSec: Double): TDateTime;
{ Datetime to milli second. }
function DTToMS(const ADateTime: TDateTime): Double;
{$IFDEF LWSINLINE}inline;{$ENDIF}
function LWSDateTimeToMilliSecond(const ADateTime: TDateTime): Double;
{ Get HTTP_ACCEPT_ENCODING set. }
function LWSGetAcceptEncodingSet(
  const AHTTPAcceptEncoding: ShortString): TLWSHTTPAcceptEncodingSet;
{$IFDEF LWSINLINE}inline;{$ENDIF}
function LWSMakeSessionID: string;{$IFDEF LWSINLINE}inline;{$ENDIF}
{ Set raw cookie. }
procedure LWSSetRawCookie(var AHTTPHeader: string; const AName, AValue: string;
  const AExpires: TDateTime = NullDate; const APath: string = ES;
  const ADomain: string = ES; const ASecure: Boolean = False;
  const AHTTPOnly: Boolean = False);{$IFDEF LWSINLINE}inline;{$ENDIF}
{ Get raw cookie. }
function LWSGetRawCookie(const AHTTPCookie: string; const AName: string): string;
{$IFDEF LWSINLINE}inline;{$ENDIF}
{ Delete cookie. }
procedure LWSDeleteCookie(var AHTTPHeader: string;
  const AName: string; const APath: string = ES; const ADomain: string = ES);
{$IFDEF LWSINLINE}inline;{$ENDIF}
{ Convert a path string to JSON }
function LWSPathToJSON(const APath: string; const ADelimiter: Char;
  const AUseURIDecode: Boolean = True): TJSONStringType;
{$IFDEF LWSINLINE}inline;{$ENDIF}
{ File to string. }
function LWSFileToString(const AFileName: TFileName): string;
{$IFDEF LWSINLINE}inline;{$ENDIF}
{ Show password as asterisks. }
function LWSShowPassword(const APassword: string;
  const AChar: Char = '*'): string;
{ Generate MD5 string from a string. (see: http://en.wikipedia.org/wiki/MD5) }
function LWSMD5(const AString: string): string;
{ Generate SHA1 string from a string. (see: http://en.wikipedia.org/wiki/SHA-1) }
function LWSSHA1(const AString: string): string;
{ Compare two URLs. }
function LWSCompareURL(AURL1, AURL2: string): Integer;

implementation

function LWSDumpExceptionCallStack: string;
var
  I: Integer;
  VFrames: PPointer;
begin
  VFrames := ExceptFrames;
  Result := BackTraceStrFunc(ExceptAddr);
  Result += LineEnding;
  for I := 0 to Pred(ExceptFrameCount) do
  begin
    Result += BackTraceStrFunc(VFrames[I]);
    Result += LineEnding;
  end;
end;

function LWSURIDecode(const AString: string): string;
var
  VHexStr: string[3];
  S1, S2, VResult: PChar;
  VLength, VCode: Integer;
begin
  VLength := Length(AString);
  SetLength(Result, VLength);
  if VLength = 0 then
    Exit;
  S1 := PChar(AString);
  S2 := S1;
  VResult := PChar(Result);
  while (S1 - S2) < VLength do
  begin
    case S1^ of
      '+': VResult^ := ' ';
      '%':
        begin
          Inc(S1);
          if (S1 - S2) < VLength then
          begin
            if S1^ = '%' then
              VResult^ := '%'
            else
            begin
              VHexStr := '$00';
              VHexStr[2] := S1^;
              Inc(S1);
              if (S1 - S2) < VLength then
              begin
                VHexStr[3] := S1^;
                Val(VHexStr, PByte(VResult)^, VCode);
                if VCode <> 0 then
                  VResult^ := ' ';
              end;
            end;
          end;
        end;
      else
        VResult^ := S1^;
    end;
    Inc(VResult);
    Inc(S1);
  end;
  SetLength(Result, VResult - PChar(Result));
end;

function LWSURIEncode(const AString: string): string;
var
  VLength: Integer;
  VHexStr: string[2];
  S1, S2, VResult: PChar;
begin
  VLength := Length(AString);
  SetLength(Result, VLength * 3);
  if VLength = 0 then
    Exit;
  VResult := PChar(Result);
  S1 := PChar(AString);
  S2 := S1;
  while (S1 - S2) < VLength do
  begin
    if S1^ in LWS_HTTP_ALLOWED_CHARS then
      VResult^ := S1^
    else
    if S1^ = ' ' then
      VResult^ := '+'
    else
    begin
      VResult^ := '%';
      VHexStr := HexStr(Ord(S1^), 2);
      Inc(VResult);
      VResult^ := VHexStr[1];
      Inc(VResult);
      VResult^ := VHexStr[2];
    end;
    Inc(VResult);
    Inc(S1);
  end;
  SetLength(Result, VResult - PChar(Result));
end;

function LWSIncludeURLPathDelimiter(const AURL: string): string;
var
  L: Integer;
begin
  Result := AURL;
  L := Length(Result);
  if (L > 0) and (Result[L] <> US) then
    Result += US;
end;

function LWSExcludeURLPathDelimiter(const AURL: string): string;
var
  L : Integer;
begin
  L := Length(AURL);
  if (L > 0) and (AURL[L] = US) then
    Dec(L);
  Result := Copy(AURL, 1, L);
end;

procedure LWSGetVariableNameValue(const AString: string;
  out AName, AValue: string);
var
  VPos: LongInt;
begin
  AName := ES;
  AValue := AString;
  VPos := Pos('=', AValue);
  if VPos <> 0 then
  begin
    AName := Copy(AValue, 1, Pred(VPos));
    Delete(AValue, 1, VPos);
  end;
end;

function LWSGetVariableName(const AString: string): string;
begin
  Result := Copy(AString, 1, Pred(Pos('=', AString)));
end;

function LWSGetVariableValue(const AString: string): string;
var
  VPos: Integer;
begin
  Result := AString;
  VPos := Pos('=', AString);
  if VPos <> 0 then
    Delete(Result, 1, VPos);
end;

function LWSParamStringToJSON(const AParamString: string;
  const AValueSeparator, ADelimiter: Char;
  const AUseURIDecode: Boolean): TJSONStringType;
var
  VChar: Char;
  I, J, VPos: Integer;
  S, VName, VValue, VResult: string;
begin
  Result := StringToJSONString(AParamString);
  if Length(Result) = 0 then
  begin
    Result := '{}';
    Exit;
  end;
  VResult := ES;
  I := 1;
  J := 1;
  while I <= Length(Result) do
  begin
    VChar := Result[I];
    if VChar = AValueSeparator then
      Result[I] := ':';
    if VChar = ADelimiter then
      Result[I] := ',';
    if VChar = ADelimiter then
    begin
      S := Copy(Result, J, I - J);
      VPos := Pos(':', S);
      VName := Copy(S, 1, Pred(VPos));
      VValue := ES;
      if VName <> ES then
        VValue := Copy(S, Succ(VPos), MaxInt);
      VResult += DQ + VName + '": "' + VValue + '", ';
      J := I + 1;
    end;
    Inc(I);
  end;
  if Length(VResult) > 0 then
  begin
    S := Copy(Result, J, I - J);
    VPos := Pos(':', S);
    VName := Copy(S, 1, Pred(VPos));
    VValue := ES;
    if VName <> ES then
      VValue := Copy(S, Succ(VPos), MaxInt);
    VResult += DQ + VName + '": "' + VValue + DQ;
  end
  else
  begin
    VPos := Pos(':', Result);
    VName := Copy(Result, 1, Pred(VPos));
    VValue := ES;
    if VName <> ES then
      VValue := Copy(Result, Succ(VPos), MaxInt);
    VResult := DQ + VName + '": "' + VValue + DQ;
  end;
  if AUseURIDecode then
    Result := '{ ' + LWSURIDecode(VResult) + ' }'
  else
    Result := '{ ' + VResult + ' }';
end;

function LWSDateTimeToGMT(const ADateTime: TDateTime): string;
var
  VYear, VMonth, VDay, VHour, VMinute, VSecond, M: Word;
begin
  DecodeDate(ADateTime, VYear, VMonth, VDay);
  DecodeTime(ADateTime, VHour, VMinute, VSecond, M);
  Result := Format(LWS_GMT_FRMT, [LWS_GMT_DAYS[DayOfWeek(ADateTime)], VDay,
    LWS_GMT_MONTHS[VMonth], VYear, VHour, VMinute, VSecond]);
end;

function LWSFileDate(const AFileName: TFileName): TDateTime;
begin
  if not FileExists(AFileName) then
    raise Exception.Create('File not found: ' + AFileName);
  Result := FileDateToDateTime(FileAge(AFileName));
end;

function LWSGetTickCount: DWord;
begin
  Result := DWord(Trunc(Now * 24 * 60 * 60 * 1000));
end;

function LWSTrimCookies(const ACookies: string): string;
var
  I: LongInt;
begin
  Result := ACookies;
  for I := 1 to Length(Result) do
    if (Result[I] = ';') and (Result[I + 1] = ' ') then
      Delete(Result, I + 1, 1);
end;

function MSToDT(const AMSec: Double): TDateTime;
begin
  Result := AMSec / MSecsPerSec / SecsPerDay;
end;

function LWSMilliSecondToDateTime(const AMSec: Double): TDateTime;
begin
  Result := MSToDT(AMSec);
end;

function DTToMS(const ADateTime: TDateTime): Double;
begin
  Result := ADateTime * HoursPerDay * MinsPerHour * SecsPerMin * MSecsPerSec;
end;

function LWSDateTimeToMilliSecond(const ADateTime: TDateTime): Double;
begin
  Result := DTToMS(ADateTime);
end;

function LWSGetAcceptEncodingSet(const AHTTPAcceptEncoding: ShortString
  ): TLWSHTTPAcceptEncodingSet;
begin
  Result := [];
  if Pos(LWS_HTTP_CONTENT_ENCODING_GZIP, AHTTPAcceptEncoding) <> 0 then
    Include(Result, ceGzip);
  if Pos(LWS_HTTP_CONTENT_ENCODING_DEFLATE, AHTTPAcceptEncoding) <> 0 then
    Include(Result, ceDeflate);
  if Pos(LWS_HTTP_CONTENT_ENCODING_X_GZIP, AHTTPAcceptEncoding) <> 0 then
    Include(Result, ceXGzip);
  if Pos(LWS_HTTP_CONTENT_ENCODING_SDCH, AHTTPAcceptEncoding) <> 0 then
    Include(Result, ceSdch);
end;

function LWSMakeSessionID: string;
var
  VGUID: TGUID;
begin
  CreateGUID(VGUID);
  SetLength(Result, 32);
  StrLFmt(PChar(Result), 32,'%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x',
    [VGUID.D1, VGUID.D2, VGUID.D3, VGUID.D4[0], VGUID.D4[1], VGUID.D4[2],
     VGUID.D4[3], VGUID.D4[4], VGUID.D4[5], VGUID.D4[6], VGUID.D4[7]]);
end;

procedure LWSSetRawCookie(var AHTTPHeader: string; const AName, AValue: string;
  const AExpires: TDateTime; const APath: string; const ADomain: string;
  const ASecure: Boolean; const AHTTPOnly: Boolean);
begin
  if AHTTPHeader <> ES then
    AHTTPHeader += CRLF;
  AHTTPHeader += LWS_HTTP_HEADER_SET_COOKIE + AName + '=' + AValue;
  if AExpires <> NullDate then
    AHTTPHeader += '; Expires=' + LWSDateTimeToGMT(AExpires);
  if APath <> ES then
    AHTTPHeader += '; Path=' + APath;
  if ADomain <> ES then
    AHTTPHeader += '; Domain=' + ADomain;
  if ASecure then
    AHTTPHeader += '; Secure';
  if AHTTPOnly then
    AHTTPHeader += '; HttpOnly';
end;

function LWSGetRawCookie(const AHTTPCookie: string; const AName: string): string;
var
  VBegin, VEnd: LongInt;
begin
  VBegin := Pos(AName + '=', AHTTPCookie);
  if VBegin > 0 then
  begin
    Result := Copy(AHTTPCookie, VBegin, MaxInt);
    VEnd := Pos(';', Result);
    if VEnd > 0 then
      Result := Copy(AHTTPCookie, VBegin, VEnd - 1);
  end;
end;

procedure LWSDeleteCookie(var AHTTPHeader: string; const AName: string;
  const APath: string; const ADomain: string);
begin
  if AHTTPHeader <> ES then
    AHTTPHeader += CRLF;
  AHTTPHeader += LWS_HTTP_HEADER_SET_COOKIE + AName + '=';
  if APath <> ES then
    AHTTPHeader += '; Path=' + APath;
  if ADomain <> ES then
    AHTTPHeader += '; Domain=' + ADomain;
  AHTTPHeader += '; Expires=' + NullCookieExpires;
end;

function LWSPathToJSON(const APath: string; const ADelimiter: Char;
  const AUseURIDecode: Boolean): TJSONStringType;
var
  S: string;
  I, L: LongInt;
begin
  Result := APath;
  L := Length(Result);
  if L < 2 then
  begin
    Result := '[]';
    Exit;
  end;
  S := ES;
  for I := 1 to L do
    if Result[I] = ADelimiter then
    begin
      if (I = 1) or (I = L) then
        S := S + DQ
      else
        S := S + '", "';
    end
    else
      S := S + Result[I];
  if S[1] <> DQ then
    Insert(DQ, S, 1);
  L := Length(S);
  if S[L] <> DQ then
    Insert(DQ, S, L + 1);
  if AUseURIDecode then
    Result := '[' + LWSURIDecode(S) + ']'
  else
    Result := '[' + S + ']';
end;

function LWSFileToString(const AFileName: TFileName): string;
var
  L: LongInt;
begin
  with TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite) do
  try
    L := Size;
    SetLength(Result, L);
    Read(Pointer(Result)^, L);
  finally
    Free;
  end;
end;

function LWSShowPassword(const APassword: string; const AChar: Char): string;
begin
  if APassword = ES then
    Result := ES
  else
    Result := StringOfChar(AChar, Length(APassword));
end;

function LWSMD5(const AString: string): string;
begin
  Result := MD5Print(MD5String(AString));
end;

function LWSSHA1(const AString: string): string;
begin
  Result := SHA1Print(SHA1String(AString));
end;

function LWSCompareURL(AURL1, AURL2: string): Integer;
begin
  AURL1 := LWSIncludeURLPathDelimiter(AURL1);
  AURL2 := LWSIncludeURLPathDelimiter(AURL2);
  Result := CompareText(AURL1, AURL2);
end;

end.

