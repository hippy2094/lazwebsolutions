(*
  LazWebSolutions, Sessions unit
  Copyright (C) 2012-2014 Silvio Clecio, Luciano Souza.

  https://github.com/silvioprog/lazwebsolutions

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LWSSessions;

{$I lazwebsolutions.inc}

interface

uses
{$IFDEF DEBUG}
  LWSDebugger,
{$ENDIF}
  LWSConsts, LWSUtils, SysUtils, Classes, FPJSON, JSONParser;

type
  ELWSSession = class(Exception);

  { TLWSSessions }

  TLWSSessions = class(TJSONObject)
  private
    FIsSetCookieParams: Boolean;
    FSessExpires: TDateTime;
    FSessPath: string;
    FSessDomain: string;
    FSessSecure: Boolean;
    FSessHTTPOnly: Boolean;
    FAutoCommit: Boolean;
    FHTTPCookie: string;
    FJSONString: TJSONStringType;
    FFile: TMemoryStream;
    FWriteJSONFile: Boolean;
    FFileName: TFileName;
    FHeader: string;
    FID: ShortString;
    FName: ShortString;
    FSavePath: string;
    FSID: ShortString;
    FTimeout: LongInt;
    function GetTimeout: LongInt;
    procedure SetSavePath(const AValue: string);
  public
    constructor Create(const AHTTPCookie: string;
      const AStart: Boolean = True); reintroduce;
    destructor Destroy; override;
    procedure MakeID;
    procedure Start; virtual;
    function Finish: Boolean; virtual;
    procedure Commit; virtual;
    procedure SetCookieParams(const AExpires: TDateTime = NullDate;
      const APath: string = ES; const ADomain: string = ES;
      const ASecure: Boolean = False; const AHTTPOnly: Boolean = False);
    procedure RegenerateID(const ADeleteOldSession: Boolean = False);
    procedure Unset;
    function TimeoutAsDateTime: TDateTime;
    procedure Delete(const ABeforeFrom: TDateTime);
    property AutoCommit: Boolean read FAutoCommit write FAutoCommit;
    property Header: string read FHeader;
    property ID: ShortString read FID;
    property Name: ShortString read FName write FName;
    property SavePath: string read FSavePath write SetSavePath;
    property SID: ShortString read FSID;
    property Timeout: LongInt read GetTimeout write FTimeout;
  end;

  TLWSSessionsClass = class of TLWSSessions;

implementation

{ TLWSSessions }

constructor TLWSSessions.Create(const AHTTPCookie: string;
  const AStart: Boolean);
begin
  inherited Create;
  FAutoCommit := True;
  FHTTPCookie := AHTTPCookie;
  FName := LWS_SESSID;
  FSavePath := GetTempDir(True);
  if FSavePath = ES then
    FSavePath := ExtractFilePath(ParamStr(0));
  FTimeout := LWS_SESSION_TIMEOUT;
  if AStart then
    Start;
end;

destructor TLWSSessions.Destroy;
begin
  if FAutoCommit then
    Commit;
  if Assigned(FFile) then
    FFile.Free;
  inherited Destroy;
end;

procedure TLWSSessions.MakeID;
begin
  FID := LWSGetVariableValue(LWSGetRawCookie(FHTTPCookie, FName));
  if FID = ES then
  begin
    FID := LowerCase(LWSMakeSessionID);
    if FIsSetCookieParams then
      LWSSetRawCookie(FHeader, FName, FID, FSessExpires, FSessPath, FSessDomain,
        FSessSecure, FSessHTTPOnly)
    else
      LWSSetRawCookie(FHeader, FName, FID);
    FSID := FName + '=' + FID;
  end;
end;

function TLWSSessions.GetTimeout: LongInt;
var
  VID: ShortString;
begin
  Result := -1;
  VID := FID;
  if VID = ES then
    VID := LWSGetVariableValue(LWSGetRawCookie(FHTTPCookie, FName));
  if VID <> ES then
  begin
    FTimeout := FileAge(FSavePath + LWS_SESSION_PREFIX + VID);
    Result := FTimeout;
  end;
end;

procedure TLWSSessions.SetSavePath(const AValue: string);
begin
  FSavePath := IncludeTrailingPathDelimiter(AValue);
end;

procedure TLWSSessions.Start;
var
  I: Integer;
  VJSONData: TJSONData;
  VJSONParser: TJSONParser;
begin
{$IFDEF DEBUG}
  LWSSendMethodEnter('TLWSSessions.Start');
{$ENDIF}
  if not DirectoryExists(FSavePath) then
    raise ELWSSession.CreateFmt(LWS_SESSION_SAVE_PATH_NOT_FOUND_ERR,
      [FSavePath, ClassName]);
  MakeID;
  FFileName := FSavePath + LWS_SESSION_PREFIX + FID;
  FFile := TMemoryStream.Create;
  try
    if FileExists(FFileName) then
      FFile.LoadFromFile(FFileName);
  except
  end;
  VJSONData := nil;
  VJSONParser := TJSONParser.Create(FFile);
  try
    VJSONData := VJSONParser.Parse;
    Clear;
    if Assigned(VJSONData) then
      for I := 0 to Pred(VJSONData.Count) do
        Add(TJSONObject(VJSONData).Names[I], VJSONData.Items[I].Clone);
  finally
    VJSONData.Free;
    VJSONParser.Free;
  end;
  FWriteJSONFile := True;
{$IFDEF DEBUG}
  LWSSendMethodExit('TLWSSessions.Start');
{$ENDIF}
end;

function TLWSSessions.Finish: Boolean;
begin
{$IFDEF DEBUG}
  LWSSendMethodEnter('TLWSSessions.Finish');
{$ENDIF}
  FWriteJSONFile := False;
  Result := FileExists(FFileName);
  if Result then
    DeleteFile(FFileName);
{$IFDEF DEBUG}
  LWSSendMethodExit('TLWSSessions.Finish');
{$ENDIF}
end;

procedure TLWSSessions.Commit;
begin
{$IFDEF DEBUG}
  LWSSendMethodEnter('TLWSSessions.Commit');
{$ENDIF}
  if Assigned(FFile) and FWriteJSONFile then
  begin
    if not DirectoryExists(FSavePath) then
      raise ELWSSession.CreateFmt(LWS_SESSION_SAVE_PATH_NOT_FOUND_ERR,
        [FSavePath, ClassName]);
    FFile.Clear;
    FJSONString := AsJSON;
    FFile.Write(Pointer(FJSONString)^, Length(FJSONString));
    FFile.SaveToFile(FFileName);
    FreeAndNil(FFile);
  end;
{$IFDEF DEBUG}
  LWSSendMethodExit('TLWSSessions.Commit');
{$ENDIF}
end;

procedure TLWSSessions.SetCookieParams(const AExpires: TDateTime;
  const APath: string; const ADomain: string; const ASecure: Boolean;
  const AHTTPOnly: Boolean);
begin
  FIsSetCookieParams := True;
  FSessExpires := AExpires;
  FSessPath := APath;
  FSessDomain := ADomain;
  FSessSecure := ASecure;
  FSessHTTPOnly := AHTTPOnly;
end;

procedure TLWSSessions.RegenerateID(const ADeleteOldSession: Boolean);
begin
  FID := LowerCase(LWSMakeSessionID);
  if FIsSetCookieParams then
    LWSSetRawCookie(FHeader, FName, FID, FSessExpires, FSessPath, FSessDomain,
      FSessSecure, FSessHTTPOnly)
  else
    LWSSetRawCookie(FHeader, FName, FID);
  if ADeleteOldSession and FileExists(FFileName) then
    DeleteFile(FFileName);
  FWriteJSONFile := True;
  FFileName := FSavePath + LWS_SESSION_PREFIX + FID;
end;

procedure TLWSSessions.Unset;
begin
  Clear;
end;

function TLWSSessions.TimeoutAsDateTime: TDateTime;
var
  T: LongInt;
begin
  T := GetTimeout;
  if T = -1 then
    Result := NullDate
  else
    Result := FileDateToDateTime(GetTimeout);
end;

procedure TLWSSessions.Delete(const ABeforeFrom: TDateTime);
var
  VResult: Integer;
  VFileName: TFileName;
  VSearchRec: TSearchRec;
begin
  VResult := FindFirst(FSavePath + '*', faArchive, VSearchRec);
  try
    while VResult = 0 do
    begin
      VFileName := FSavePath + VSearchRec.Name;
      if (VSearchRec.Name <> '..') and (VSearchRec.Name <> '.') and
        (VFileName <> FFileName) and (LWSFileDate(VFileName) < ABeforeFrom) and
        FileExists(VFileName) then
        DeleteFile(VFileName);
      VResult := FindNext(VSearchRec);
    end;
  finally
    FindClose(VSearchRec);
  end;
end;

end.

