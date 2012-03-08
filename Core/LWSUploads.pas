(*
  LazWebSolutions, Uploads unit
  Copyright (C) 2012-2014 Silvio Clecio, Luciano Souza.

  https://github.com/silvioprog/lazwebsolutions

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LWSUploads;

{$I lazwebsolutions.inc}

interface

uses
{$IFDEF DEBUG}
  LWSDebugger,
{$ENDIF}
  LWSConsts, Classes, SysUtils, FPJSON;

type
  PJSONObject = ^TJSONObject;

  ELWSUpload = class(Exception);

  { TLWSUploadItem }

  TLWSUploadItem = class(TCollectionItem)
  private
    FContent: string;
    FContentDisposition: ShortString;
    FContentLength: LongInt;
    FContentType: ShortString;
    FFieldName: ShortString;
    FFileName: string;
    FIsFile: Boolean;
  protected
    procedure DoParseLine; virtual;
  public
    procedure ContentStream(AStream: TStream);
    property Content: string read FContent;
    property ContentDisposition: ShortString read FContentDisposition;
    property ContentLength: LongInt read FContentLength;
    property ContentType: ShortString read FContentType;
    property FieldName: ShortString read FFieldName;
    property FileName: string read FFileName;
    property IsFile: Boolean read FIsFile;
  end;

  TLWSUploadItemClass = class of TLWSUploadItem;

  { TLWSUploads }

  TLWSUploads = class(TCollection)
  private
    FSavePath: string;
    function GetItem(AIndex: Integer): TLWSUploadItem;
    procedure SetItem(AIndex: Integer; const AValue: TLWSUploadItem);
    procedure SetSavePath(const AValue: string);
  protected
    procedure DoSplitForm(var AContent: string; const ABoundary: string); virtual;
  public
    constructor Create;
    procedure ReadUploads(AData: TMemoryStream; AFields: PJSONObject;
      const ABoundary: string); virtual;
    function Add: TLWSUploadItem;
    function IndexOfFile(const AName: string): Integer;
    function FileByName(const AName: string): TLWSUploadItem;
    function FindFile(const AName: string): TLWSUploadItem;
    property Items[AIndex: Integer]: TLWSUploadItem read GetItem
      write SetItem; default;
    property SavePath: string read FSavePath write SetSavePath;
  end;

  TLWSUploadsClass = class of TLWSUploads;

implementation

{ TLWSUploadItem }

procedure TLWSUploadItem.DoParseLine;

  function GetLine(var S: string): string;
  var
    P: Integer;
  begin
    P := Pos(CRLF, S);
    if P <> 0 then
    begin
      Result := Copy(S, 1, P - 1);
      Delete(S, 1, P + 1);
    end;
  end;

  function GetWord(var S: string): string;
  var
    C: Char;
    VQuoted: Boolean;
    I, VLength: Integer;
  begin
    VLength := Length(S);
    VQuoted := False;
    Result := ES;
    for I := 1 to VLength do
    begin
      C := S[I];
      if C = DQ then
        VQuoted := not VQuoted
      else
      begin
        if not (C in [' ', '=', ';', ':']) or VQuoted then
          Result += C;
        if (C in [';', ':', '=']) and not VQuoted then
        begin
          Delete(S, 1, I);
          Exit;
        end;
      end;
    end;
    S := ES;
  end;

var
  S, VLine: string;
  VLength: Integer;
begin
{$IFDEF DEBUG}
  LWSSendMethodEnter('TLWSUploadItem.DoParseLine');
{$ENDIF}
  VLine := GetLine(FContent);
  while VLine <> ES do
  begin
    S := GetWord(VLine);
    while S <> ES do
    begin
      if CompareText(S, 'Content-Disposition') = 0 then
        FContentDisposition := GetWord(VLine)
      else
      if CompareText(S, 'name') = 0 then
        FFieldName := GetWord(VLine)
      else
      if CompareText(S, 'filename') = 0 then
      begin
        FFileName := GetWord(VLine);
        FIsFile := True;
      end
      else
      if CompareText(S, 'Content-Type') = 0 then
        FContentType := GetWord(VLine);
      S := GetWord(VLine);
    end;
    VLine := GetLine(FContent);
  end;
  VLength := Length(FContent);
  if VLength > 2 then
    FContent := Copy(FContent, 1, VLength - 2);
{$IFDEF DEBUG}
  LWSSendMethodExit('TLWSUploadItem.DoParseLine');
{$ENDIF}
end;

procedure TLWSUploadItem.ContentStream(AStream: TStream);
begin
  AStream.Write(Pointer(FContent)^, Length(FContent));
end;

{ TLWSUploads }

constructor TLWSUploads.Create;
begin
  inherited Create(TLWSUploadItem);
  FSavePath := GetTempDir(True);
  if FSavePath = ES then
    FSavePath := ExtractFilePath(ParamStr(0));
end;

function TLWSUploads.Add: TLWSUploadItem;
begin
  Result := inherited Add as TLWSUploadItem;
end;

function TLWSUploads.IndexOfFile(const AName: string): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (CompareText(Items[Result].FileName, AName) <> 0) do
    Dec(Result);
end;

function TLWSUploads.FileByName(const AName: string): TLWSUploadItem;
begin
  Result := FindFile(AName);
  if not Assigned(Result) then
    raise ELWSUpload.CreateFmt(LWS_UPLOAD_NO_SUCH_UPLOADED_FILE_ERR, [AName]);
end;

function TLWSUploads.FindFile(const AName: string): TLWSUploadItem;
var
  I: Integer;
begin
  I := IndexOfFile(AName);
  if I = -1 then
    Result := nil
  else
    Result := Items[I];
end;

function TLWSUploads.GetItem(AIndex: Integer): TLWSUploadItem;
begin
  Result := inherited GetItem(AIndex) as TLWSUploadItem;
end;

procedure TLWSUploads.SetItem(AIndex: Integer; const AValue: TLWSUploadItem);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TLWSUploads.SetSavePath(const AValue: string);
begin
  FSavePath := IncludeTrailingPathDelimiter(AValue);
end;

procedure TLWSUploads.DoSplitForm(var AContent: string; const ABoundary: string);
var
  VSeparator: string;
  VItem: TLWSUploadItem;
  VContentLength, VSeparatorLength, P: LongInt;
begin
{$IFDEF DEBUG}
  LWSSendMethodEnter('TLWSUploads.DoSplitForm');
{$ENDIF}
  VSeparator := '--' + ABoundary + CRLF;
  VSeparatorLength := Length(VSeparator);
  VContentLength := Pos('--' + ABoundary + '--', AContent);
  AContent := Copy(AContent, 1, VContentLength - 1);
  System.Delete(AContent, 1, VSeparatorLength);
  VContentLength := Length(AContent);
  while VContentLength > 0 do
  begin
    VItem := Add;
    P := Pos(VSeparator, AContent);
    if P = 0 then
      P := VContentLength + 1;
    VItem.FContent := Copy(AContent, 1, P - 1);
    System.Delete(AContent, 1, P + VSeparatorLength - 1);
    VContentLength := Length(AContent);
  end;
{$IFDEF DEBUG}
  LWSSendMethodExit('TLWSUploads.DoSplitForm');
{$ENDIF}
end;

procedure TLWSUploads.ReadUploads(AData: TMemoryStream; AFields: PJSONObject;
  const ABoundary: string);
var
  VFile: TStream;
  VFileName: TFileName;
  VItem: TLWSUploadItem;
  I, VContentLength: Integer;
  S, VFieldName, VFieldValue, VBoundary: string;
begin
{$IFDEF DEBUG}
  LWSSendMethodEnter('TLWSUploads.ReadUploads');
{$ENDIF}
  if not DirectoryExists(FSavePath) then
    raise ELWSUpload.CreateFmt(LWS_UPLOAD_SAVE_PATH_NOT_FOUND_ERR,
      [FSavePath, ClassName]);
  I := Pos('=', ABoundary);
  VBoundary := Copy(ABoundary, I + 1, Length(ABoundary) - I);
  I := Length(VBoundary);
  if (I > 0) and (VBoundary[1] = DQ) then
    VBoundary := Copy(VBoundary, 2, I - 2);
  VContentLength := AData.Size;
  SetLength(S, VContentLength);
  AData.Position := 0;
  AData.Read(Pointer(S)^, VContentLength);
  DoSplitForm(S, VBoundary);
  AFields^ := TJSONObject.Create([]);
  for I := 0 to Pred(Count) do
  begin
    VItem := GetItem(I);
    VItem.DoParseLine;
    if VItem.FFieldName = ES then
      raise Exception.CreateFmt(LWS_UPLOAD_INVALID_MULTIPART_ENCODING_ERR,
        [VItem.FContent]);
    VFieldName := VItem.FFieldName;
    if VItem.FIsFile then
    begin
      VFieldValue := VItem.FFileName;
      VContentLength := Length(VItem.FContent);
      if (VContentLength = 0) or ((VContentLength = 2) and
        (VItem.FContent = CRLF)) then
        VFileName := ES
      else
      begin
        VItem.FContentLength := VContentLength;
        VFileName := FSavePath + VItem.FFileName;
        VFile := TFileStream.Create(VFileName, fmCreate);
        try
          VFile.Write(Pointer(VItem.FContent)^, VItem.FContentLength);
        finally
          VFile.Free;
        end;
      end;
    end
    else
      VFieldValue := VItem.FContent;
    AFields^.Add(VFieldName, VFieldValue);
  end;
{$IFDEF DEBUG}
  LWSSendMethodExit('TLWSUploads.ReadUploads');
{$ENDIF}
end;

end.

