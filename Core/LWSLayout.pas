(*
  LazWebSolutions, Layout unit
  Copyright (C) 2012-2014 Silvio Clecio, Luciano Souza.

  https://github.com/silvioprog/lazwebsolutions

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LWSLayout;

{$I lazwebsolutions.inc}

interface

uses
  LWSConsts, Classes, SysUtils, FPJSON;

type
  ELWSLayout = class(Exception);

  { TLWSLayout }

  TLWSLayout = class(TJSONObject)
  private
    FContent: string;
    FPath: string;
    FRecursive: Boolean;
    procedure SetPath(const AValue: string);
  public
    constructor Create(const AElements: array of const;
      const ALayoutPath: string = '';
      const AAutoLoadLayout: Boolean = True); overload;
    function LoadFileToString(const AFileName: TFileName): string;
    procedure LoadFromFile(const AFileName: TFileName);
    procedure Format; virtual;
    function GetFormatedContent(const ARecursive: Boolean = False): string;
    property Content: string read FContent;
    property Path: string read FPath write SetPath;
    property Recursive: Boolean read FRecursive write FRecursive;
  end;

  TLWSLayoutClass = class of TLWSLayout;

implementation

{ TLWSLayout }

constructor TLWSLayout.Create(const AElements: array of const;
  const ALayoutPath: string; const AAutoLoadLayout: Boolean);
begin
  inherited Create(AElements);
  if ALayoutPath = '' then
    SetPath(ExtractFilePath(ParamStr(0)) + 'views' + DirectorySeparator)
  else
    SetPath(ALayoutPath);
  if AAutoLoadLayout then
    LoadFromFile(LWS_DEFAULT_LAYOUT_FILENAME);
end;

function TLWSLayout.LoadFileToString(const AFileName: TFileName): string;
var
  L: LongInt;
begin
  with TFileStream.Create(AFileName, fmOpenRead) do
  try
    L := Size;
    SetLength(Result, L);
    Read(Pointer(Result)^, L);
  finally
    Free;
  end;
end;

procedure TLWSLayout.Format;
var
  I, L: Integer;
  J, P: LongInt;
  VName, VValue: string;
begin
  for I := 0 to Pred(Count) do
  begin
    VName := '@' + Names[I];
    VValue := Items[I].AsString;
    if FRecursive then
      FContent := StringReplace(FContent, VName, VValue,
        [rfIgnoreCase, rfReplaceAll])
    else
      for J := 1 to Length(FContent) do
      begin
        P := Pos(VName, FContent);
        if P <> 0 then
        begin
          L := Length(VName);
          System.Delete(FContent, P, L);
          Insert(VValue, FContent, P);
          Break;
        end;
      end;
  end;
end;

function TLWSLayout.GetFormatedContent(const ARecursive: Boolean): string;
begin
  FRecursive := ARecursive;
  Format;
  Result := FContent;
end;

procedure TLWSLayout.SetPath(const AValue: string);
begin
  FPath := IncludeTrailingPathDelimiter(AValue);
end;

procedure TLWSLayout.LoadFromFile(const AFileName: TFileName);
var
  L: LongInt;
begin
  if not DirectoryExists(FPath) then
    raise ELWSLayout.CreateFmt(LWS_LAYOUT_PATH_NOT_FOUND_ERR,
      [FPath, ClassName]);
  with TFileStream.Create(FPath + AFileName, fmOpenRead) do
  try
    L := Size;
    SetLength(FContent, L);
    Read(Pointer(FContent)^, L);
  finally
    Free;
  end;
end;

end.

