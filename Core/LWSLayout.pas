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
    procedure SetPath(const AValue: string);
  public
    constructor Create(const AElements: array of const;
      const AAutoLoadLayout: Boolean = True); overload;
    procedure LoadLayoutFromFile(const AFileName: TFileName);
    procedure Format; virtual;
    function GetFormatedContent: string;
    property Content: string read FContent;
    property Path: string read FPath write SetPath;
  end;

implementation

{ TLWSLayout }

constructor TLWSLayout.Create(const AElements: array of const;
  const AAutoLoadLayout: Boolean);
begin
  inherited Create(AElements);
  SetPath(ExtractFilePath(ParamStr(0)) + 'views' + DirectorySeparator);
  if AAutoLoadLayout then
    LoadLayoutFromFile(LWS_DEFAULT_LAYOUT_FILENAME);
end;

procedure TLWSLayout.Format;
var
  VName: string;
  I, L: Integer;
  J, P: LongInt;
begin
  for I := 0 to Pred(Count) do
  begin
    for J := 1 to Length(FContent) do
    begin
      VName := '@' + Names[I];
      P := Pos(VName, FContent);
      if P <> 0 then
      begin
        L := Length(VName);
        System.Delete(FContent, P, L);
        Insert(Items[I].AsString, FContent, P);
        Break;
      end;
    end;
  end;
end;

function TLWSLayout.GetFormatedContent: string;
begin
  Format;
  Result := FContent;
end;

procedure TLWSLayout.SetPath(const AValue: string);
begin
  FPath := IncludeTrailingPathDelimiter(AValue);
end;

procedure TLWSLayout.LoadLayoutFromFile(const AFileName: TFileName);
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

