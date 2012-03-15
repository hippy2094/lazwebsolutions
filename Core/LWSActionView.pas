(*
  LazWebSolutions, Action View unit
  Copyright (C) 2012-2014 Silvio Clecio, Luciano Souza.

  https://github.com/silvioprog/lazwebsolutions

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LWSActionView;

{$I lazwebsolutions.inc}

interface

uses
{$IFDEF DEBUG}
  LWSDebugger,
{$ENDIF}
  LWSConsts, LWSUtils, SysUtils, FPJSON;

type
  ELWSActionView = class(Exception);

  { TLWSActionView }

  TLWSActionView = class(TJSONObject)
  private
    FContent: string;
    FDomain: string;
    FPath: string;
    FRecursive: Boolean;
    FTagPrefix: ShortString;
    function GetContent: string;
    procedure SetContent(const AValue: string);
    procedure SetPath(const AValue: string);
  public
    constructor Create(const AElements: array of const;
      const AViewPath: string = ES; const AViewFile: string = ES;
      const AAutoLoaded: Boolean = True); overload;
    function LinkTo(const AControllerName, AActionName, ACaption: string): string;
    function URLFor(const AControllerName, AActionName: string): string;
    function FileToString(const AFileName: TFileName;
      const AUseViewPath: Boolean = True): string;
    function HTMLToString(const AFileName: TFileName;
      const AUseViewPath: Boolean = True): string;
    procedure Format;
    procedure LoadFromFile(const AFileName: TFileName);
    property Content: string read GetContent write SetContent;
    property Domain: string read FDomain write FDomain;
    property Path: string read FPath write SetPath;
    property Recursive: Boolean read FRecursive write FRecursive;
    property TagPrefix: ShortString read FTagPrefix write FTagPrefix;
  end;

  TLWSActionViewClass = class of TLWSActionView;

implementation

{ TLWSActionView }

constructor TLWSActionView.Create(const AElements: array of const;
  const AViewPath: string; const AViewFile: string;
  const AAutoLoaded: Boolean);
begin
  inherited Create(AElements);
  FTagPrefix := LWS_DEFAULT_TAG_PREFIX;
  if AViewPath = ES then
    SetPath(ExtractFilePath(ParamStr(0)) + 'view' + DirectorySeparator)
  else
    SetPath(AViewPath);
  if AAutoLoaded then
  begin
    if AViewFile = ES then
      LoadFromFile(LWS_DEFAULT_VIEW_FILENAME)
    else
      LoadFromFile(AViewFile);
  end;
end;

function TLWSActionView.LinkTo(const AControllerName, AActionName,
  ACaption: string): string;
begin
  Result := '<a href="' + URLFor(AControllerName, AActionName) + '">' +
    ACaption + '</a>';
end;

function TLWSActionView.URLFor(const AControllerName, AActionName: string
  ): string;
begin
  Result := FDomain + AControllerName + '/' + AActionName;
end;

function TLWSActionView.FileToString(const AFileName: TFileName;
  const AUseViewPath: Boolean): string;
begin
  if AUseViewPath then
    Result := LWSFileToString(FPath + AFileName)
  else
    Result := LWSFileToString(AFileName);
end;

function TLWSActionView.HTMLToString(const AFileName: TFileName;
  const AUseViewPath: Boolean): string;
begin
  Result := FileToString(AFileName + '.lws.html', AUseViewPath);
end;

procedure TLWSActionView.Format;
var
  I, L: Integer;
  J, P: LongInt;
  VName, VValue: string;
begin
{$IFDEF DEBUG}
  LWSSendMethodEnter('TLWSActionView.Format');
{$ENDIF}
  for I := 0 to Pred(Count) do
  begin
    VName := FTagPrefix + Names[I];
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
{$IFDEF DEBUG}
  LWSSendMethodExit('TLWSActionView.Format');
{$ENDIF}
end;

procedure TLWSActionView.SetPath(const AValue: string);
begin
  FPath := IncludeTrailingPathDelimiter(AValue);
end;

function TLWSActionView.GetContent: string;
begin
  Format;
  Result := FContent;
end;

procedure TLWSActionView.SetContent(const AValue: string);
begin
  FContent := AValue;
end;

procedure TLWSActionView.LoadFromFile(const AFileName: TFileName);
begin
  if not DirectoryExists(FPath) then
    raise ELWSActionView.CreateFmt(LWS_VIEW_PATH_NOT_FOUND_ERR,
      [FPath, ClassName]);
  FContent := LWSFileToString(FPath + AFileName);
end;

end.

