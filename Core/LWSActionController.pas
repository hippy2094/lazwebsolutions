(*
  LazWebSolutions, Action Controller unit
  Copyright (C) 2012-2014 Silvio Clecio, Luciano Souza.

  https://github.com/silvioprog/lazwebsolutions

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LWSActionController;

{$I lazwebsolutions.inc}
{$DEFINE USELWSCGI}

interface

uses
{$IFDEF USELWSCGI}
  LWSCGI,
{$ELSE}
  LWSClasses, Classes,
{$ENDIF}
  LWSConsts, LWSActionView, FPJSON;

type

  { TLWSActionController }

  TLWSActionController = class
  private
{$IFDEF USELWSCGI}
    FCGI: TLWSCGI;
{$ELSE}
    FContents: TLWSMemoryStream;
    FEnvironmentVariables: TStrings;
    FFields: TJSONObject;
    FHeaders: TLWSMemoryStream;
    FParams: TJSONObject;
{$ENDIF}
    FView: TLWSActionView;
  public
    function GetView: TLWSActionView;
    procedure SetView(const AValue: TLWSActionView);
    class function Name: ShortString; virtual;
    constructor Create; virtual;
    procedure Index; virtual; abstract;
    procedure Delete(AValue: TJSONData); virtual;
    procedure Edit(AValue: TJSONData); virtual;
    procedure Extra(const ARequestMethod: ShortString; const APathInfo: string;
      APathInfos: TJSONArray); virtual;
    procedure Insert; virtual;
    procedure New; virtual;
    procedure Show(AValue: TJSONData); virtual;
    procedure Update(AValue: TJSONData); virtual;
{$IFDEF USELWSCGI}
    procedure RedirectTo(const AActionName: ShortString = ES;
      AControllerName: ShortString = ES);
    property CGI: TLWSCGI read FCGI write FCGI;
{$ELSE}
    property Contents: TLWSMemoryStream read FContents write FContents;
    property EnvironmentVariables: TStrings read FEnvironmentVariables
      write FEnvironmentVariables;
    property Fields: TJSONObject read FFields write FFields;
    property Headers: TLWSMemoryStream read FHeaders write FHeaders;
    property Params: TJSONObject read FParams write FParams;
{$ENDIF}
    property View: TLWSActionView read GetView write SetView;
  end;

  TLWSActionControllerClass = class of TLWSActionController;

implementation

{ TLWSActionController }

constructor TLWSActionController.Create;
begin
end;

{$HINTS OFF}
procedure TLWSActionController.Delete(AValue: TJSONData);
begin
end;

procedure TLWSActionController.Edit(AValue: TJSONData);
begin
end;

procedure TLWSActionController.Extra(const ARequestMethod: ShortString;
  const APathInfo: string; APathInfos: TJSONArray);
begin
end;

procedure TLWSActionController.Show(AValue: TJSONData);
begin
end;

procedure TLWSActionController.Update(AValue: TJSONData);
begin
end;
{$HINTS ON}

{$IFDEF USELWSCGI}
procedure TLWSActionController.RedirectTo(const AActionName: ShortString;
  AControllerName: ShortString);
begin
  if AControllerName = ES then
    AControllerName := Name;
  CGI.Location := View.URLFor(AControllerName, AActionName);
end;
{$ENDIF}

function TLWSActionController.GetView: TLWSActionView;
begin
  Result := FView;
end;

procedure TLWSActionController.SetView(const AValue: TLWSActionView);
begin
  FView := AValue;
end;

class function TLWSActionController.Name: ShortString;
begin
  Result := Copy(LowerCase(ClassName), 2, MaxInt);
end;

procedure TLWSActionController.New;
begin
end;

procedure TLWSActionController.Insert;
begin
end;

end.

