(*
  LazWebSolutions, Router unit
  Copyright (C) 2012-2014 Silvio Clecio, Luciano Souza.

  https://github.com/silvioprog/lazwebsolutions

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LWSRouter;

{$I lazwebsolutions.inc}

interface

uses
{$IFDEF DEBUG}
  LWSDebugger,
{$ENDIF}
  LWSActionController, LWSConsts, LWSUtils, SysUtils, Classes, FPJSON,
  JSONParser;

type
  TLWSActionType = (atInsert, atUpdate, atDelete, atLocate);

  TLWSRouterCreateControllerEvent = procedure(
    AController: TLWSActionController) of object;

  TLWSRouterNotFoundEvent = procedure(const APathInfo: string) of object;
  { TLWSRouter }

  TLWSRouter = class
  private
    FActionEdit: ShortString;
    FActionFind: ShortString;
    FActionNew: ShortString;
    FController: TLWSActionController;
    FControllerClasses: TList;
    FPathInfos: TJSONArray;
    FSkippedItems: Integer;
  protected
    function FindControllerClass(
      const AName: ShortString): TLWSActionControllerClass;
    function Require(const AActionType: TLWSActionType): Boolean; virtual;
    procedure Routing(const ARequestMethod: ShortString;
      const APathInfo: string; APathInfos: TJSONArray;
      AController: TLWSActionController; var AContinue: Boolean); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddControllerClass(AControllerClass: TLWSActionControllerClass);
    procedure AddControllerClasses(
      AControllerClasses: array of TLWSActionControllerClass);
    function Route(const ARequestMethod: ShortString; const APathInfo: string;
      AOnCreateController: TLWSRouterCreateControllerEvent;
      AOnNotFound: TLWSRouterNotFoundEvent): Boolean;
    property ActionEdit: ShortString read FActionEdit write FActionEdit;
    property ActionFind: ShortString read FActionFind write FActionFind;
    property ActionNew: ShortString read FActionNew write FActionNew;
    property SkippedItems: Integer read FSkippedItems write FSkippedItems;
  end;

  TLWSRouterClass = class of TLWSRouter;

implementation

{ TLWSRouter }

constructor TLWSRouter.Create;
begin
  FControllerClasses := TList.Create;
  FPathInfos := nil;
  FActionEdit := 'edit';
  FActionFind := 'find';
  FActionNew := 'new';
  FSkippedItems := 0;
end;

destructor TLWSRouter.Destroy;
begin
  if Assigned(FPathInfos) then
    FPathInfos.Free;
  if Assigned(FController) then
    FController.Free;
  FControllerClasses.Free;
  inherited Destroy;
end;

{$HINTS OFF}
function TLWSRouter.Require(const AActionType: TLWSActionType): Boolean;
begin
  Result := True;
end;

procedure TLWSRouter.Routing(const ARequestMethod: ShortString;
  const APathInfo: string; APathInfos: TJSONArray;
  AController: TLWSActionController; var AContinue: Boolean);
begin
end;
{$HINTS ON}

function TLWSRouter.FindControllerClass(const AName: ShortString
  ): TLWSActionControllerClass;
var
  I: Integer;
  VClass: TLWSActionControllerClass;
begin
  Result := nil;
  for I := 0 to Pred(FControllerClasses.Count) do
  begin
    VClass := TLWSActionControllerClass(FControllerClasses[I]);
    if CompareText(AName, VClass.Name) = 0 then
    begin
      Result := VClass;
      Break;
    end;
  end;
end;

procedure TLWSRouter.AddControllerClass(
  AControllerClass: TLWSActionControllerClass);
begin
  FControllerClasses.Add(AControllerClass);
end;

procedure TLWSRouter.AddControllerClasses(
  AControllerClasses: array of TLWSActionControllerClass);
var
  I: Integer;
begin
  for I := 0 to High(AControllerClasses) do
    FControllerClasses.Add(AControllerClasses[I]);
end;

function TLWSRouter.Route(
  const ARequestMethod: ShortString; const APathInfo: string;
  AOnCreateController: TLWSRouterCreateControllerEvent;
  AOnNotFound: TLWSRouterNotFoundEvent): Boolean;
var
  VCount: LongInt;
  VParser: TJSONParser;
  VParams: TJSONObject;
  VPathInfoItem: TJSONData;
  VContinue: Boolean = True;
  VControllerName: ShortString;
  VControllerClass: TLWSActionControllerClass;
begin
{$IFDEF DEBUG}
  LWSSendMethodEnter('TLWSRouter.Route');
{$ENDIF}
  if ARequestMethod = LWS_HTTP_REQUEST_METHOD_HEAD then
    Exit;
  VParser := TJSONParser.Create(LWSPathToJSON(APathInfo, US));
  try
    FPathInfos := TJSONArray(VParser.Parse);
    VCount := FPathInfos.Count - FSkippedItems;
    Result := Assigned(FPathInfos) and (VCount > 0);
    if Result then
    begin
      VControllerName := FPathInfos[FSkippedItems].AsString;
      VControllerClass := FindControllerClass(VControllerName);
      if Assigned(VControllerClass) then
      begin
        FController := VControllerClass.Create;
        if Assigned(AOnCreateController) then
          AOnCreateController(FController);
        Routing(ARequestMethod, APathInfo, FPathInfos, FController, VContinue);
        if VContinue then
        begin
          FController.Clear;
          if VCount > 1 then
            VPathInfoItem := FPathInfos[Succ(FSkippedItems)];
          if ARequestMethod = LWS_HTTP_REQUEST_METHOD_GET then
          begin
            case VCount of
              1: FController.Index;
              2:
                if VPathInfoItem.AsString = FActionFind then
                begin
                  VParams := FController{$IFDEF USELWSCGI}.CGI{$ENDIF}.Params;
                  if Assigned(VParams) and (VParams.Count > 0) then
                  begin
                    if Require(atLocate) then
                      FController.Locate(VParams);
                  end
                  else
                    FController.Find;
                end
                else
                if VPathInfoItem.AsString = FActionNew then
                  FController.New
                else
                  FController.Show(VPathInfoItem.AsInt64);
              3:
                if FPathInfos[FSkippedItems + 2].AsString = FActionEdit then
                  FController.Edit(VPathInfoItem.AsInt64);
            end;
          end
          else
          if ARequestMethod = LWS_HTTP_REQUEST_METHOD_POST then
          begin
            if Require(atInsert) then
              FController.Insert;
          end
          else
          if (ARequestMethod = LWS_HTTP_REQUEST_METHOD_PUT) and (VCount > 1) then
          begin
            if Require(atUpdate) then
              FController.Update(VPathInfoItem.AsInt64);
          end
          else
          if (ARequestMethod = LWS_HTTP_REQUEST_METHOD_DELETE) and
            (VCount > 1) then
          begin
            if Require(atDelete) then
              FController.Delete(VPathInfoItem.AsInt64);
          end;
        end;
      end
      else
        if Assigned(AOnNotFound) then
          AOnNotFound(APathInfo);
    end;
  finally
    VParser.Free;
  end;
{$IFDEF DEBUG}
  LWSSendMethodExit('TLWSRouter.Route');
{$ENDIF}
end;

end.

