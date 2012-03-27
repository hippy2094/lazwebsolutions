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
  TLWSRouterCreateControllerEvent = procedure(
    AController: TLWSActionController) of object;

  TLWSRouterNotFoundEvent = procedure(const APathInfo: string) of object;
  { TLWSRouter }

  TLWSRouter = class
  private
    FActionEdit: ShortString;
    FActionExclude: ShortString;
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
    property ActionExclude: ShortString read FActionExclude write FActionExclude;
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
  FActionExclude := 'exclude';
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
  VFields: TJSONObject;
  VParams: TJSONObject;
  _methodIndex: Integer;
  VPathInfoItem: TJSONData;
  VContinue: Boolean = True;
  VActionName, VControllerName: ShortString;
  VControllerClass: TLWSActionControllerClass;
begin
{$IFDEF DEBUG}
  LWSSendMethodEnter('TLWSRouter.Route');
{$ENDIF}
  if ARequestMethod = LWS_HTTP_REQUEST_METHOD_HEAD then
  begin
    Result := True;
{$IFDEF DEBUG}
    LWSSendMethodExit('TLWSRouter.Route');
{$ENDIF}
    Exit;
  end;
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
                begin
                  VActionName := VPathInfoItem.AsString;
                  if VActionName = FActionFind then
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
                  if VActionName = FActionNew then
                    FController.New
                  else
                    FController.Show(VPathInfoItem.AsInt64);
                end;
              3:
                begin
                  VActionName := FPathInfos[FSkippedItems + 2].AsString;
                  if VActionName = FActionEdit then
                    FController.Edit(VPathInfoItem.AsInt64)
                  else
                  if VActionName = FActionExclude then
                    FController.Exclude(VPathInfoItem.AsInt64);
                end;
            end;
          end
          else
          if ARequestMethod = LWS_HTTP_REQUEST_METHOD_POST then
          begin
            VFields := FController{$IFDEF USELWSCGI}.CGI{$ENDIF}.Fields;
            if VCount = 2 then
            begin
              _methodIndex := VFields.IndexOfName('_method');
              if Assigned(VFields) and (_methodIndex <> -1) and
                (VFields.Items[_methodIndex].AsString = 'delete') and
                Require(atDelete) then
                FController.Delete(VPathInfoItem.AsInt64)
              else
              if Require(atUpdate) then
                FController.Update(VPathInfoItem.AsInt64);
            end
            else
            if Require(atInsert) then
              FController.Insert;
          end
          else
          if ARequestMethod = LWS_HTTP_REQUEST_METHOD_PUT then
          begin
            if (VCount > 1) and Require(atUpdate) then
              FController.Update(VPathInfoItem.AsInt64);
          end
          else
          if ARequestMethod = LWS_HTTP_REQUEST_METHOD_DELETE then
          begin
            if (VCount > 1) and Require(atDelete) then
              FController.Delete(VPathInfoItem.AsInt64);
          end
          else
            FController.MethodNotAllowed;
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

