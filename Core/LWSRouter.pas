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
    FActionJSON: ShortString;
    FActionNew: ShortString;
    FController: TLWSActionController;
    FControllerClasses: TList;
    FFound: Boolean;
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
    property ActionJSON: ShortString read FActionJSON write FActionJSON;
    property ActionNew: ShortString read FActionNew write FActionNew;
    property Found: Boolean read FFound write FFound;
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
  FActionJSON := 'json';
  FActionNew := 'new';
  FFound := True;
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
        if FController.Validate(atUnknow) then
        begin
          Routing(ARequestMethod, APathInfo, FPathInfos, FController, VContinue);
          if VContinue then
          begin
            FController.Clear;
            if VCount > 1 then
              VPathInfoItem := FPathInfos[Succ(FSkippedItems)];
            if ARequestMethod = LWS_HTTP_REQUEST_METHOD_GET then
            begin
              case VCount of
                1:
                  if FController.Validate(atIndex) then
                  begin
                    FController.Allowed := True;
                    FController.Index;
                  end;
                2:
                  begin
                    VActionName := VPathInfoItem.AsString;
                    if VActionName = FActionFind then
                    begin
                      VParams := FController{$IFDEF USELWSCGI}.CGI{$ENDIF}.Params;
                      if Assigned(VParams) and (VParams.Count > 0) then
                      begin
                        if Require(atLocate) and FController.Validate(atLocate) then
                        begin
                          FController.Allowed := True;
                          FController.Locate(VParams);
                        end;
                      end
                      else
                        if FController.Validate(atFind) then
                        begin
                          FController.Allowed := True;
                          FController.Find;
                        end;
                    end
                    else
                    if VActionName = FActionJSON then
                    begin
                      FController.Allowed := True;
                      FController.JSON;
                    end
                    else
                    if VActionName = FActionNew then
                    begin
                      if FController.Validate(atNew) then
                      begin
                        FController.Allowed := True;
                        FController.New;
                      end;
                    end
                    else
                      if FController.Validate(atShow) then
                      begin
                        FController.Allowed := True;
                        FController.Show(VPathInfoItem.AsInt64);
                      end;
                  end;
                3:
                  begin
                    VActionName := FPathInfos[FSkippedItems + 2].AsString;
                    if VActionName = FActionEdit then
                    begin
                      if FController.Validate(atEdit) then
                      begin
                        FController.Allowed := True;
                        FController.Edit(VPathInfoItem.AsInt64);
                      end;
                    end
                    else
                    if (VActionName = FActionExclude) and
                      FController.Validate(atExclude) then
                    begin
                      FController.Allowed := True;
                      FController.Exclude(VPathInfoItem.AsInt64);
                    end;
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
                  (VFields.Items[_methodIndex].AsString = 'delete') then
                begin
                  if Require(atDelete) and FController.Validate(atDelete) then
                  begin
                    FController.Allowed := True;
                    FController.Delete(VPathInfoItem.AsInt64);
                  end;
                end
                else
                if Require(atUpdate) and FController.Validate(atUpdate) then
                begin
                  FController.Allowed := True;
                  FController.Update(VPathInfoItem.AsInt64);
                end;
              end
              else
              if Require(atInsert) and FController.Validate(atInsert) then
              begin
                FController.Allowed := True;
                FController.Insert;
              end;
            end
            else
            if ARequestMethod = LWS_HTTP_REQUEST_METHOD_PUT then
            begin
              if (VCount > 1) and Require(atUpdate) and
                FController.Validate(atUpdate) then
              begin
                FController.Allowed := True;
                FController.Update(VPathInfoItem.AsInt64);
              end;
            end
            else
            if ARequestMethod = LWS_HTTP_REQUEST_METHOD_DELETE then
            begin
              if (VCount > 1) and Require(atDelete) and
                FController.Validate(atDelete) then
              begin
                FController.Allowed := True;
                FController.Delete(VPathInfoItem.AsInt64);
              end;
            end
            else
              FController.MethodNotAllowed;
            if (not FController.Allowed) or (not FFound) and
              Assigned(AOnNotFound) then
              AOnNotFound(APathInfo);
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

