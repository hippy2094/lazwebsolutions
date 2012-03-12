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
  TLWSActionType = (atInsert, atUpdate, atDelete);

  { TLWSRouter }

  TLWSRouter = class
  private
    FActionEdit: ShortString;
    FActionNew: ShortString;
    FControllers: TList;
    FPathInfos: TJSONArray;
    FSkippedItems: Integer;
  protected
    function FindController(const AName: ShortString): TLWSActionController;
    procedure MissingController(const AName: ShortString); virtual;
    procedure Customize; virtual;
    function Require(const AActionType: TLWSActionType): Boolean; virtual;
    property PathInfos: TJSONArray read FPathInfos;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddController(AController: TLWSActionController);
    procedure AddControllers(AControllers: array of TLWSActionController);
    procedure Route(const ARequestMethod: ShortString; const APathInfo: string);
    property ActionEdit: ShortString read FActionEdit write FActionEdit;
    property ActionNew: ShortString read FActionNew write FActionNew;
    property SkippedItems: Integer read FSkippedItems write FSkippedItems;
  end;

  TLWSRouterClass = class of TLWSRouter;

implementation

{ TLWSRouter }

constructor TLWSRouter.Create;
begin
  FControllers := TList.Create;
  FPathInfos := nil;
  FActionEdit := 'edit';
  FActionNew := 'new';
  FSkippedItems := 0;
end;

destructor TLWSRouter.Destroy;
var
  I: Integer;
  VController: TObject;
begin
  for I := 0 to Pred(FControllers.Count) do
  begin
    VController := TObject(FControllers[I]);
    FreeAndNil(VController);
  end;
  if Assigned(FPathInfos) then
    FPathInfos.Free;
  FControllers.Free;
  inherited Destroy;
end;

procedure TLWSRouter.Customize;
begin
end;

{$HINTS OFF}
procedure TLWSRouter.MissingController(const AName: ShortString);
begin
end;

function TLWSRouter.Require(const AActionType: TLWSActionType): Boolean;
begin
  Result := True;
end;
{$HINTS ON}

function TLWSRouter.FindController(const AName: ShortString): TLWSActionController;
var
  I: Integer;
  VController: TLWSActionController;
begin
  Result := nil;
  for I := 0 to Pred(FControllers.Count) do
  begin
    VController := TLWSActionController(FControllers[I]);
    if CompareText(AName, VController.Name) = 0 then
    begin
      Result := VController;
      Break;
    end;
  end;
end;

procedure TLWSRouter.AddController(AController: TLWSActionController);
begin
  FControllers.Add(AController);
end;

procedure TLWSRouter.AddControllers(AControllers: array of TLWSActionController);
var
  I: Integer;
begin
  for I := Low(AControllers) to High(AControllers) do
    FControllers.Add(AControllers[I]);
end;

procedure TLWSRouter.Route(const ARequestMethod: ShortString;
  const APathInfo: string);
var
  VCount: LongInt;
  VParser: TJSONParser;
  VControllerName: ShortString;
  VController: TLWSActionController;
begin
{$IFDEF DEBUG}
  LWSSendMethodEnter('TLWSRouter.Route');
{$ENDIF}
  if ARequestMethod = LWS_HTTP_REQUEST_METHOD_HEAD then
    Exit;
  VParser := TJSONParser.Create(LWSPathToJSON(APathInfo, '/'));
  try
    FPathInfos := TJSONArray(VParser.Parse);
    VCount := FPathInfos.Count - FSkippedItems;
    if Assigned(FPathInfos) and (VCount > 0) then
    begin
      VControllerName := FPathInfos[FSkippedItems].AsString;
      VController := FindController(VControllerName);
      if Assigned(VController) then
      begin
        if ARequestMethod = LWS_HTTP_REQUEST_METHOD_GET then
        begin
          case VCount of
            1: VController.Index;
            2:
              if FPathInfos[Succ(FSkippedItems)].AsString = FActionNew then
                VController.New
              else
                VController.Show(FPathInfos[Succ(FSkippedItems)]);
            3:
              if FPathInfos[FSkippedItems + 2].AsString = FActionEdit then
                VController.Edit(FPathInfos[Succ(FSkippedItems)]);
          end;
        end
        else
        if ARequestMethod = LWS_HTTP_REQUEST_METHOD_POST then
        begin
          if Require(atInsert) then
            VController.Insert;
        end
        else
        if (ARequestMethod = LWS_HTTP_REQUEST_METHOD_PUT) and (VCount > 1) then
        begin
          if Require(atUpdate) then
            VController.Update(FPathInfos[Succ(FSkippedItems)]);
        end
        else
        if (ARequestMethod = LWS_HTTP_REQUEST_METHOD_DELETE) and
          (VCount > 1) then
        begin
          if Require(atDelete) then
            VController.Delete(FPathInfos[Succ(FSkippedItems)]);
        end;
        VController.Extra(FPathInfos);
      end
      else
        MissingController(VControllerName);
    end
    else
      Customize;
  finally
    VParser.Free;
  end;
{$IFDEF DEBUG}
  LWSSendMethodExit('TLWSRouter.Route');
{$ENDIF}
end;

end.

