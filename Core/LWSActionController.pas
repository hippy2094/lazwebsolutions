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

interface

uses
{$IFDEF USELWSCGI}
  LWSCGI,
{$ELSE}
  LWSClasses, Classes,
{$ENDIF}
  LWSConsts, LWSActionView, FPJSON;

type
  TLWSActionType = (atUnknow, atIndex, atNew, atInsert, atShow, atEdit, atUpdate,
    atExclude, atDelete, atFind, atLocate);

  { TLWSActionController }

  TLWSActionController = class
  private
    FAllowed: Boolean;
    FUseJSON: Boolean;
{$IFDEF USELWSCGI}
    FCGI: TLWSCGI;
{$ELSE}
    FContents: TLWSMemoryStream;
    FEnvironmentVariables: TStrings;
    FFields: TJSONObject;
    FHeaders: TLWSMemoryStream;
    FParams: TJSONObject;
    FReasonPhrase: ShortString;
    FStatusCode: Word;
{$ENDIF}
    FView: TLWSActionView;
  protected
    function GetView: TLWSActionView;
    procedure SetView(const AValue: TLWSActionView);
  public
    constructor Create; virtual;
    procedure SetHTTPStatusCode(const AStatusCode: Word;
      const AReasonPhrase: ShortString);
    class function Name: ShortString; virtual;
    function Display: Boolean;
    procedure Clear; virtual;
    procedure Index; virtual; abstract;
    procedure Delete(AValue: Int64); virtual;
    procedure Edit(AValue: Int64); virtual;
    procedure Exclude(AValue: Int64); virtual;
    procedure Find; virtual;
    procedure Insert; virtual;
    procedure JSON(AFields: TJSONObject); virtual;
    procedure Locate(AParams: TJSONObject); virtual;
    procedure New; virtual;
    procedure Show(AValue: Int64); virtual;
    procedure Update(AValue: Int64); virtual;
    function Validate(const AActionType: TLWSActionType): Boolean; virtual;
    procedure MethodNotAllowed; virtual;
{$IFDEF USELWSCGI}
    procedure RedirectTo(const AURL: string = ES;
      const AActionName: ShortString = ES; AControllerName: ShortString = ES);
    property CGI: TLWSCGI read FCGI write FCGI;
{$ELSE}
    property Contents: TLWSMemoryStream read FContents write FContents;
    property EnvironmentVariables: TStrings read FEnvironmentVariables
      write FEnvironmentVariables;
    property Fields: TJSONObject read FFields write FFields;
    property Headers: TLWSMemoryStream read FHeaders write FHeaders;
    property Params: TJSONObject read FParams write FParams;
    property ReasonPhrase: ShortString read FReasonPhrase write FReasonPhrase;
    property StatusCode: Word read FStatusCode write FStatusCode;
{$ENDIF}
    property Allowed: Boolean read FAllowed write FAllowed;
    property UseJSON: Boolean read FUseJSON write FUseJSON;
    property View: TLWSActionView read GetView write SetView;
  end;

  TLWSActionControllerClass = class of TLWSActionController;

implementation

{ TLWSActionController }

constructor TLWSActionController.Create;
begin
  FAllowed := False;
  FUseJSON := False;
end;

{$HINTS OFF}
procedure TLWSActionController.Delete(AValue: Int64);
begin
  SetHTTPStatusCode(LWS_HTTP_STATUS_CODE_NO_CONTENT,
    LWS_HTTP_REASON_PHRASE_NO_CONTENT);
end;

procedure TLWSActionController.Edit(AValue: Int64);
begin
end;

procedure TLWSActionController.Exclude(AValue: Int64);
begin
end;

procedure TLWSActionController.Locate(AParams: TJSONObject);
begin
end;

procedure TLWSActionController.Show(AValue: Int64);
begin
end;

procedure TLWSActionController.Update(AValue: Int64);
begin
  SetHTTPStatusCode(LWS_HTTP_STATUS_CODE_NO_CONTENT,
    LWS_HTTP_REASON_PHRASE_NO_CONTENT);
end;

function TLWSActionController.Validate(const AActionType: TLWSActionType
  ): Boolean;
begin
  Result := True;
end;

procedure TLWSActionController.JSON(AFields: TJSONObject);
begin
end;
{$HINTS ON}

procedure TLWSActionController.Find;
begin
end;

procedure TLWSActionController.Insert;
begin
  SetHTTPStatusCode(LWS_HTTP_STATUS_CODE_CREATED,
    LWS_HTTP_REASON_PHRASE_CREATED);
end;

procedure TLWSActionController.New;
begin
end;

procedure TLWSActionController.MethodNotAllowed;
begin
  SetHTTPStatusCode(LWS_HTTP_STATUS_CODE_METHOD_NOT_ALLOWED,
    LWS_HTTP_REASON_PHRASE_METHOD_NOT_ALLOWED);
end;

{$IFDEF USELWSCGI}
procedure TLWSActionController.RedirectTo(const AURL: string;
  const AActionName: ShortString; AControllerName: ShortString);
var
  VURL: string;
begin
  if AControllerName = ES then
    AControllerName := Name;
  if AURL <> ES then
    VURL := AURL
  else
    VURL := View.URLFor(AControllerName, AActionName);
  CGI.Headers.Clear;
  CGI.Headers.Add(LWS_HTTP_HEADER_LOCATION + VURL + CRLF);
end;
{$ENDIF}

procedure TLWSActionController.SetHTTPStatusCode(const AStatusCode: Word;
  const AReasonPhrase: ShortString);
begin
{$IFDEF USELWSCGI}CGI.{$ENDIF}StatusCode := AStatusCode;
{$IFDEF USELWSCGI}CGI.{$ENDIF}ReasonPhrase := AReasonPhrase;
end;

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

function TLWSActionController.Display: Boolean;
begin
  if FView.Content <> ES then
  begin
    Result := True;
    FView.Format;
{$IFDEF USELWSCGI}CGI.{$ENDIF}Contents.Text := FView.Content;
  end
  else
  begin
    Result := FUseJSON;
    if not Result then
      Exit;
    JSON({$IFDEF USELWSCGI}CGI.{$ENDIF}Fields);
{$IFDEF USELWSCGI}CGI.{$ENDIF}Contents.Text := FView.AsJSON;
  end;
end;

procedure TLWSActionController.Clear;
begin
end;

end.

