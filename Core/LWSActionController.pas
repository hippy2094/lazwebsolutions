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
    FReasonPhrase: ShortString;
    FStatusCode: Word;
{$ENDIF}
    FView: TLWSActionView;
  public
    constructor Create; virtual;
    procedure SetHTTPStatusCode(const AStatusCode: Word;
      const AReasonPhrase: ShortString);
    function GetView: TLWSActionView;
    procedure SetView(const AValue: TLWSActionView);
    class function Name: ShortString; virtual;
    procedure Index; virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure Delete(AValue: Int64); virtual;
    procedure Edit(AValue: Int64); virtual;
    procedure Find; virtual;
    procedure Insert; virtual;
    procedure Locate(AParams: TJSONObject); virtual;
    procedure New; virtual;
    procedure Show(AValue: Int64); virtual;
    procedure Update(AValue: Int64); virtual;
    procedure MethodNotAllowed; virtual;
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
    property ReasonPhrase: ShortString read FReasonPhrase write FReasonPhrase;
    property StatusCode: Word read FStatusCode write FStatusCode;
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
procedure TLWSActionController.Delete(AValue: Int64);
begin
  SetHTTPStatusCode(LWS_HTTP_STATUS_CODE_NO_CONTENT,
    LWS_HTTP_REASON_PHRASE_NO_CONTENT);
end;

procedure TLWSActionController.Edit(AValue: Int64);
begin
  SetHTTPStatusCode(LWS_HTTP_STATUS_CODE_NO_CONTENT,
    LWS_HTTP_REASON_PHRASE_NO_CONTENT);
end;

procedure TLWSActionController.Locate(AParams: TJSONObject);
begin
end;

procedure TLWSActionController.Show(AValue: Int64);
begin
end;

procedure TLWSActionController.Update(AValue: Int64);
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
procedure TLWSActionController.RedirectTo(const AActionName: ShortString;
  AControllerName: ShortString);
begin
  if AControllerName = ES then
    AControllerName := Name;
  CGI.Location := View.URLFor(AControllerName, AActionName);
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

end.

