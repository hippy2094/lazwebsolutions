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
  LWSActionView, FPJSON;

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
    class function Name: ShortString; virtual;
    constructor Create; virtual;
    { TODO: ButtonTo }
    { TODO: MailTo }
    function LinkTo(
      const AController, AAction, ACaption: string): string;
    function LinkTo(const AAction, ACaption: string): string;
    function URLFor(const AController, AAction: string): string;
    function URLFor(const AAction: string): string;
    procedure Index; virtual; abstract;
    procedure Delete(AValue: TJSONData); virtual;
    procedure Edit(AValue: TJSONData); virtual;
    procedure Extra(APathInfos: TJSONArray); virtual;
    procedure Insert; virtual;
    procedure New; virtual;
    procedure Show(AValue: TJSONData); virtual;
    procedure Update(AValue: TJSONData); virtual;
    property View: TLWSActionView read FView write FView;
{$IFDEF USELWSCGI}
    property CGI: TLWSCGI read FCGI write FCGI;
{$ELSE}
    property Contents: TLWSMemoryStream read FContents write FContents;
    property EnvironmentVariables: TStrings read FEnvironmentVariables
      write FEnvironmentVariables;
    property Fields: TJSONObject read FFields write FFields;
    property Headers: TLWSMemoryStream read FHeaders write FHeaders;
    property Params: TJSONObject read FParams write FParams;
{$ENDIF}
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

procedure TLWSActionController.Extra(APathInfos: TJSONArray);
begin
end;

procedure TLWSActionController.Show(AValue: TJSONData);
begin
end;

procedure TLWSActionController.Update(AValue: TJSONData);
begin
end;
{$HINTS ON}

function TLWSActionController.LinkTo(const AController, AAction,
  ACaption: string): string;
begin
  Result := '<a href="' + URLFor(AController, AAction) + '">' +
    ACaption + '</a>';
end;

function TLWSActionController.LinkTo(const AAction, ACaption: string): string;
begin
  Result := LinkTo(Name, AAction, ACaption);
end;

function TLWSActionController.URLFor(const AController, AAction: string
  ): string;
begin
  Result := CGI.Domain + AController + '/' + AAction;
end;

function TLWSActionController.URLFor(const AAction: string): string;
begin
  Result := URLFor(Name, AAction);
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

