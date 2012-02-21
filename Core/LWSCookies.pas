(*
  LazWebSolutions, Cookies unit
  Copyright (C) 2012-2014 Silvio Clecio, Luciano Souza.

  http://code.google.com/p/lazwebsolutions

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LWSCookies;

{$I lazwebsolutions.inc}

interface

uses
{$IFDEF DEBUG}
  LWSDebugger,
{$ENDIF}
  LWSConsts, LWSUtils, FPJSON, JSONParser;

type

  { TLWSCookies }

  TLWSCookies = class(TJSONObject)
  private
    FHeader: string;
    FHTTPCookie: string;
    FResent: Boolean;
    function GetHeader: string;
  public
    constructor Create(const AHTTPCookie: string;
      const AParse: Boolean = True); reintroduce;
    procedure Parse; virtual;
    procedure Clear(const APath: string = '';
      const ADomain: string = ''); overload;
    procedure SetRawCookie(const AName, AValue: string;
      const AExpires: TDateTime = NullDate; const APath: string = '';
      const ADomain: string = ''; const ASecure: Boolean = False;
      const AHTTPOnly: Boolean = False);
    procedure SetCookie(const AName, AValue: string;
      const AExpires: TDateTime = NullDate; const APath: string = '';
      const ADomain: string = ''; const ASecure: Boolean = False;
      const AHTTPOnly: Boolean = False);
    function GetRawCookie(const AName: string): string;
    function GetCookie(const AName: string): string;
    procedure DeleteCookie(const AName: string; const APath: string = '';
      const ADomain: string = '');
    property HTTPCookie: string read FHTTPCookie;
    property Header: string read GetHeader;
    property Resent: Boolean read FResent write FResent;
  end;

implementation

{ TLWSCookies }

constructor TLWSCookies.Create(const AHTTPCookie: string; const AParse: Boolean);
begin
  inherited Create;
  FHTTPCookie := AHTTPCookie;
  FResent := False;
  if AParse then
    Parse;
end;

procedure TLWSCookies.Parse;
var
  I: Integer;
  VJSONData: TJSONData;
  VJSONParser: TJSONParser;
begin
{$IFDEF DEBUG}
  LWSSendMethodEnter('TLWSCookies.Parse');
{$ENDIF}
  if FHTTPCookie <> ES then
  begin
    VJSONData := nil;
    VJSONParser := TJSONParser.Create(
      LWSParamStringToJSON(LWSTrimCookies(FHTTPCookie), '=', ';'));
    try
      VJSONData := VJSONParser.Parse;
      Clear;
      if Assigned(VJSONData) then
        for I := 0 to Pred(VJSONData.Count) do
          Add(TJSONObject(VJSONData).Names[I], VJSONData.Items[I].Clone);
    finally
      VJSONData.Free;
      VJSONParser.Free;
    end;
  end;
{$IFDEF DEBUG}
  LWSSendMethodExit('TLWSCookies.Parse');
{$ENDIF}
end;

function TLWSCookies.GetHeader: string;
var
  I: Integer;
begin
  if FResent then
    for I := 0 to Pred(Count) do
      LWSSetRawCookie(FHeader, Names[I], LWSURIEncode(Items[I].AsString));
  Result := FHeader;
end;

procedure TLWSCookies.Clear(const APath: string; const ADomain: string);
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
    LWSDeleteCookie(FHeader, Names[I], APath, ADomain);
  inherited Clear;
end;

procedure TLWSCookies.SetRawCookie(const AName, AValue: string;
  const AExpires: TDateTime; const APath: string; const ADomain: string;
  const ASecure: Boolean; const AHTTPOnly: Boolean);
begin
  LWSSetRawCookie(FHeader, AName, AValue, AExpires, APath, ADomain, ASecure,
    AHTTPOnly);
end;

procedure TLWSCookies.SetCookie(const AName, AValue: string;
  const AExpires: TDateTime; const APath: string; const ADomain: string;
  const ASecure: Boolean; const AHTTPOnly: Boolean);
begin
  LWSSetRawCookie(FHeader, AName, AValue, AExpires, APath, ADomain, ASecure,
    AHTTPOnly);
end;

function TLWSCookies.GetRawCookie(const AName: string): string;
begin
  Result := LWSGetRawCookie(FHTTPCookie, AName);
end;

function TLWSCookies.GetCookie(const AName: string): string;
begin
  Result := LWSURIDecode(LWSGetRawCookie(FHTTPCookie, AName));
end;

procedure TLWSCookies.DeleteCookie(const AName: string; const APath: string;
  const ADomain: string);
begin
  LWSDeleteCookie(FHeader, AName, APath, ADomain);
end;

end.

