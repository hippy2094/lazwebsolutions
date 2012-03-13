(*
  LazWebSolutions, Memory stream unit
  Copyright (C) 2012-2014 Silvio Clecio, Luciano Souza.

  https://github.com/silvioprog/lazwebsolutions

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LWSClasses;

{$I lazwebsolutions.inc}

interface

uses
  Classes;

type

  { TLWSMemoryStream }

  TLWSMemoryStream = class(TMemoryStream)
  private
    FLineBreakString: ShortString;
    function GetText: string;
    procedure SetText(const AValue: string);
  public
    constructor Create;
    procedure Add(const AString: string);
    procedure Put(const AString: string);
    property LineBreakString: ShortString read FLineBreakString
      write FLineBreakString;
    property Text: string read GetText write SetText;
  end;

  TLWSMemoryStreamClass = class of TLWSMemoryStream;

implementation

{ TLWSMemoryStream }

function TLWSMemoryStream.GetText: string;
begin
  SetLength(Result, Size);
  Position := 0;
  Read(Pointer(Result)^, Size);
end;

procedure TLWSMemoryStream.SetText(const AValue: string);
begin
  Clear;
  Write(Pointer(AValue)^, Length(AValue));
end;

constructor TLWSMemoryStream.Create;
begin
  FLineBreakString := LineEnding;
end;

procedure TLWSMemoryStream.Add(const AString: string);
var
  S: string;
begin
  S := AString + FLineBreakString;
  Position := Size;
  Write(Pointer(S)^, Length(S));
end;

procedure TLWSMemoryStream.Put(const AString: string);
begin
  Write(Pointer(AString)^, Length(AString));
end;

end.

