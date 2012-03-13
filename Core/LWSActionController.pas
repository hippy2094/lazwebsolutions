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
  FPJSON;

type

  { TLWSActionController }

  TLWSActionController = class
  public
    class function Name: ShortString; virtual;
    constructor Create; virtual;
    procedure Index; virtual; abstract;
    procedure Delete(AValue: TJSONData); virtual;
    procedure Edit(AValue: TJSONData); virtual;
    procedure Extra(APathInfos: TJSONArray); virtual;
    procedure Insert; virtual;
    procedure New; virtual;
    procedure Show(AValue: TJSONData); virtual;
    procedure Update(AValue: TJSONData); virtual;
  end;

  TLWSActionControllerClass = class of TLWSActionController;

implementation

{ TLWSActionController }

constructor TLWSActionController.Create;
begin
end;

class function TLWSActionController.Name: ShortString;
begin
  Result := Copy(LowerCase(ClassName), 2, MaxInt);
end;

procedure TLWSActionController.Delete(AValue: TJSONData);
begin
end;

procedure TLWSActionController.Edit(AValue: TJSONData);
begin
end;

procedure TLWSActionController.Extra(APathInfos: TJSONArray);
begin
end;

procedure TLWSActionController.Insert;
begin
end;

procedure TLWSActionController.New;
begin
end;

procedure TLWSActionController.Show(AValue: TJSONData);
begin
end;

procedure TLWSActionController.Update(AValue: TJSONData);
begin
end;

end.

