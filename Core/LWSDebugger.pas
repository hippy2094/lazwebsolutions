(*
  LazWebSolutions, Debugger unit
  Copyright (C) 2012-2014 Silvio Clecio, Luciano Souza.

  https://github.com/silvioprog/lazwebsolutions

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LWSDebugger;

{$I lazwebsolutions.inc}

interface

uses
  LWSConsts, SysUtils, Classes;

var
  LWS_DEBUG_FILENAME: TFileName = '';
  LWS_DEBUGHEAP_FILENAME: TFileName = '';

{ Send msg to debug file. }
procedure LWSSendMsg(const AMsg: string);
{ Send stream to debug file. }
procedure LWSSendStream(AStream: TStream);
{ Send method enter to debug file. }
procedure LWSSendMethodEnter(const AMethodName: string;
  const ALineBreak: ShortString = '');
{ Send method exit to debug file. }
procedure LWSSendMethodExit(const AMethodName: string;
  const ALineBreak: ShortString = '');
{ Send begin msg to file. }
procedure LWSSendBegin(const AProjectName, AWelcomeMsg: string;
  const ALineBreak: ShortString = '');
{ Send end msg to file. }
procedure LWSSendEnd(const AProjectName, AGoodByeMsg: string;
  const ALineBreak: ShortString = '');

implementation

var
  _DebugFile: TFileStream;

function DebugFile: TFileStream;
begin
  if not Assigned(_DebugFile) then
    _DebugFile := TFileStream.Create(LWS_DEBUG_FILENAME, fmCreate);
  Result := _DebugFile;
end;

procedure LWSSendMsg(const AMsg: string);
begin
  DebugFile.Write(Pointer(AMsg)^, Length(AMsg));
end;

procedure LWSSendStream(AStream: TStream);
begin
  AStream.Position := 0;
  DebugFile.CopyFrom(AStream, AStream.Size);
end;

procedure LWSSendMethodEnter(const AMethodName: string;
  const ALineBreak: ShortString);
begin
  LWSSendMsg(ALineBreak + '>> ' + TimeToStr(Now) + ' - Entering -> ' +
    AMethodName + ' ...' + LF);
end;

procedure LWSSendMethodExit(const AMethodName: string;
  const ALineBreak: ShortString);
begin
  LWSSendMsg(ALineBreak + '>> ' + TimeToStr(Now) + ' - Exiting <- ' +
    AMethodName + ' done!' + LF);
end;

procedure LWSSendBegin(const AProjectName, AWelcomeMsg: string;
  const ALineBreak: ShortString);
begin
  LWSSendMsg(ALineBreak + '>> ' + TimeToStr(Now) + ' - Begin -> ' +
    AProjectName + ' - ' + AWelcomeMsg + LF);
end;

procedure LWSSendEnd(const AProjectName, AGoodByeMsg: string;
  const ALineBreak: ShortString);
begin
  LWSSendMsg(ALineBreak + '>> ' + TimeToStr(Now) + ' - End <- ' + AProjectName +
    ' - ' + AGoodByeMsg + LF + LF);
end;

initialization
  if LWS_DEBUG_FILENAME = '' then
    LWS_DEBUG_FILENAME := GetTempDir + 'DEBUG.LOG';
{$IFDEF DEBUGHEAP}
  if LWS_DEBUGHEAP_FILENAME = '' then
  begin
    LWS_DEBUGHEAP_FILENAME := GetTempDir + 'DEBUGHEAP.LOG';
    DeleteFile(LWS_DEBUGHEAP_FILENAME);
  end;
{$ENDIF}

finalization
  if Assigned(_DebugFile) then
    _DebugFile.Free;

end.

