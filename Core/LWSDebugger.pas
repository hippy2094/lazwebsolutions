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

type
  ELWSDebugger = class(Exception);

var
  LWS_DEBUG_DIR: string = ES;
  LWS_DEBUG_FILENAME: TFileName = ES;
  LWS_DEBUGHEAP_FILENAME: TFileName = ES;

{ Init debugger. }
procedure LWSInitDebugger(const ADebugDir: string = ES);
{ Send msg to debug file. }
procedure LWSSendMsg(const AMsg: string);
{ Send stream to debug file. }
procedure LWSSendStream(AStream: TStream);
{ Send method enter to debug file. }
procedure LWSSendMethodEnter(const AMethodName: string;
  const ALineBreak: ShortString = ES);
{ Send method exit to debug file. }
procedure LWSSendMethodExit(const AMethodName: string;
  const ALineBreak: ShortString = ES);
{ Send begin msg to file. }
procedure LWSSendBegin(const AProjectName, AWelcomeMsg: string;
  const ALineBreak: ShortString = ES);
{ Send end msg to file. }
procedure LWSSendEnd(const AProjectName, AGoodByeMsg: string;
  const ALineBreak: ShortString = ES);

implementation

var
  _DebugFile: TFileStream;

function DebugFile: TFileStream;
begin
  if not Assigned(_DebugFile) then
    _DebugFile := TFileStream.Create(LWS_DEBUG_FILENAME, fmCreate);
  Result := _DebugFile;
end;

procedure LWSInitDebugger(const ADebugDir: string);
begin
  LWS_DEBUG_DIR := ADebugDir;
  if LWS_DEBUG_DIR = ES then
    LWS_DEBUG_DIR := GetTempDir;
  LWS_DEBUG_DIR := IncludeTrailingPathDelimiter(LWS_DEBUG_DIR);
  if not DirectoryExists(LWS_DEBUG_DIR) then
    WriteLn(LWS_HTTP_HEADER_CONTENT_TYPE +
      LWS_HTTP_CONTENT_TYPE_TEXT_PLAIN + CRLF + CRLF +
      Format(LWS_DEBUG_SAVE_PATH_NOT_FOUND_ERR,
      [LWS_DEBUG_DIR, 'LWSDebugger unit']));
  if LWS_DEBUG_FILENAME = ES then
    LWS_DEBUG_FILENAME := LWS_DEBUG_DIR + 'DEBUG.LOG';
{$IFDEF DEBUGHEAP}
  if LWS_DEBUGHEAP_FILENAME = ES then
  begin
    LWS_DEBUGHEAP_FILENAME := LWS_DEBUG_DIR + 'DEBUGHEAP.LOG';
    DeleteFile(LWS_DEBUGHEAP_FILENAME);
  end;
{$ENDIF}
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

finalization
  if Assigned(_DebugFile) then
    _DebugFile.Free;

end.

