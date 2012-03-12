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
{$IFDEF DEBUGHEAP}
  HeapTrc,
{$ENDIF}
  LWSConsts, SysUtils, Classes;

{ Init debugger. }
procedure LWSInitDebugger(
  const ADebugFileName{$IFDEF DEBUGHEAP}, ADebugHeapFileName{$ENDIF}: TFileName);
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
  _DebugFileName: TFileName;

function DebugFile: TFileStream;
begin
  try
    if not Assigned(_DebugFile) then
      _DebugFile := TFileStream.Create(_DebugFileName, fmCreate);
    Result := _DebugFile;
  except
    on E: Exception do
      WriteLn(LWS_HTTP_HEADER_CONTENT_TYPE +
        LWS_HTTP_CONTENT_TYPE_TEXT_PLAIN + CRLF + CRLF +
        Format('DEBUG ERROR: %s', [E.Message]));
  end;
end;

procedure LWSInitDebugger(
  const ADebugFileName{$IFDEF DEBUGHEAP}, ADebugHeapFileName{$ENDIF}: TFileName);
begin
{$IFDEF DEBUGHEAP}
  DeleteFile(ADebugHeapFileName);
  SetHeapTraceOutput(ADebugHeapFileName);
{$ENDIF}
  _DebugFileName := ADebugFileName;
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

finalization
  if Assigned(_DebugFile) then
    _DebugFile.Free;

end.

