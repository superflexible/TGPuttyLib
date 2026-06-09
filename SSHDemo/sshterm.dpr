program sshterm;

{
  Super-minimal SSH "terminal" test program for TGPuttyLib's raw SSH
  support (tgputtyssh.pas / TTGPuttySSH).

  It opens an SSH connection, starts a remote shell (no PTY), then loops:
  read one line from the local console with ReadLn, send it to the shell,
  and print whatever the shell writes back until it goes quiet.

  Deliberately dumb: no escape-sequence handling, no raw single-key input,
  no prompt synchronisation. The remote shell has no PTY, so you won't see
  its own prompt - just type a command and press Enter.

  Build (Windows, Delphi):   dcc32 sshterm.dpr     (or dcc64)
  Build (Linux/macOS, FPC):  fpc sshterm.dpr
  Needs tgputtylib.dll / libtgputty.so next to the executable.

  Usage:  sshterm [-persist] <host> <user> [port]
          (password is read from the console)

  Modes:
    (default)   Milestone 1: one SSH connection running a remote shell.
                Lines you type are fed to that shell's stdin; state (cwd,
                env, etc.) persists across commands.

    -persist    Milestone 2: a persistent SSH connection where EACH line
                you type is run as its own command in a fresh channel that
                is opened and then closed, while the SSH connection itself
                stays up. Demonstrates OpenChannel/CloseChannel. Because
                every command gets a brand-new channel, shell state does
                NOT carry over (e.g. "cd /tmp" then "pwd" won't show /tmp).
}

{$APPTYPE CONSOLE}
{$ifdef FPC}{$MODE Delphi}{$endif}

uses
  SysUtils,
  tgputtylib,
  tgputtyssh;

type
  { Holds the event handlers. The library calls these back during connect
    (host-key check) and for diagnostic messages. }
  TTermHandlers = class
    function VerifyHostKey(const host: PAnsiChar; const port: Integer;
                           const fingerprint: PAnsiChar;
                           const verificationstatus: Integer;
                           var storehostkey: Boolean): Boolean;
    procedure LibMessage(const Msg: AnsiString; const isstderr: Boolean);
  end;

function TTermHandlers.VerifyHostKey(const host: PAnsiChar; const port: Integer;
                                     const fingerprint: PAnsiChar;
                                     const verificationstatus: Integer;
                                     var storehostkey: Boolean): Boolean;
begin
  WriteLn;
  WriteLn('Host key for ', string(AnsiString(host)), ':', port);
  WriteLn('  fingerprint: ', string(AnsiString(fingerprint)));
  WriteLn('  (auto-accepting for this test - do not do this in production)');
  storehostkey := False;   { don't persist anything }
  Result := True;          { accept the key and continue }
end;

procedure TTermHandlers.LibMessage(const Msg: AnsiString; const isstderr: Boolean);
begin
  { Library progress / diagnostic text (connection status, warnings). }
  Write(string(Msg));
end;

var
  SSH      : TTGPuttySSH;
  Handlers : TTermHandlers;

{ Print one received chunk that has already been read into the buffers.
  Returns False if it was an EOF marker (0 stdout AND 0 stderr). }
function PrintChunk(const OutBuf; OutLen: Integer;
                    const ErrBuf; ErrLen: Integer): Boolean;
var
  S: AnsiString;
begin
  if (OutLen = 0) and (ErrLen = 0) then
  begin
    Result := False;
    Exit;
  end;
  if OutLen > 0 then
  begin
    SetString(S, PAnsiChar(@OutBuf), OutLen);
    Write(string(S));
  end;
  if ErrLen > 0 then
  begin
    SetString(S, PAnsiChar(@ErrBuf), ErrLen);
    Write(string(S));
  end;
  Result := True;
end;

{ M1: print whatever the shell has sent, waiting up to QuietMS for the
  first/next chunk. Returns when the channel is quiet for QuietMS or the
  shell exits. The shell stays open afterwards. }
procedure DrainShell(const QuietMS: Integer);
var
  OutBuf : array[0..65535] of AnsiChar;
  ErrBuf : array[0..65535] of AnsiChar;
  OutLen : Integer;
  ErrLen : Integer;
begin
  while SSH.IsConnected and SSH.CanReceive(QuietMS) do
  begin
    OutLen := SizeOf(OutBuf);
    ErrLen := SizeOf(ErrBuf);
    SSH.Receive(@OutBuf[0], OutLen, @ErrBuf[0], ErrLen);
    if not PrintChunk(OutBuf, OutLen, ErrBuf, ErrLen) then
      Break;   { EOF: shell exited }
  end;
end;

{ M2: drain one channel's output until the command finishes (channel EOF).
  Unlike DrainShell this keeps waiting through quiet pauses, because we want
  the command to run to completion before we close the channel. PollMS only
  affects how often we re-check; it does not end the drain. }
procedure DrainChannelUntilDone(const Ch: TTGPuttySSHChannel; const PollMS: Integer);
var
  OutBuf : array[0..65535] of AnsiChar;
  ErrBuf : array[0..65535] of AnsiChar;
  OutLen : Integer;
  ErrLen : Integer;
begin
  while True do
  begin
    if not Ch.CanReceive(PollMS) then
    begin
      { Quiet window. If the channel is still open the command is just
        thinking (e.g. "sleep 5") - keep waiting. If it closed, we're done. }
      if not Ch.IsOpen then
        Break;
      Continue;
    end;
    OutLen := SizeOf(OutBuf);
    ErrLen := SizeOf(ErrBuf);
    Ch.Receive(@OutBuf[0], OutLen, @ErrBuf[0], ErrLen);
    if not PrintChunk(OutBuf, OutLen, ErrBuf, ErrLen) then
      Break;   { EOF: command finished and output drained }
  end;
end;

procedure ReadPassword(out APassword: AnsiString);
var
  S: string;
begin
  Write('Password: ');
  ReadLn(S);           { visible echo - this is only a test tool }
  APassword := AnsiString(S);
end;

{ M1: single connection running a remote shell; lines go to its stdin. }
procedure RunShellMode;
var
  Line : string;
  Cmd  : AnsiString;
begin
  WriteLn('Connecting (shell mode) ...');
  SSH.Connect('');   { empty command => remote runs a login/shell (no PTY) }

  WriteLn('Connected. Enter commands one line at a time. Type "exit" to quit.');
  WriteLn('(The remote shell has no PTY, so there is no remote prompt.)');
  WriteLn;

  while True do
  begin
    { Show anything the shell produced (e.g. result of the last command). }
    DrainShell(300);

    if not SSH.IsConnected then
    begin
      WriteLn;
      WriteLn('[remote shell closed, exit status ', SSH.ExitStatus, ']');
      Break;
    end;

    Write('ssh> ');
    if Eof(Input) then          { Ctrl+Z (Windows) / Ctrl+D (Unix) }
    begin
      WriteLn;
      Break;
    end;
    ReadLn(Line);

    if SameText(Trim(Line), 'exit') or SameText(Trim(Line), 'quit') then
      Break;

    { Send the line plus a newline so the remote shell executes it. }
    Cmd := AnsiString(Line) + #10;
    try
      SSH.Send(PAnsiChar(Cmd), Length(Cmd));
    except
      on E: Exception do
      begin
        WriteLn('Send failed: ', E.Message);
        Break;
      end;
    end;
  end;

  { Politely tell the shell we're done, then collect any final output. }
  if SSH.IsConnected then
  begin
    SSH.SendEOF;
    DrainShell(500);
  end;
end;

{ M2: persistent connection; each line is run in its own fresh channel that
  is opened and then closed, while the SSH connection itself stays up. }
procedure RunPersistMode;
var
  Line : string;
  Ch   : TTGPuttySSHChannel;
begin
  WriteLn('Connecting (persist mode - channel per command) ...');
  SSH.ConnectPersistent;   { auth only; no channel yet }

  WriteLn('Connected. Each line runs as its own command in a new channel.');
  WriteLn('The SSH connection stays up between commands; shell state does NOT');
  WriteLn('carry over. Type "exit" to quit.');
  WriteLn;

  while True do
  begin
    if not SSH.IsConnected then
    begin
      WriteLn('[SSH connection lost]');
      Break;
    end;

    Write('ssh(persist)> ');
    if Eof(Input) then          { Ctrl+Z (Windows) / Ctrl+D (Unix) }
    begin
      WriteLn;
      Break;
    end;
    ReadLn(Line);

    if SameText(Trim(Line), 'exit') or SameText(Trim(Line), 'quit') then
      Break;
    if Trim(Line) = '' then
      Continue;

    { Open a fresh channel that execs this command. }
    try
      Ch := SSH.OpenChannel(AnsiString(Line));
    except
      on E: Exception do
      begin
        WriteLn('OpenChannel failed: ', E.Message);
        Continue;
      end;
    end;

    try
      { We send no stdin, so give the command EOF immediately - that lets
        stdin-reading commands (cat, wc, ...) finish instead of hanging. }
      Ch.SendEOF;
      DrainChannelUntilDone(Ch, 300);
      WriteLn;
      WriteLn('[command exit status ', Ch.ExitStatus, ']');
    finally
      Ch.Free;   { closes the channel; the SSH connection stays up }
    end;
  end;
end;

var
  Persist  : Boolean;
  Positional : array of string;
  i        : Integer;
  Arg      : string;
  Host, User : AnsiString;
  Port       : Integer;
  Pwd        : AnsiString;
begin
  { Parse args: an optional -persist flag (anywhere) plus positional
    <host> <user> [port]. }
  Persist := False;
  SetLength(Positional, 0);
  for i := 1 to ParamCount do
  begin
    Arg := ParamStr(i);
    if SameText(Arg, '-persist') or SameText(Arg, '--persist') then
      Persist := True
    else
    begin
      SetLength(Positional, Length(Positional) + 1);
      Positional[High(Positional)] := Arg;
    end;
  end;

  if Length(Positional) < 2 then
  begin
    WriteLn('Usage: sshterm [-persist] <host> <user> [port]');
    WriteLn('  default   : remote shell, lines fed to its stdin (state persists)');
    WriteLn('  -persist  : persistent connection, each line run in a new channel');
    WriteLn('Type "exit" (or Ctrl+Z/Ctrl+D + Enter) to quit.');
    Halt(1);
  end;

  Host := AnsiString(Positional[0]);
  User := AnsiString(Positional[1]);
  if Length(Positional) >= 3 then
    Port := StrToIntDef(Positional[2], 22)
  else
    Port := 22;

  ReadPassword(Pwd);

  Handlers := TTermHandlers.Create;
  SSH := nil;
  try
    SSH := TTGPuttySSH.Create(False { verbose });
    SSH.OnVerifyHostKey := Handlers.VerifyHostKey;
    SSH.OnMessage       := Handlers.LibMessage;
    SSH.HostName := Host;
    SSH.UserName := User;
    SSH.Port     := Port;
    SSH.Password := Pwd;

    try
      if Persist then
        RunPersistMode
      else
        RunShellMode;
    except
      on E: Exception do
        WriteLn('Error: ', E.Message);
    end;

  finally
    if Assigned(SSH) then
    begin
      SSH.Disconnect;
      SSH.Free;
    end;
    Handlers.Free;
  end;
end.
