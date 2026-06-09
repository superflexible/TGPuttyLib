program sshmultichan;

{
  Multi-channel test for TGPuttyLib's raw SSH (Milestone 2 / Pattern C).

  Proves that several session channels can be open AT THE SAME TIME on a
  single persistent SSH connection, that their output streams stay isolated
  (no cross-talk), that channels can be closed independently, and that the
  SSH connection survives all of it.

  IMPORTANT: this is single-threaded. All channels are driven from one
  thread via the shared event loop - that is the supported usage model.
  Do NOT drive different channels from different threads.

  What it does:
    1. ConnectPersistent (auth only, no channel).
    2. Open 3 channels at once, each running a command that prints N tagged
       lines with a 1s pause between them:
          echo "A line 1"; sleep 1; echo "A line 2"; ...   (tag A)
          echo "B line 1"; sleep 1; ...                     (tag B)
          echo "C line 1"; sleep 1; ...                     (tag C)
       Because all three run concurrently, their lines arrive interleaved
       on the wire and must be demultiplexed into per-channel buffers.
    3. Poll the channels round-robin, accumulating each channel's output
       separately, until every channel hits EOF.
    4. Verify each channel received EXACTLY its own N lines and nothing
       belonging to another channel (the isolation check).
    5. Close all channels, confirm the connection is still up, then open a
       final channel to prove the connection persisted.

  Build (Windows, Delphi):   dcc32 sshmultichan.dpr   (or dcc64)
  Build (Linux/macOS, FPC):  fpc sshmultichan.dpr
  Needs tgputtylib.dll / libtgputty.so next to the executable, and a
  Unix-like server (uses sh syntax, echo and sleep).

  Usage:  sshmultichan <host> <user> [port]
          (password is read from the console)
}

{$APPTYPE CONSOLE}
{$ifdef FPC}{$MODE Delphi}{$endif}

uses
  SysUtils,
  Classes,        { TStringList, TThread.GetTickCount64 }
  tgputtylib,
  tgputtyssh;

const
  NCHAN       = 3;                 { number of concurrent channels }
  LINES       = 3;                 { tagged lines per channel }
  TAGS        : array[0..NCHAN-1] of string = ('A', 'B', 'C');
  OVERALL_MS  = 60000;            { safety deadline for the whole drain }

type
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
  WriteLn('Host key for ', string(AnsiString(host)), ':', port,
          '  fp=', string(AnsiString(fingerprint)), '  (auto-accepting)');
  storehostkey := False;
  Result := True;
end;

procedure TTermHandlers.LibMessage(const Msg: AnsiString; const isstderr: Boolean);
begin
  { Library diagnostics can be noisy during connect; keep them but tag them. }
  Write('[lib] ', string(Msg));
end;

var
  SSH      : TTGPuttySSH;
  Handlers : TTermHandlers;

function GetMS: Int64;
begin
  Result := Int64(TThread.GetTickCount64);
end;

{ Append whatever a single channel currently has buffered onto OutAcc/ErrAcc.
  Returns False if the channel signalled EOF (finished and drained). }
function PumpChannel(const Ch: TTGPuttySSHChannel;
                     var OutAcc, ErrAcc: AnsiString;
                     const PollMS: Integer): Boolean;
var
  OutBuf : array[0..16383] of AnsiChar;
  ErrBuf : array[0..16383] of AnsiChar;
  OutLen : Integer;
  ErrLen : Integer;
  Tmp    : AnsiString;
begin
  Result := True;
  if not Ch.CanReceive(PollMS) then
  begin
    { Quiet for now. If it has closed, that's EOF; otherwise it's just
      mid-"sleep" - report still-active so the caller keeps polling. }
    if not Ch.IsOpen then
      Result := False;
    Exit;
  end;

  OutLen := SizeOf(OutBuf);
  ErrLen := SizeOf(ErrBuf);
  Ch.Receive(@OutBuf[0], OutLen, @ErrBuf[0], ErrLen);

  if (OutLen = 0) and (ErrLen = 0) then
  begin
    Result := False;   { EOF: command finished and output drained }
    Exit;
  end;

  if OutLen > 0 then
  begin
    SetString(Tmp, PAnsiChar(@OutBuf[0]), OutLen);
    OutAcc := OutAcc + Tmp;
  end;
  if ErrLen > 0 then
  begin
    SetString(Tmp, PAnsiChar(@ErrBuf[0]), ErrLen);
    ErrAcc := ErrAcc + Tmp;
  end;
end;

{ Verify that every non-empty line of Text starts with "<Tag> line ", and
  that there are exactly ExpectedLines of them. Fills Report with details. }
function VerifyTag(const Text: AnsiString; const Tag: string;
                   const ExpectedLines: Integer; out Report: string): Boolean;
var
  Lines  : TStringList;
  i      : Integer;
  L      : string;
  Prefix : string;
  Good   : Integer;
  Foreign: Integer;
begin
  Prefix := Tag + ' line ';
  Good := 0;
  Foreign := 0;
  Lines := TStringList.Create;
  try
    Lines.Text := string(Text);   { splits on CR/LF }
    for i := 0 to Lines.Count - 1 do
    begin
      L := Trim(Lines[i]);
      if L = '' then Continue;
      if Copy(L, 1, Length(Prefix)) = Prefix then
        Inc(Good)
      else
        Inc(Foreign);
    end;
  finally
    Lines.Free;
  end;

  Result := (Good = ExpectedLines) and (Foreign = 0);
  Report := Format('%d/%d own lines, %d foreign lines', [Good, ExpectedLines, Foreign]);
end;

function BuildCmd(const Tag: string; const NLines: Integer): AnsiString;
var
  i : Integer;
  s : string;
begin
  s := '';
  for i := 1 to NLines do
  begin
    s := s + Format('echo "%s line %d";', [Tag, i]);
    if i < NLines then
      s := s + ' sleep 1;';
  end;
  Result := AnsiString(s);
end;

{ Run the final single channel that proves the connection persisted. }
procedure RunPersistedProof;
var
  Ch         : TTGPuttySSHChannel;
  OutAcc, ErrAcc : AnsiString;
  Deadline   : Int64;
begin
  WriteLn;
  WriteLn('--- Reopening one channel to prove the connection persisted ---');
  Ch := SSH.OpenChannel(AnsiString('echo "connection survived; reopened OK"'));
  try
    Ch.SendEOF;
    OutAcc := '';
    ErrAcc := '';
    Deadline := GetMS + 10000;
    while PumpChannel(Ch, OutAcc, ErrAcc, 200) do
      if GetMS > Deadline then
      begin
        WriteLn('  TIMEOUT waiting for proof channel');
        Break;
      end;
    Write('  output: ', string(OutAcc));
    if Pos('reopened OK', string(OutAcc)) > 0 then
      WriteLn('  RESULT: PASS (connection persisted across all channel closes)')
    else
      WriteLn('  RESULT: FAIL (unexpected output)');
  finally
    Ch.Free;
  end;
end;

var
  Host, User : AnsiString;
  Port       : Integer;
  Pwd        : AnsiString;
  PwdLine    : string;
  Ch         : array[0..NCHAN-1] of TTGPuttySSHChannel;
  OutAcc     : array[0..NCHAN-1] of AnsiString;
  ErrAcc     : array[0..NCHAN-1] of AnsiString;
  Done       : array[0..NCHAN-1] of Boolean;
  i          : Integer;
  AnyActive  : Boolean;
  AllPass    : Boolean;
  Report     : string;
  Pass       : Boolean;
  Deadline   : Int64;
begin
  if ParamCount < 2 then
  begin
    WriteLn('Usage: sshmultichan <host> <user> [port]');
    Halt(1);
  end;
  Host := AnsiString(ParamStr(1));
  User := AnsiString(ParamStr(2));
  if ParamCount >= 3 then
    Port := StrToIntDef(ParamStr(3), 22)
  else
    Port := 22;

  Write('Password: ');
  ReadLn(PwdLine);
  Pwd := AnsiString(PwdLine);

  Handlers := TTermHandlers.Create;
  SSH := nil;
  AllPass := True;
  try
    SSH := TTGPuttySSH.Create(False);
    SSH.OnVerifyHostKey := Handlers.VerifyHostKey;
    SSH.OnMessage       := Handlers.LibMessage;
    SSH.HostName := Host;
    SSH.UserName := User;
    SSH.Port     := Port;
    SSH.Password := Pwd;
    SSH.Verbose := true;

    WriteLn('Connecting (persistent) ...');
    SSH.ConnectPersistent;
    WriteLn('Connected.');
    WriteLn;

    { 1. Open all channels at once. }
    for i := 0 to NCHAN - 1 do
    begin
      Ch[i] := SSH.OpenChannel(BuildCmd(TAGS[i], LINES));
      Ch[i].SendEOF;            { commands read no stdin }
      OutAcc[i] := '';
      ErrAcc[i] := '';
      Done[i]   := False;
      WriteLn(Format('Opened channel %d (tag %s).', [i, TAGS[i]]));
    end;
    WriteLn(Format('%d channels open simultaneously. Draining round-robin ...',
                   [NCHAN]));
    WriteLn;

    { 2. Round-robin drain until every channel hits EOF. }
    Deadline := GetMS + OVERALL_MS;
    repeat
      AnyActive := False;
      for i := 0 to NCHAN - 1 do
        if not Done[i] then
        begin
          AnyActive := True;
          if not PumpChannel(Ch[i], OutAcc[i], ErrAcc[i], 100) then
          begin
            Done[i] := True;
            WriteLn(Format('Channel %d (tag %s) reached EOF, exit status %d.',
                           [i, TAGS[i], Ch[i].ExitStatus]));
          end;
        end;
      if GetMS > Deadline then
      begin
        WriteLn('OVERALL TIMEOUT - giving up on remaining channels.');
        Break;
      end;
    until not AnyActive;

    { 3. Report + verify isolation. }
    WriteLn;
    WriteLn('--- Per-channel output and verification ---');
    for i := 0 to NCHAN - 1 do
    begin
      WriteLn(Format('Channel %d (tag %s):', [i, TAGS[i]]));
      Write(string(OutAcc[i]));
      if OutAcc[i] <> '' then
        if OutAcc[i][Length(OutAcc[i])] <> #10 then WriteLn;
      if ErrAcc[i] <> '' then
        WriteLn('  [stderr] ', string(ErrAcc[i]));
      Pass := VerifyTag(OutAcc[i], TAGS[i], LINES, Report);
      if Pass then
        WriteLn('  RESULT: PASS (', Report, ')')
      else
      begin
        WriteLn('  RESULT: FAIL (', Report, ')');
        AllPass := False;
      end;
      WriteLn;
    end;

    { 4. Close all channels; connection should stay up. }
    for i := 0 to NCHAN - 1 do
      FreeAndNil(Ch[i]);
    WriteLn('All channels closed.');

    if SSH.IsConnected then
      WriteLn('Connection still up after closing all channels: PASS')
    else
    begin
      WriteLn('Connection DROPPED after closing channels: FAIL');
      AllPass := False;
    end;

    { 5. Prove we can still use the connection. }
    if SSH.IsConnected then
      RunPersistedProof;

    WriteLn;
    if AllPass then
      WriteLn('=========== OVERALL: PASS ===========')
    else
      WriteLn('=========== OVERALL: FAIL ===========');

  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      AllPass := False;
    end;
  end;

  if Assigned(SSH) then
  begin
    SSH.Disconnect;
    SSH.Free;
  end;
  Handlers.Free;

  WriteLn('Press any key to continue');

  ReadLn;

  if not AllPass then
    Halt(1);
end.
