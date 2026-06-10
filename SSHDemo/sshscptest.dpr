program sshscptest;

{
  Test program for TGPuttyLib's scp support (tgscp_download / tgscp_upload,
  i.e. PuTTY's pscp protocol engine running over a channel of the
  persistent SSH connection).

  What it does:
    1. ConnectPersistent (auth only, no channel).
    2. Generates ~760 KB of deterministic pseudo-random binary data
       (all byte values, including #0, #10, #13 - proves 8-bit cleanliness)
       and uploads it via ScpUploadStream with a fixed timestamp. The test
       filename contains a SPACE on purpose, to exercise the library's
       shell quoting and the scp C-record name parsing.
    3. Runs 'ls -l' of the uploaded file on ANOTHER channel of the SAME
       connection - proving scp and shell commands coexist per design.
    4. Downloads the file back via ScpDownloadStream and verifies:
       byte-for-byte equality, reported size, and the timestamp coming
       back from the server's T record (timestamp mismatch is a WARN,
       not a FAIL - some servers/filesystems don't preserve it exactly).
    5. Negative test: downloading a nonexistent file must raise an
       exception carrying the remote scp error text.
    6. Cleanup: rm the test file over another channel, verify it's gone.

  Each step prints PASS/FAIL; exits with code 1 if anything failed.

  Build (Windows, Delphi):   dcc32 sshscptest.dpr   (or dcc64)
  Build (Linux/macOS, FPC):  fpc sshscptest.dpr
  Needs tgputtylib.dll / libtgputty.so next to the executable, and a
  Unix-like server with scp in the PATH.

  Usage:  sshscptest <host> <user> [port] [remotedir]
          (port defaults to 22, remotedir to /tmp;
           password is read from the console)
}

{$APPTYPE CONSOLE}
{$ifdef FPC}{$MODE Delphi}{$endif}

uses
  SysUtils,
  Classes,
  tgputtylib,
  tgputtyssh;

const
  TESTSIZE  = 777777;      { odd size - catches off-by-one chunk bugs }
  TESTMTIME = 1600000000;  { 2020-09-13 12:26:40 UTC, sent in the T record }

type
  TTermHandlers = class
    ProgressCalls : Integer;
    LastBytes     : Int64;
    function VerifyHostKey(const host: PAnsiChar; const port: Integer;
                           const fingerprint: PAnsiChar;
                           const verificationstatus: Integer;
                           var storehostkey: Boolean): Boolean;
    procedure LibMessage(const Msg: AnsiString; const isstderr: Boolean);
    function Progress(const bytescopied: Int64; const isupload: Boolean): Boolean;
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
  Write('[lib] ', string(Msg));
  if (Msg <> '') and (Msg[Length(Msg)] <> #10) then
    WriteLn;
end;

function TTermHandlers.Progress(const bytescopied: Int64; const isupload: Boolean): Boolean;
begin
  Inc(ProgressCalls);
  LastBytes := bytescopied;
  Write('.');             { one dot per chunk - shows OnProgress is wired }
  Result := True;         { False would abort the transfer }
end;

var
  SSH      : TTGPuttySSH;
  Handlers : TTermHandlers;

function CIFstr(const Cond: Boolean; const IfTrue, IfFalse: string): string;
begin
  if Cond then
    Result := IfTrue
  else
    Result := IfFalse;
end;

{ Run one command on its own channel of the persistent connection and
  return its combined stdout+stderr. Same pattern as sshmultichan.dpr. }
function RunCommand(const cmd: AnsiString): AnsiString;
const BUFSIZE = 65536;
var
  Ch     : TTGPuttySSHChannel;
  OutBuf : array[0..BUFSIZE-1] of AnsiChar;
  ErrBuf : array[0..BUFSIZE-1] of AnsiChar;
  OutLen : Integer;
  ErrLen : Integer;
  chunk  : AnsiString;
  Waited : Integer;
begin
  Result := '';
  Ch := SSH.OpenChannel(cmd);
  try
    Ch.SendEOF;
    Waited := 0;
    while True do begin
      if not Ch.CanReceive(300) then begin
        if not Ch.IsOpen then
          Break;
        Inc(Waited);
        if Waited > 100 then begin   { ~30 s safety net }
          WriteLn('  RunCommand timeout!');
          Break;
        end;
        Continue;
      end;
      OutLen := BUFSIZE;
      ErrLen := BUFSIZE;
      Ch.Receive(@OutBuf[0], OutLen, @ErrBuf[0], ErrLen);
      if (OutLen = 0) and (ErrLen = 0) then
        Break;  { EOF }
      if OutLen > 0 then begin
        SetString(chunk, PAnsiChar(@OutBuf[0]), OutLen);
        Result := Result + chunk;
      end;
      if ErrLen > 0 then begin
        SetString(chunk, PAnsiChar(@ErrBuf[0]), ErrLen);
        Result := Result + chunk;
      end;
    end;
  finally
    Ch.Free;
  end;
end;

{ Deterministic pseudo-random data covering all byte values. }
procedure FillTestData(const MS: TMemoryStream; const Size: Integer);
var
  i    : Integer;
  seed : Cardinal;
  p    : PByte;
begin
  MS.Size := Size;
  p := MS.Memory;
  seed := 42;
  for i := 1 to Size do begin
    seed := seed * 1103515245 + 12345;   { simple LCG }
    p^ := (seed shr 16) and $FF;
    Inc(p);
  end;
  MS.Position := 0;
end;

var
  Host, User, RemoteDir : AnsiString;
  Port       : Integer;
  PwdLine    : string;
  TestName   : AnsiString;
  RemotePath : AnsiString;
  UpStream   : TMemoryStream;
  DownStream : TMemoryStream;
  GotSize    : UInt64;
  GotMTime   : UInt64;
  Res        : AnsiString;
  AllPass    : Boolean;
  StepPass   : Boolean;
begin
  if ParamCount < 2 then begin
    WriteLn('Usage: sshscptest <host> <user> [port] [remotedir]');
    WriteLn('  port defaults to 22, remotedir to /tmp');
    Halt(1);
  end;
  Host := AnsiString(ParamStr(1));
  User := AnsiString(ParamStr(2));
  if ParamCount >= 3 then
    Port := StrToIntDef(ParamStr(3), 22)
  else
    Port := 22;
  if ParamCount >= 4 then
    RemoteDir := AnsiString(ParamStr(4))
  else
    RemoteDir := '/tmp';

  Write('Password: ');
  ReadLn(PwdLine);

  Randomize;
  TestName   := AnsiString('tgscp test ' + IntToStr(Random(1000000)) + '.bin');
  RemotePath := RemoteDir + '/' + TestName;   { note: contains a space! }

  Handlers := TTermHandlers.Create;
  SSH := nil;
  UpStream := TMemoryStream.Create;
  DownStream := TMemoryStream.Create;
  AllPass := True;
  try
    try
      SSH := TTGPuttySSH.Create(False);
      SSH.OnVerifyHostKey := Handlers.VerifyHostKey;
      SSH.OnMessage       := Handlers.LibMessage;
      SSH.OnProgress      := Handlers.Progress;
      SSH.HostName := Host;
      SSH.UserName := User;
      SSH.Port     := Port;
      SSH.Password := AnsiString(PwdLine);

      WriteLn('Connecting (persistent) ...');
      SSH.ConnectPersistent;
      WriteLn('Connected.');
      WriteLn;

      { --- 1. Upload --- }
      WriteLn('--- 1. scp upload: "', string(TestName), '" (', TESTSIZE, ' bytes) to ', string(RemoteDir), ' ---');
      FillTestData(UpStream, TESTSIZE);
      Handlers.ProgressCalls := 0;
      SSH.ScpUploadStream(RemoteDir, TestName, UpStream, TESTMTIME, {perms=}-1);
      WriteLn;
      WriteLn('  uploaded; progress callbacks: ', Handlers.ProgressCalls,
              ', last byte count: ', Handlers.LastBytes);
      StepPass := Handlers.LastBytes = TESTSIZE;
      WriteLn('  RESULT: ', CIFstr(StepPass, 'PASS', 'FAIL (progress did not reach file size)'));
      AllPass := AllPass and StepPass;
      WriteLn;

      { --- 2. Listing over the same connection --- }
      WriteLn('--- 2. ls -l over the SAME connection (separate channel) ---');
      Res := RunCommand(AnsiString('ls -l ''') + RemotePath + AnsiString(''''));
      Write(string(Res));
      StepPass := Pos(AnsiString(IntToStr(TESTSIZE)), Res) > 0;
      WriteLn('  RESULT: ', CIFstr(StepPass, 'PASS (size visible in listing)',
                                   'FAIL (expected size not found in listing)'));
      AllPass := AllPass and StepPass;
      WriteLn;

      { --- 3. Download + verify --- }
      WriteLn('--- 3. scp download + verification ---');
      Handlers.ProgressCalls := 0;
      SSH.ScpDownloadStream(RemotePath, DownStream, GotSize, GotMTime);
      WriteLn;
      WriteLn('  downloaded; reported size=', GotSize, ', mtime=', GotMTime,
              ', stream size=', DownStream.Size);

      StepPass := (GotSize = TESTSIZE) and (DownStream.Size = TESTSIZE);
      if not StepPass then
        WriteLn('  RESULT: FAIL (size mismatch)')
      else begin
        StepPass := CompareMem(UpStream.Memory, DownStream.Memory, TESTSIZE);
        if StepPass then
          WriteLn('  RESULT: PASS (', TESTSIZE, ' bytes identical)')
        else
          WriteLn('  RESULT: FAIL (data differs!)');
      end;
      AllPass := AllPass and StepPass;

      if GotMTime = TESTMTIME then
        WriteLn('  timestamp round-trip: PASS (', TESTMTIME, ')')
      else
        WriteLn('  timestamp round-trip: WARN - sent ', TESTMTIME, ', got back ',
                GotMTime, ' (server may not preserve times; not a failure)');
      WriteLn;

      { --- 4. Negative test --- }
      WriteLn('--- 4. download of a nonexistent file must fail cleanly ---');
      StepPass := False;
      try
        SSH.ScpDownloadStream(RemoteDir + AnsiString('/tgscp nonexistent ') +
                              AnsiString(IntToStr(Random(1000000))),
                              DownStream, GotSize, GotMTime);
        WriteLn('  RESULT: FAIL (no exception was raised)');
      except
        on E: Exception do begin
          WriteLn('  got expected exception: ', E.Message);
          StepPass := True;
          WriteLn('  RESULT: PASS');
        end;
      end;
      AllPass := AllPass and StepPass;
      WriteLn;

      { --- 5. Cleanup --- }
      WriteLn('--- 5. cleanup (rm over another channel) ---');
      RunCommand(AnsiString('rm ''') + RemotePath + AnsiString(''''));
      Res := RunCommand(AnsiString('ls -l ''') + RemotePath + AnsiString(''''));
      StepPass := (Pos(AnsiString('No such'), Res) > 0) or (Pos(AnsiString('no such'), Res) > 0);
      if not StepPass then
        Write(string(Res));
      WriteLn('  RESULT: ', CIFstr(StepPass, 'PASS (file removed)',
                                   'FAIL (file still listed?)'));
      AllPass := AllPass and StepPass;

      WriteLn;
      if SSH.IsConnected then
        WriteLn('Connection still up after all operations: PASS')
      else begin
        WriteLn('Connection DROPPED: FAIL');
        AllPass := False;
      end;

    except
      on E: Exception do begin
        WriteLn;
        WriteLn('Error: ', E.Message);
        AllPass := False;
      end;
    end;
  finally
    if Assigned(SSH) then begin
      SSH.Disconnect;
      SSH.Free;
    end;
    UpStream.Free;
    DownStream.Free;
    Handlers.Free;
  end;

  WriteLn;
  if AllPass then
    WriteLn('=========== OVERALL: PASS ===========')
  else begin
    WriteLn('=========== OVERALL: FAIL ===========');
    Halt(1);
  end;
end.
