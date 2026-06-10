unit tgputtyssh;

{$ifdef FPC}{$MODE Delphi}{$endif}

// Raw SSH client wrapper around tgputtylib.dll / libtgputty.so.
//
// Where TTGPuttySFTP (tgputtysftp.pas) drives the SFTP subsystem, this
// class exposes a *raw* SSH connection: it execs an arbitrary command
// (typically "rsync --server ...") and lets the caller stream bytes to
// its stdin and read its stdout/stderr - the moral equivalent of
// SecureBlackBox's TElSimpleSSHClient.
//
// Two usage models are supported, mirroring SBSimpleSSH:
//
//   Milestone 1 - one command per connection (SBB Pattern A):
//     SSH := TTGPuttySSH.Create(false);
//     SSH.HostName := ...; SSH.UserName := ...; SSH.Password := ...;
//     SSH.Connect('rsync --server ...');   // connects + execs
//     SSH.Send(...); SSH.Receive(...); SSH.SendEOF;
//     SSH.Disconnect;
//
//   Milestone 2 - persistent connection, channel per command
//   (SBB Pattern C - the recommended model for multiple rsync ops):
//     SSH.ConnectPersistent;               // auth only, no channel
//     Ch := SSH.OpenChannel('rsync --server ...');
//     ... Send/Receive on Ch ...
//     SSH.CloseChannel(Ch);                // connection stays up
//     SSH.Disconnect;
//
// Pascal unit Written and Copyright 2025 by Tobias Giesen
// License: same as PuTTY / tgputtylib.

interface

uses {$ifdef SFFS}
     TGGlobal, // for TPasswordString
     {$endif}
     {$ifdef MSWINDOWS}Windows,{$endif}
     Classes, SysUtils, SyncObjs,
     tgputtylib;

const MinimumLibraryBuildNum=32; // raw SSH channels + scp exports (tgscp_*) require build 32

      cMaxConfCount=1000;

{$ifndef SFFS}
type TPasswordString=AnsiString;
{$endif}

type TGPuttySSHException=class(Exception);

     TSSHOnMessage=procedure(const Msg:AnsiString;const isstderr:Boolean) of object;
     TSSHOnCheckpoint=procedure(const Msg:AnsiString;const kind:Byte) of object;
     TSSHOnGetInput=function(var cancel:Boolean):AnsiString of object;
     TSSHOnProgress=function(const bytescopied:Int64;const isupload:Boolean):Boolean of object;
     TSSHOnVerifyHostKey=function(const host:PAnsiChar;const port:Integer;
                                  const fingerprint:PAnsiChar;
                                  const verificationstatus:Integer;
                                  var storehostkey:Boolean):Boolean of object;

     TConfIntArray=array[0..cMaxConfCount-1] of Integer;
     TConfPAnsiCharArray=array[0..cMaxConfCount-1] of PAnsiChar;

     PConfIntArray=^TConfIntArray;
     PConfPAnsiCharArray=^TConfPAnsiCharArray;

     { TTGPuttySSHChannel - one session channel on a persistent connection
       (Milestone 2). Created by TTGPuttySSH.OpenChannel; closing it leaves
       the underlying SSH connection up for the next channel. }

     TTGPuttySSHChannel=class(TObject)
       private
         FCtx:PTGLibraryContext;     // borrowed: points at the owner's context
         FHandle:TSSHChannel;
         FClosed:Boolean;
       public
         constructor Create(const ACtx:PTGLibraryContext; const AHandle:TSSHChannel);
         destructor Destroy; override;

         { Send Count bytes to the channel command's stdin. }
         procedure Send(const Buffer:Pointer; const Count:Integer);
         { Wait up to TimeoutMS for data or channel end. }
         function CanReceive(const TimeoutMS:Integer):Boolean;
         { Non-blocking drain of stdout/stderr; see TTGPuttySSH.Receive. }
         function Receive(const StdOutBuf:Pointer; var StdOutLen:Integer;
                          const StdErrBuf:Pointer; var StdErrLen:Integer):Integer;
         { Half-close stdin. }
         procedure SendEOF;
         { Remote command exit status, or < 0 while still running. }
         function ExitStatus:Integer;
         { True while the channel is open and usable. }
         function IsOpen:Boolean;
         { Close the channel and invalidate the handle. Idempotent. The SSH
           connection stays up. }
         procedure Close;

         property Handle:TSSHChannel read FHandle;
       end;

     { TTGPuttySSH }

     TTGPuttySSH=class(TObject)
       private
         Fcontext:TTGLibraryContext;
         FVerbose,FCheckpoints:Boolean;
         FHostName,FUserName:AnsiString;
         FPassword,FKeyPassword:TPasswordString;
         FPort:Integer;
         FConnected:Boolean;
         FAttempts:Integer;
         FLastMessages:AnsiString;
         FConfNames:PConfPAnsiCharArray;
         FConfTypes:PConfIntArray;
         FConfSubTypes:PConfIntArray;
         FConfCount:Integer;
         FTempPassword,FTempKeyPassword:AnsiString;

         FOnMessage: TSSHOnMessage;
         FOnCheckpoint: TSSHOnCheckpoint;
         FOnGetInput: TSSHOnGetInput;
         FOnVerifyHostKey: TSSHOnVerifyHostKey;
         FOnProgress: TSSHOnProgress;

         { Streams for the scp functions; bound for the duration of one
           ScpDownloadStream / ScpUploadStream call, accessed by the
           write_to_stream / read_from_stream callbacks. }
         FUploadStream,
         FDownloadStream:TStream;

         function GetWorkDir: AnsiString;
         procedure SetVerbose(const Value: Boolean);
         procedure SetCheckpoints(const Value: Boolean);
         procedure SetKeyfile(const Value: AnsiString);
         function GetLibVersion: AnsiString;
         function GetAborted: Boolean;
         function GetConnectionTimeoutTicks: Integer;
         function GetTimeoutTicks: Integer;
         procedure SetAborted(const Value: Boolean);
         procedure SetConnectionTimeoutTicks(const Value: Integer);
         procedure SetTimeoutTicks(const Value: Integer);
         procedure CreateConfigIndex;
         function GetProxyType: TProxyTypes;
         function GetProxyHost: AnsiString;
         function GetProxyPassword: AnsiString;
         function GetProxyUserName: AnsiString;
         function GetProxyPort: Integer;
         procedure SetProxyType(const Value: TProxyTypes);
         procedure SetProxyHost(const Value: AnsiString);
         procedure SetProxyPassword(const Value: AnsiString);
         procedure SetProxyUserName(const Value: AnsiString);
         procedure SetProxyPort(const Value: Integer);
         procedure ResolvePassword;
         procedure ClearTempPasswords;

         procedure CP(const ACP:AnsiString);
       public
         tgputtylibbuild:Integer;
         constructor Create(const verbose:Boolean);
         destructor Destroy; override;

         function MakeSSHErrorMsg(const where:string):string;
         function GetPuttyConfIndex(const name:string):Integer;

         { Milestone 1: connect and exec a single command. The connection
           lasts as long as the command runs. }
         procedure Connect(const ACommand:AnsiString);

         { Milestone 2: connect WITHOUT opening any channel. Auth runs and
           the SSH connection is kept alive; use OpenChannel for each
           command (e.g. each rsync operation). }
         procedure ConnectPersistent;

         { Open a new session channel and exec ACommand on it. The caller
           owns the returned object and must Free it (which closes the
           channel); the SSH connection stays up. Raises on failure. }
         function OpenChannel(const ACommand:AnsiString):TTGPuttySSHChannel;

         procedure Disconnect;

         { Send Count bytes to the remote command's stdin. Raises on a
           dead connection. }
         procedure Send(const Buffer:Pointer; const Count:Integer);

         { Bytes of stdin still buffered locally (not yet on the wire). }
         function SendBuffer:Integer;

         { Wait up to TimeoutMS for stdout/stderr data or command exit.
           TimeoutMS < 0 waits indefinitely. Returns True if data is
           available or the command exited; False on timeout/abort. }
         function CanReceive(const TimeoutMS:Integer):Boolean;

         { Non-blocking drain. On entry StdOutLen/StdErrLen hold buffer
           capacities; on return the bytes actually copied. Either buffer
           may be nil (pass 0 length). Returns total bytes copied. }
         function Receive(const StdOutBuf:Pointer; var StdOutLen:Integer;
                          const StdErrBuf:Pointer; var StdErrLen:Integer):Integer;

         { Half-close: signal EOF on the command's stdin. }
         procedure SendEOF;

         { Remote command exit status, or < 0 while still running. }
         function ExitStatus:Integer;

         { True while the SSH connection / command is live. }
         function IsConnected:Boolean;

         { scp file transfer over a channel of the persistent connection,
           using PuTTY's pscp protocol engine inside the DLL. Requires
           ConnectPersistent. The remote paths are passed UNESCAPED - the
           library does the shell quoting. Progress/abort via OnProgress
           (return false to abort) and the Aborted property. Raises
           TGPuttySSHException on failure (message includes LastMessages,
           which carries the remote scp error text).

           Download: the file lands in AStream; ASize gets the file size,
           AMTimeUnix the remote mtime from scp's T record (0 if none).

           Upload: AStream from position 0, size = AStream.Size (scp needs
           it up front). AMTimeUnix 0 = don't send a timestamp.
           APermissions e.g. $1A4 (octal 0644); -1 = default. }
         procedure ScpDownloadStream(const ARemotePath:AnsiString;const AStream:TStream;
                                     out ASize:UInt64;out AMTimeUnix:UInt64);
         procedure ScpUploadStream(const ARemoteDir,AFileName:AnsiString;const AStream:TStream;
                                   const AMTimeUnix:UInt64;const APermissions:Integer);

         procedure SetBooleanConfigValue(const OptionName:AnsiString;const OptionValue:Boolean);
         procedure SetIntegerConfigValue(const OptionName:AnsiString;const OptionValue:Integer);
         procedure SetIntegerConfigValueWithSubkey(const OptionName:AnsiString;const OptionSubKey,OptionValue:Integer);
         procedure SetStringConfigValue(const OptionName,OptionValue:AnsiString);

         property HostName:AnsiString read FHostName write FHostName;
         property UserName:AnsiString read FUserName write FUserName;
         property Port:Integer read FPort write FPort;
         property Password:TPasswordString read FPassword write FPassword;
         property KeyPassword:TPasswordString read FKeyPassword write FKeyPassword;

         property WorkDir:AnsiString read GetWorkDir;
         property LibVersion:AnsiString read GetLibVersion;

         property Connected:Boolean read FConnected;
         property Verbose:Boolean read FVerbose write SetVerbose;
         property Checkpoints:Boolean read FCheckpoints write SetCheckpoints;
         property Keyfile:AnsiString write SetKeyfile;
         property LastMessages:AnsiString read FLastMessages write FLastMessages;
         property TimeoutTicks:Integer read GetTimeoutTicks write SetTimeoutTicks;
         property ConnectionTimeoutTicks:Integer read GetConnectionTimeoutTicks write SetConnectionTimeoutTicks;
         property Aborted:Boolean read GetAborted write SetAborted;

         property ProxyType:TProxyTypes read GetProxyType write SetProxyType;
         property ProxyHost:AnsiString read GetProxyHost write SetProxyHost;
         property ProxyPort:Integer read GetProxyPort write SetProxyPort;
         property ProxyUserName:AnsiString read GetProxyUserName write SetProxyUserName;
         property ProxyPassword:AnsiString read GetProxyPassword write SetProxyPassword;

         property OnMessage:TSSHOnMessage read FOnMessage write FOnMessage;
         property OnCheckpoint:TSSHOnCheckpoint read FOnCheckpoint write FOnCheckpoint;
         property OnGetInput:TSSHOnGetInput read FOnGetInput write FOnGetInput;
         property OnVerifyHostKey:TSSHOnVerifyHostKey read FOnVerifyHostKey write FOnVerifyHostKey;
         property OnProgress:TSSHOnProgress read FOnProgress write FOnProgress;
       end;

implementation

var GPuttyConfigIndex:tStringList;
    GPuttyConfigCS:TCriticalSection;

{ ---- C callbacks (libctx.Tag points back at the TTGPuttySSH instance) ---- }

function ssh_ls_callback(const names:Pfxp_names;const libctx:PTGLibraryContext):Boolean; cdecl;
begin
  // raw SSH never lists directories; satisfy the callback contract.
  Result:=true;
  end;

function ssh_getpassword_callback(const prompt:PAnsiChar;const echo:Boolean;const cancel:System.PBoolean;const libctx:PTGLibraryContext):PAnsiChar; cdecl;
var SSH:TTGPuttySSH;
begin
  Result:=nil;
  SSH:=TTGPuttySSH(libctx.Tag);
  Inc(SSH.FAttempts);
  if SSH.FAttempts>3 then begin
     cancel^:=true;
     if Assigned(SSH.OnMessage) then
        SSH.OnMessage(AnsiString('Password was rejected, or no password given for ')+prompt+AnsiString('.')+sLineBreak,true);
     end
  else begin
    if System.Pos(AnsiString('Passphrase for key'),AnsiString(prompt))>0 then begin
       SSH.FTempKeyPassword:=SSH.KeyPassword;
       Result:=PAnsiChar(SSH.FTempKeyPassword);
       end
    else begin
       if SSH.FTempPassword='' then
          {$ifdef SFFS}
          SSH.FTempPassword:=SSH.Password.GetAnsiString;
          {$else}
          SSH.FTempPassword:=SSH.Password;
          {$endif}
       Result:=PAnsiChar(SSH.FTempPassword);
       end;
    cancel^:=false;
    end;
  end;

procedure ssh_printmessage_callback(const msg:PAnsiChar;const kind:Byte;const libctx:PTGLibraryContext); cdecl;
var SSH:TTGPuttySSH;
begin
  SSH:=TTGPuttySSH(libctx.Tag);
  if kind=2 then begin
     if Assigned(SSH.OnCheckpoint) then
        SSH.OnCheckpoint(msg,kind);
     end
  else begin
     if Assigned(SSH.OnMessage) then
        SSH.OnMessage(msg,kind=1);
     SSH.LastMessages:=SSH.LastMessages+msg;
     end;
  end;

function ssh_get_input_callback(linebuf:PAnsiChar;const maxchars:Integer;const libctx:PTGLibraryContext):Boolean; cdecl;
var line:AnsiString;
    SSH:TTGPuttySSH;
    cancel:Boolean;
begin
  SSH:=TTGPuttySSH(libctx.Tag);
  cancel:=false;

  if Assigned(SSH.OnGetInput) then begin
     line:=SSH.OnGetInput(cancel);
     Result:=not cancel;
     end
  else begin
     line:='';
     Result:=false;
     end;

  if Result then begin
     if Length(line)>maxchars then
        SetLength(line,maxchars);
     if line>'' then begin
        Move(line[1],linebuf^,Length(line));
        linebuf[Length(line)]:=#0;
        end
     else
        linebuf^:=#0;
     end;
  end;

procedure ssh_raise_exception_callback(const msg:PAnsiChar;const srcfile:PAnsiChar;const line:Integer;const libctx:PTGLibraryContext); cdecl;
begin
  raise TGPuttySSHException.Create('TTGPuttySSH exception '+
                                   {$ifdef UNICODE}Utf8ToString{$endif}(AnsiString(msg))+
                                   ' at line '+IntToStr(line)+' in '+
                                   {$ifdef UNICODE}Utf8ToString{$endif}(AnsiString(srcfile)));
  end;

function ssh_verify_host_key_callback(const host:PAnsiChar;const port:Integer;const keytype:PAnsiChar;
                                      const keystr:PAnsiChar;const fingerprint:PAnsiChar;
                                      const verificationstatus:Integer;const storehostkey:System.PBoolean;
                                      const libctx:PTGLibraryContext):Boolean; cdecl;
var SSH:TTGPuttySSH;
begin
  SSH:=TTGPuttySSH(libctx.Tag);
  if Assigned(SSH.OnVerifyHostKey) then
     Result:=SSH.OnVerifyHostKey(host,port,fingerprint,verificationstatus,storehostkey^)
  else
     Result:=false;
  end;

function ssh_read_from_stream(const offset:UInt64;const buffer:Pointer;const bufsize:Integer;const libctx:PTGLibraryContext):Integer; cdecl;
var SSH:TTGPuttySSH;
begin
  { scp upload data source. Exceptions must not cross the DLL boundary -
    report 0 bytes instead; the library aborts the transfer cleanly. }
  SSH:=TTGPuttySSH(libctx.Tag);
  try
    if Assigned(SSH.FUploadStream) then begin
       SSH.FUploadStream.Position:=offset;
       Result:=SSH.FUploadStream.Read(buffer^,bufsize);
       end
    else
       Result:=0;
    except
      Result:=0;
    end;
  end;

function ssh_write_to_stream(const offset:UInt64;const buffer:Pointer;const bufsize:Integer;const libctx:PTGLibraryContext):Integer; cdecl;
var SSH:TTGPuttySSH;
begin
  { scp download data sink. Same no-exceptions rule as above. }
  SSH:=TTGPuttySSH(libctx.Tag);
  try
    if Assigned(SSH.FDownloadStream) then begin
       SSH.FDownloadStream.Position:=offset;
       SSH.FDownloadStream.WriteBuffer(buffer^,bufsize);
       Result:=bufsize;
       end
    else
       Result:=0;
    except
      Result:=0;
    end;
  end;

function ssh_progress_callback(const bytescopied:Int64;const isupload:Boolean;const libctx:PTGLibraryContext):Boolean; cdecl;
var SSH:TTGPuttySSH;
begin
  SSH:=TTGPuttySSH(libctx.Tag);
  try
    if Assigned(SSH.OnProgress) then
       Result:=SSH.OnProgress(bytescopied,isupload)
    else
       Result:=true;
    except
      Result:=false; // treat a failing handler as an abort request
    end;
  end;

{ TTGPuttySSHChannel }

constructor TTGPuttySSHChannel.Create(const ACtx: PTGLibraryContext; const AHandle: TSSHChannel);
begin
  inherited Create;
  FCtx := ACtx;
  FHandle := AHandle;
  FClosed := False;
  end;

destructor TTGPuttySSHChannel.Destroy;
begin
  Close;
  inherited;
  end;

procedure TTGPuttySSHChannel.Send(const Buffer: Pointer; const Count: Integer);
begin
  if FClosed or (FHandle=nil) then
     raise TGPuttySSHException.Create('tgssh_channel_send: channel is closed');
  if Count<=0 then
     Exit;
  if tgssh_channel_send(FHandle,Buffer,Count,FCtx)<0 then
     raise TGPuttySSHException.Create('tgssh_channel_send failed');
  end;

function TTGPuttySSHChannel.CanReceive(const TimeoutMS: Integer): Boolean;
begin
  if FClosed or (FHandle=nil) then
     Result:=True // closed -> Receive will report EOF
  else
     Result:=tgssh_channel_can_recv(FHandle,TimeoutMS,FCtx);
  end;

function TTGPuttySSHChannel.Receive(const StdOutBuf: Pointer; var StdOutLen: Integer;
                                    const StdErrBuf: Pointer; var StdErrLen: Integer): Integer;
begin
  if FClosed or (FHandle=nil) then begin
     StdOutLen:=0;
     StdErrLen:=0;
     Result:=0;
     end
  else
     Result:=tgssh_channel_recv(FHandle,StdOutBuf,@StdOutLen,StdErrBuf,@StdErrLen,FCtx);
  end;

procedure TTGPuttySSHChannel.SendEOF;
begin
  if (not FClosed) and (FHandle<>nil) then
     tgssh_channel_send_eof(FHandle,FCtx);
  end;

function TTGPuttySSHChannel.ExitStatus: Integer;
begin
  if FHandle=nil then
     Result:=-1
  else
     Result:=tgssh_channel_exit_status(FHandle,FCtx);
  end;

function TTGPuttySSHChannel.IsOpen: Boolean;
begin
  Result:=(not FClosed) and (FHandle<>nil) and tgssh_channel_is_open(FHandle,FCtx);
  end;

procedure TTGPuttySSHChannel.Close;
begin
  if FClosed or (FHandle=nil) then
     Exit;
  FClosed:=True;
  tgssh_channel_close(FHandle,FCtx); // frees the handle on the C side
  FHandle:=nil;
  end;

{ TTGPuttySSH }

procedure TTGPuttySSH.CP(const ACP:AnsiString);
begin
  if Assigned(OnCheckpoint) then
     OnCheckpoint(ACP,2);
  end;

constructor TTGPuttySSH.Create(const verbose:Boolean);
var puttyversion:Double;
begin
  if not TGPuttyLibAvailable then
     raise TGPuttySSHException.Create('TGPuttyLib is not available: '+TGPuttyLibLoadError);
  tgputtygetversions(@puttyversion,@tgputtylibbuild);
  if tgputtylibbuild<MinimumLibraryBuildNum then
     raise TGPuttySSHException.Create('tgputtylib is too old, its build number is '+
                             IntToStr(tgputtylibbuild)+
                            ', but we need a minimum of '+IntToStr(MinimumLibraryBuildNum));

  Fcontext.Init;
  FVerbose:=verbose;
  FPort:=22;

  Fcontext.structsize:=sizeof(Fcontext);
  if Fcontext.structsize<tggetlibrarycontextsize then
     raise TGPuttySSHException.Create('Incorrect TTGLibraryContext record size');
  Fcontext.Tag:=UInt64(NativeUInt(self));
  Fcontext.ls_callback:=ssh_ls_callback;
  Fcontext.getpassword_callback:=ssh_getpassword_callback;
  Fcontext.printmessage_callback:=ssh_printmessage_callback;
  Fcontext.get_input_callback:=ssh_get_input_callback;
  Fcontext.raise_exception_callback:=ssh_raise_exception_callback;
  Fcontext.verify_host_key_callback:=ssh_verify_host_key_callback;
  Fcontext.read_from_stream:=ssh_read_from_stream;   // scp upload source
  Fcontext.write_to_stream:=ssh_write_to_stream;     // scp download sink
  Fcontext.progress_callback:=ssh_progress_callback;

  if tgputty_initcontext(ord(verbose),@Fcontext)<>0 then
     raise TGPuttySSHException.Create('tgputty_initcontext failed - incorrect tgputtylib version?');

  if not tgputty_getconfigarrays(@FConfTypes,@FConfSubTypes,@FConfNames,@FConfCount) then
     raise TGPuttySSHException.Create('tgputty_getconfigarrays failed - incorrect tgputtylib version?');

  if FConfCount>cMaxConfCount then
     FConfCount:=cMaxConfCount;

  CreateConfigIndex;
  end;

destructor TTGPuttySSH.Destroy;
begin
  Disconnect;
  tgputtyfree(@Fcontext);
  inherited;
  end;

procedure TTGPuttySSH.CreateConfigIndex;
var i:Integer;
begin
  GPuttyConfigCS.Enter;
  try
    if not Assigned(GPuttyConfigIndex) then begin
       GPuttyConfigIndex:=tStringList.Create;
       GPuttyConfigIndex.Sorted:=true;
       for i:=0 to FConfCount-1 do
         GPuttyConfigIndex.AddObject(string(FConfNames[i]),TObject(NativeUInt(i)));
       end;
    finally
      GPuttyConfigCS.Leave;
    end;
  end;

function TTGPuttySSH.GetPuttyConfIndex(const name: string): Integer;
var idx:Integer;
begin
  idx:=GPuttyConfigIndex.IndexOf(name);
  if idx<0 then
     raise TGPuttySSHException.Create('Putty Conf Name Not Found: '+name);
  Result:=NativeUInt(GPuttyConfigIndex.Objects[idx]);
  end;

function TTGPuttySSH.MakeSSHErrorMsg(const where: string): string;
begin
  Result:=where+' failed.'+sLineBreak+{$ifdef UNICODE}Utf8ToString{$endif}(LastMessages);
  end;

procedure TTGPuttySSH.ResolvePassword;
begin
  {$ifdef SFFS}
  FPassword.DecryptInto(FTempPassword); // avoids a temp AnsiString password leak
  {$else}
  FTempPassword:=FPassword;
  {$endif}
  end;

procedure TTGPuttySSH.ClearTempPasswords;
begin
  if Length(FTempPassword)>0 then
     FillChar(FTempPassword[1],Length(FTempPassword),32);
  if Length(FTempKeyPassword)>0 then
     FillChar(FTempKeyPassword[1],Length(FTempKeyPassword),32);
  {$ifdef SFFS}
  Purge(FTempPassword);
  Purge(FTempKeyPassword);
  {$endif}
  end;

procedure TTGPuttySSH.Connect(const ACommand: AnsiString);
var res:Integer;
begin
  FAttempts:=0;
  FLastMessages:='';
  ResolvePassword;
  res:=tgssh_connect(PAnsiChar(FHostName),PAnsiChar(FUserName),FPort,
                     PAnsiChar(FTempPassword),PAnsiChar(ACommand),@Fcontext);
  FConnected:=res=0; // 0 = success
  ClearTempPasswords;
  if not FConnected then
     raise TGPuttySSHException.Create(MakeSSHErrorMsg('tgssh_connect'));
  end;

procedure TTGPuttySSH.ConnectPersistent;
var res:Integer;
begin
  FAttempts:=0;
  FLastMessages:='';
  ResolvePassword;
  res:=tgssh_connect_persistent(PAnsiChar(FHostName),PAnsiChar(FUserName),FPort,
                                PAnsiChar(FTempPassword),@Fcontext);
  FConnected:=res=0; // 0 = success
  ClearTempPasswords;
  if not FConnected then
     raise TGPuttySSHException.Create(MakeSSHErrorMsg('tgssh_connect_persistent'));
  end;

function TTGPuttySSH.OpenChannel(const ACommand: AnsiString): TTGPuttySSHChannel;
var h:TSSHChannel;
begin
  if not FConnected then
     raise TGPuttySSHException.Create('OpenChannel: not connected');
  FLastMessages:='';
  h:=tgssh_open_channel(PAnsiChar(ACommand),@Fcontext);
  if h=nil then
     raise TGPuttySSHException.Create(MakeSSHErrorMsg('tgssh_open_channel'));
  Result:=TTGPuttySSHChannel.Create(@Fcontext,h);
  end;

procedure TTGPuttySSH.Disconnect;
begin
  ClearTempPasswords;
  if FConnected then begin
     tgssh_close(@Fcontext);
     FConnected:=false;
     end;
  end;

procedure TTGPuttySSH.Send(const Buffer: Pointer; const Count: Integer);
begin
  if not FConnected then
     raise TGPuttySSHException.Create('tgssh_send: not connected');
  if Count<=0 then
     Exit;
  if tgssh_send(Buffer,Count,@Fcontext)<0 then
     raise TGPuttySSHException.Create('tgssh_send failed');
  end;

function TTGPuttySSH.SendBuffer: Integer;
begin
  Result:=tgssh_sendbuffer(@Fcontext);
  end;

function TTGPuttySSH.CanReceive(const TimeoutMS: Integer): Boolean;
begin
  Result:=tgssh_can_recv(TimeoutMS,@Fcontext);
  end;

function TTGPuttySSH.Receive(const StdOutBuf: Pointer; var StdOutLen: Integer;
                             const StdErrBuf: Pointer; var StdErrLen: Integer): Integer;
begin
  Result:=tgssh_recv(StdOutBuf,@StdOutLen,StdErrBuf,@StdErrLen,@Fcontext);
  end;

procedure TTGPuttySSH.SendEOF;
begin
  tgssh_send_eof(@Fcontext);
  end;

function TTGPuttySSH.ExitStatus: Integer;
begin
  Result:=tgssh_get_exit_status(@Fcontext);
  end;

function TTGPuttySSH.IsConnected: Boolean;
begin
  Result:=FConnected and tgssh_is_connected(@Fcontext);
  end;

procedure TTGPuttySSH.ScpDownloadStream(const ARemotePath:AnsiString;const AStream:TStream;
                                        out ASize:UInt64;out AMTimeUnix:UInt64);
var res,perms:Integer;
begin
  if not FConnected then
     raise TGPuttySSHException.Create('ScpDownloadStream: not connected');
  ASize:=0;
  AMTimeUnix:=0;
  perms:=0;
  FLastMessages:='';
  FDownloadStream:=AStream;
  try
    res:=tgscp_download(PAnsiChar(ARemotePath),@ASize,@AMTimeUnix,@perms,@Fcontext);
    finally
      FDownloadStream:=nil;
    end;
  if res<>0 then
     raise TGPuttySSHException.Create(MakeSSHErrorMsg('tgscp_download ('+IntToStr(res)+')'));
  end;

procedure TTGPuttySSH.ScpUploadStream(const ARemoteDir,AFileName:AnsiString;const AStream:TStream;
                                      const AMTimeUnix:UInt64;const APermissions:Integer);
var res:Integer;
begin
  if not FConnected then
     raise TGPuttySSHException.Create('ScpUploadStream: not connected');
  FLastMessages:='';
  FUploadStream:=AStream;
  try
    res:=tgscp_upload(PAnsiChar(ARemoteDir),PAnsiChar(AFileName),
                      UInt64(AStream.Size),AMTimeUnix,APermissions,@Fcontext);
    finally
      FUploadStream:=nil;
    end;
  if res<>0 then
     raise TGPuttySSHException.Create(MakeSSHErrorMsg('tgscp_upload ('+IntToStr(res)+')'));
  end;

function TTGPuttySSH.GetWorkDir: AnsiString;
begin
  Result:=Fcontext.pwd;
  end;

function TTGPuttySSH.GetLibVersion: AnsiString;
var puttyversion:Double;
    libbuild:Integer;
    strpv:AnsiString;
begin
  tgputtygetversions(@puttyversion,@libbuild);
  Str(puttyversion:0:2,strpv);
  Result:=AnsiString('tgputtylib build ')+AnsiString(IntToStr(libbuild))+AnsiString(' based on PuTTY Release ')+strpv;
  end;

function TTGPuttySSH.GetAborted: Boolean;
begin
  Result:=Fcontext.aborted;
  end;

procedure TTGPuttySSH.SetAborted(const Value: Boolean);
begin
  Fcontext.aborted:=Value;
  end;

function TTGPuttySSH.GetConnectionTimeoutTicks: Integer;
begin
  Result:=Fcontext.connectiontimeoutticks;
  end;

procedure TTGPuttySSH.SetConnectionTimeoutTicks(const Value: Integer);
begin
  Fcontext.connectiontimeoutticks:=Value;
  end;

function TTGPuttySSH.GetTimeoutTicks: Integer;
begin
  Result:=Fcontext.timeoutticks;
  end;

procedure TTGPuttySSH.SetTimeoutTicks(const Value: Integer);
begin
  Fcontext.timeoutticks:=Value;
  end;

procedure TTGPuttySSH.SetVerbose(const Value: Boolean);
begin
  FVerbose:=Value;
  tgputty_setverbose(ord(FVerbose)+2*ord(FCheckpoints));
  end;

procedure TTGPuttySSH.SetCheckpoints(const Value: Boolean);
begin
  FCheckpoints:=Value;
  tgputty_setverbose(ord(FVerbose)+2*ord(FCheckpoints));
  end;

procedure TTGPuttySSH.SetKeyfile(const Value: AnsiString);
begin
  tgputty_setkeyfile(PAnsiChar(Value),@Fcontext);
  end;

procedure TTGPuttySSH.SetBooleanConfigValue(const OptionName: AnsiString; const OptionValue: Boolean);
begin
  tgputty_conf_set_bool(GetPuttyConfIndex(string(OptionName)),OptionValue,@FContext);
  end;

procedure TTGPuttySSH.SetIntegerConfigValue(const OptionName: AnsiString; const OptionValue: Integer);
begin
  tgputty_conf_set_int(GetPuttyConfIndex(string(OptionName)),OptionValue,@FContext);
  end;

procedure TTGPuttySSH.SetIntegerConfigValueWithSubkey(const OptionName: AnsiString; const OptionSubKey, OptionValue: Integer);
begin
  tgputty_conf_set_int_int(GetPuttyConfIndex(string(OptionName)),OptionSubKey,OptionValue,@FContext);
  end;

procedure TTGPuttySSH.SetStringConfigValue(const OptionName, OptionValue: AnsiString);
begin
  tgputty_conf_set_str(GetPuttyConfIndex(string(OptionName)),PAnsiChar(OptionValue),@FContext);
  end;

function TTGPuttySSH.GetProxyHost: AnsiString;
begin
  Result:=tgputty_conf_get_str(GetPuttyConfIndex(cPuttyConf_proxy_host),@FContext);
  end;

procedure TTGPuttySSH.SetProxyHost(const Value: AnsiString);
begin
  tgputty_conf_set_str(GetPuttyConfIndex(cPuttyConf_proxy_host),PAnsiChar(Value),@FContext);
  end;

function TTGPuttySSH.GetProxyPassword: AnsiString;
begin
  Result:=tgputty_conf_get_str(GetPuttyConfIndex(cPuttyConf_proxy_password),@FContext);
  end;

procedure TTGPuttySSH.SetProxyPassword(const Value: AnsiString);
begin
  tgputty_conf_set_str(GetPuttyConfIndex(cPuttyConf_proxy_password),PAnsiChar(Value),@FContext);
  end;

function TTGPuttySSH.GetProxyPort: Integer;
begin
  Result:=tgputty_conf_get_int(GetPuttyConfIndex(cPuttyConf_proxy_port),@FContext);
  end;

procedure TTGPuttySSH.SetProxyPort(const Value: Integer);
begin
  tgputty_conf_set_int(GetPuttyConfIndex(cPuttyConf_proxy_port),Value,@FContext);
  end;

function TTGPuttySSH.GetProxyType: TProxyTypes;
begin
  Result:=TProxyTypes(tgputty_conf_get_int(GetPuttyConfIndex(cPuttyConf_proxy_type),@FContext));
  end;

procedure TTGPuttySSH.SetProxyType(const Value: TProxyTypes);
begin
  tgputty_conf_set_int(GetPuttyConfIndex(cPuttyConf_proxy_type),ord(Value),@FContext);
  end;

function TTGPuttySSH.GetProxyUserName: AnsiString;
begin
  Result:=tgputty_conf_get_str(GetPuttyConfIndex(cPuttyConf_proxy_username),@FContext);
  end;

procedure TTGPuttySSH.SetProxyUserName(const Value: AnsiString);
begin
  tgputty_conf_set_str(GetPuttyConfIndex(cPuttyConf_proxy_username),PAnsiChar(Value),@FContext);
  end;

initialization
  GPuttyConfigCS:=TCriticalSection.Create;

finalization
  FreeAndNil(GPuttyConfigIndex);
  FreeAndNil(GPuttyConfigCS);

end.
