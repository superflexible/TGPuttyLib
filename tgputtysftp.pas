unit tgputtysftp;

{$ifdef FPC}{$MODE Delphi}{$endif}

interface

uses {$ifdef SFFS}TGGlobal,Basics,{$endif}
     Classes, SysUtils, DateUtils,
     tgputtylib;

{$POINTERMATH ON}

const MinimumLibraryBuildNum=1;
      cDummyClearedErrorCode=-1000; // this error code means there was no real error code

type TGPuttySFTPException=class(Exception);

     TOnMessage=procedure(const Msg:AnsiString;const isstderr:Boolean) of object;
     TOnProgress=function(const bytescopied:Int64;const isupload:Boolean):Boolean of object;
     TOnListing=function(const names:Pfxp_names):Boolean of object;
     TOnGetInput=function(var cancel:Boolean):AnsiString of object;
     TOnVerifyHostKey=function(const host:PAnsiChar;const port:Integer;
                               const fingerprint:PAnsiChar;
                               const verificationstatus:Integer;
                               var storehostkey:Boolean):Boolean of object;

     TTGPuttySFTP=class(TObject)
       private
         Fcontext:TTGLibraryContext;
         FVerbose:Boolean;
         FHostName,FUserName,FPassword,FKeyPassword:AnsiString;
         FPort:Integer;
         FOnMessage: TOnMessage;
         FOnProgress: TOnProgress;
         FOnListing: TOnListing;
         FOnGetInput: TOnGetInput;
         FOnVerifyHostKey: TOnVerifyHostKey;
         FUploadStream,
         FDownloadStream:TStream;
         FConnected:Boolean;
         FPasswordAttempts:Integer;
         FLastMessages:AnsiString;
         function GetHomeDir: AnsiString;
         function GetWorkDir: AnsiString;
         procedure SetVerbose(const Value: Boolean);
         procedure SetKeyfile(const Value: AnsiString);
         function GetLibVersion: AnsiString;
         function GetErrorCode: Integer;
         function GetErrorMessage: AnsiString;
       public
         constructor Create(const verbose:Boolean);
         destructor Destroy; override;

         function MakePSFTPErrorMsg(const where:string):string;

         procedure Connect;
         procedure Disconnect;

         procedure ChangeDir(const ADirectory:AnsiString);
         procedure MakeDir(const ADirectory:AnsiString);
         procedure RemoveDir(const ADirectory:AnsiString);
         procedure ListDir(const ADirectory:AnsiString);

         procedure GetStat(const AFileName:AnsiString;var Attrs:fxp_attrs);
         procedure SetStat(const AFileName:AnsiString;const Attrs:fxp_attrs);
         procedure SetModifiedDate(const AFileName:AnsiString;const ATimestamp:TDateTime; const isUTC:Boolean);
         procedure SetFileSize(const AFileName:AnsiString;const ASize:Int64);
         procedure Move(const AFromName,AToName:AnsiString);
         procedure DeleteFile(const AName:AnsiString);

         procedure UploadFile(const ALocalFilename,ARemoteFilename:AnsiString;const anAppend:Boolean);
         procedure DownloadFile(const ARemoteFilename,ALocalFilename:AnsiString;const anAppend:Boolean);

         procedure UploadStream(const ARemoteFilename:AnsiString;const AStream:TStream; const anAppend:Boolean);
         procedure DownloadStream(const ARemoteFilename:AnsiString;const AStream:TStream; const anAppend:Boolean);

         function OpenFile(const apathname:AnsiString;
                           const anopenflags:Integer;
                           const attrs:Pfxp_attrs):TSFTPFileHandle;
         function CloseFile(var fh:TSFTPFileHandle):Integer;

         function xfer_upload_init(const fh:TSFTPFileHandle;const offset:UInt64):TSFTPTransfer;
         function xfer_upload_ready(const xfer:TSFTPTransfer):Boolean;
         procedure xfer_upload_data(const xfer:TSFTPTransfer;const buffer:Pointer;
                                    const len:Integer;const anoffset:UInt64);
         function xfer_ensuredone(const xfer:TSFTPTransfer):Boolean;
         function xfer_done(const xfer:TSFTPTransfer):Boolean;
         procedure xfer_cleanup(const xfer:TSFTPTransfer);

         property HostName:AnsiString read FHostName write FHostName;
         property UserName:AnsiString read FUserName write FUserName;
         property Port:Integer read FPort write FPort;
         property Password:AnsiString read FPassword write FPassword;
         property KeyPassword:AnsiString read FKeyPassword write FKeyPassword;

         property HomeDir:AnsiString read GetHomeDir;
         property WorkDir:AnsiString read GetWorkDir;
         property LibVersion:AnsiString read GetLibVersion;

         property Connected:Boolean read FConnected;
         property Verbose:Boolean read FVerbose write SetVerbose;
         property Keyfile:AnsiString write SetKeyfile;
         property LastMessages:AnsiString read FLastMessages write FLastMessages;
         property ErrorCode:Integer read GetErrorCode;
         property ErrorMessage:AnsiString read GetErrorMessage;

         property OnMessage:TOnMessage read FOnMessage write FOnMessage;
         property OnProgress:TOnProgress read FOnProgress write FOnProgress;
         property OnListing:TOnListing read FOnListing write FOnListing;
         property OnGetInput:TOnGetInput read FOnGetInput write FOnGetInput;
         property OnVerifyHostKey:TOnVerifyHostKey read FOnVerifyHostKey write FOnVerifyHostKey;
       end;

implementation

function ls_callback(const names:Pfxp_names;const libctx:PTGLibraryContext):Boolean; cdecl;
var TGPSFTP:TTGPuttySFTP;
begin
  TGPSFTP:=TTGPuttySFTP(libctx.Tag);
  if Assigned(TGPSFTP.OnListing) then
     Result:=TGPSFTP.OnListing(names)
  else
     Result:=true;
  end;

function getpassword_callback(const prompt:PAnsiChar;const echo:Boolean;const cancel:PBoolean;const libctx:PTGLibraryContext):PAnsiChar; cdecl;
var TGPSFTP:TTGPuttySFTP;
begin
  TGPSFTP:=TTGPuttySFTP(libctx.Tag);
  Inc(TGPSFTP.FPasswordAttempts);
  if TGPSFTP.FPasswordAttempts>3 then begin
     cancel^:=true;
     if Assigned(TGPSFTP.OnMessage) then
        TGPSFTP.OnMessage('Password was rejected, or no password given for '+prompt+'.'+sLineBreak,true);
     end
  else begin
    if System.Pos('Passphrase for key',prompt)>0 then
       Result:=PAnsiChar(TGPSFTP.KeyPassword)
    else
       Result:=PAnsiChar(TGPSFTP.Password);
    cancel^:=false;
    end;
  end;

procedure printmessage_callback(const msg:PAnsiChar;const isstderr:Boolean;const libctx:PTGLibraryContext); cdecl;
var TGPSFTP:TTGPuttySFTP;
begin
  TGPSFTP:=TTGPuttySFTP(libctx.Tag);
  if Assigned(TGPSFTP.OnMessage) then
     TGPSFTP.OnMessage(msg,isstderr);
  TGPSFTP.LastMessages:=TGPSFTP.LastMessages+msg;
  end;

function progress_callback(const bytescopied:Int64;const isupload:Boolean;const libctx:PTGLibraryContext):Boolean; cdecl;
var TGPSFTP:TTGPuttySFTP;
begin
  TGPSFTP:=TTGPuttySFTP(libctx.Tag);
  if Assigned(TGPSFTP.OnProgress) then
     Result:=TGPSFTP.OnProgress(bytescopied,isupload)
  else
     Result:=true;
  end;

function get_input_callback(linebuf:PAnsiChar;const maxchars:Integer;const libctx:PTGLibraryContext):Boolean; cdecl;
var line:AnsiString;
    TGPSFTP:TTGPuttySFTP;
    cancel:Boolean;
begin
  TGPSFTP:=TTGPuttySFTP(libctx.Tag);

  if Assigned(TGPSFTP.OnGetInput) then begin
     line:=TGPSFTP.OnGetInput(cancel);
     Result:=not cancel;
     end
  else
     try
       Write('Your Input: ');
       ReadLn(line);
       Result:=true;
       except
         raise Exception.Create('No input method available');
       end;

  if Result then begin
     if Length(line)>maxchars then
        SetLength(line,maxchars);
     if line>'' then begin
        Move(line[1],linebuf^,maxchars);
        linebuf[Length(line)]:=#0;
        end
     else
        linebuf^:=#0;
     end;
  end;

var FS:TFileStream;

function read_from_stream(const offset:UInt64;const buffer:Pointer;const bufsize:Integer;const libctx:PTGLibraryContext):Integer; cdecl;
var TGPSFTP:TTGPuttySFTP;
begin
  TGPSFTP:=TTGPuttySFTP(libctx.Tag);
  if Assigned(TGPSFTP.FUploadStream) then begin
     TGPSFTP.FUploadStream.Position:=Offset;
     Result:=TGPSFTP.FUploadStream.Read(buffer^,bufsize);
     end
  else
     Result:=0;
  end;

function write_to_stream(const offset:UInt64;const buffer:Pointer;const bufsize:Integer;const libctx:PTGLibraryContext):Integer; cdecl;
var TGPSFTP:TTGPuttySFTP;
begin
  TGPSFTP:=TTGPuttySFTP(libctx.Tag);
  if Assigned(TGPSFTP.FDownloadStream) then begin
     TGPSFTP.FDownloadStream.Position:=Offset;
     Result:=TGPSFTP.FDownloadStream.Write(buffer^,bufsize);
     end
  else
     Result:=0;
  end;

procedure raise_exception_callback(const msg:PAnsiChar;const srcfile:PAnsiChar;const line:Integer;const libctx:PTGLibraryContext); cdecl;
begin
{$ifdef SFFS}
  if IndyLogging then begin
     WriteLn(IndyLog,'NOW RAISING TTGPuttySFTP exception '+AnsiString(msg)+' at line '+IntToStr(line)+' in '+AnsiString(srcfile));
     CloseFile(IndyLog);
     Append(IndyLog);
     if Logging then begin
        WriteLn(LogFile,'NOW RAISING TTGPuttySFTP exception '+AnsiString(msg)+' at line '+IntToStr(line)+' in '+AnsiString(srcfile));
        CloseFile(LogFile);
        Append(LogFile);
        end;
     end;
{$endif}
  raise TGPuttySFTPException.Create('TTGPuttySFTP exception '+AnsiString(msg)+' at line '+IntToStr(line)+' in '+AnsiString(srcfile));
  end;

function verify_host_key_callback(const host:PAnsiChar;const port:Integer;const keytype:PAnsiChar;
                                  const keystr:PAnsiChar;const fingerprint:PAnsiChar;
                                  const verificationstatus:Integer;const storehostkey:PBoolean;
                                  const libctx:PTGLibraryContext):Boolean; cdecl;
var TGPSFTP:TTGPuttySFTP;
begin
  TGPSFTP:=TTGPuttySFTP(libctx.Tag);
  if Assigned(TGPSFTP.OnVerifyHostKey) then
     Result:=TGPSFTP.OnVerifyHostKey(host,port,fingerprint,verificationstatus,storehostkey^)
  else
     Result:=false;
  end;

{ TTGPuttySFTP }

procedure TTGPuttySFTP.ChangeDir(const ADirectory: AnsiString);
var res:Integer;
begin
  FLastMessages:='';
  Fcontext.fxp_errtype:=cDummyClearedErrorCode; // "clear" error field
  res:=tgsftp_cd(PAnsiChar(ADirectory),@Fcontext);
  if res<>1 then // 1 = success
     raise TGPuttySFTPException.Create(MakePSFTPErrorMsg('tgsftp_cd'));
  end;

function TTGPuttySFTP.CloseFile(var fh: TSFTPFileHandle): Integer;
begin
  Result:=tgputty_closefile(@fh,@Fcontext);
  end;

procedure TTGPuttySFTP.Connect;
var res:Integer;
begin
  FPasswordAttempts:=0;
  FLastMessages:='';
  Fcontext.fxp_errtype:=cDummyClearedErrorCode; // "clear" error field
  res:=tgsftp_connect(PAnsiChar(FHostName),PAnsiChar(FUserName),FPort,PAnsiChar(FPassword),@Fcontext);
  FConnected:=res=0; // 0 = success
  if not FConnected then
     raise TGPuttySFTPException.Create(MakePSFTPErrorMsg('tgsftp_connect'));
  end;

constructor TTGPuttySFTP.Create(const verbose:Boolean);
var puttyversion:Double;
    tgputtylibbuild:Integer;
begin
  tgputtygetversions(@puttyversion,@tgputtylibbuild);
  if tgputtylibbuild<MinimumLibraryBuildNum then
     raise Exception.Create('tgputtylib is too old, its build number is '+
                             IntToStr(tgputtylibbuild)+
                            ', but we need a minimum of '+IntToStr(MinimumLibraryBuildNum));

  Fcontext.Init;
  FVerbose:=verbose;

  Fcontext.structsize:=sizeof(Fcontext);
  if Fcontext.structsize<tggetlibrarycontextsize then
     raise Exception.Create('Incorrect TTGLibraryContext record size');
  Fcontext.Tag:=UInt64(NativeUInt(self));
  Fcontext.ls_callback:=ls_callback;
  Fcontext.getpassword_callback:=getpassword_callback;
  Fcontext.printmessage_callback:=printmessage_callback;
  Fcontext.progress_callback:=progress_callback;
  Fcontext.read_from_stream:=read_from_stream;
  Fcontext.write_to_stream:=write_to_stream;
  Fcontext.get_input_callback:=get_input_callback;
  Fcontext.raise_exception_callback:=raise_exception_callback;
  Fcontext.verify_host_key_callback:=verify_host_key_callback;

  if tgputty_initcontext(verbose,@Fcontext)<>0 then
     raise TGPuttySFTPException.Create('tgputty_initcontext failed - incorrect DLL version?');
  end;

procedure TTGPuttySFTP.DeleteFile(const AName: AnsiString);
var res:Integer;
begin
  FLastMessages:='';
  Fcontext.fxp_errtype:=cDummyClearedErrorCode; // "clear" error field
  res:=tgsftp_rm(PAnsiChar(AName),@Fcontext);
  if res<>1 then // 1 = success
     raise TGPuttySFTPException.Create(MakePSFTPErrorMsg('tgsftp_rm'));
  end;

destructor TTGPuttySFTP.Destroy;
begin
  Disconnect;
  tgputtyfree(@Fcontext);
  inherited;
  end;

procedure TTGPuttySFTP.Disconnect;
begin
  if FConnected then begin
     tgsftp_close(@Fcontext);
     FConnected:=false;
     end;
  end;

procedure TTGPuttySFTP.DownloadFile(const ARemoteFilename, ALocalFilename: AnsiString; const anAppend: Boolean);
var res:Integer;
begin
  FLastMessages:='';
  Fcontext.fxp_errtype:=cDummyClearedErrorCode; // "clear" error field
  res:=tgsftp_getfile(PAnsiChar(ARemoteFilename),PAnsiChar(ALocalFilename),anAppend,@Fcontext);
  if res<>1 then
     raise TGPuttySFTPException.Create(MakePSFTPErrorMsg('tgsftp_getfile'));
  end;

procedure TTGPuttySFTP.DownloadStream(const ARemoteFilename: AnsiString; const AStream: TStream; const anAppend: Boolean);
var res:Integer;
begin
  FLastMessages:='';
  FDownloadStream:=AStream;
  Fcontext.fxp_errtype:=cDummyClearedErrorCode; // "clear" error field
  try
    res:=tgsftp_getfile(PAnsiChar(ARemoteFilename),nil,anAppend,@Fcontext);
    if res<>1 then
       raise TGPuttySFTPException.Create(MakePSFTPErrorMsg('tgsftp_getfile'));
    finally
      FDownloadStream:=nil;
    end;
  end;

function TTGPuttySFTP.GetErrorCode: Integer;
begin
  Result:=Fcontext.fxp_errtype;
  end;

function TTGPuttySFTP.GetErrorMessage: AnsiString;
begin
  Result:=Fcontext.fxp_error_message;
  end;

function TTGPuttySFTP.GetHomeDir: AnsiString;
begin
  Result:=Fcontext.homedir;
  end;

function TTGPuttySFTP.GetLibVersion: AnsiString;
var puttyversion:Double;
    tgputtylibbuild:Integer;
    strpv:string;
begin
  tgputtygetversions(@puttyversion,@tgputtylibbuild);
  Str(puttyversion:0:2,strpv);
  Result:='tgputtylib build '+IntToStr(tgputtylibbuild)+' based on PuTTY Release '+strpv;
  end;

procedure TTGPuttySFTP.GetStat(const AFileName: AnsiString; var Attrs: fxp_attrs);
begin
  FLastMessages:='';
  Fcontext.fxp_errtype:=cDummyClearedErrorCode; // "clear" error field
  if not tgsftp_getstat(PAnsiChar(AFileName),@Attrs,@Fcontext) then
     raise TGPuttySFTPException.Create(MakePSFTPErrorMsg('tgsftp_getstat'));
  end;

function TTGPuttySFTP.GetWorkDir: AnsiString;
begin
  Result:=Fcontext.pwd;
  end;

procedure TTGPuttySFTP.ListDir(const ADirectory: AnsiString);
var res:Integer;
begin
  FLastMessages:='';
  Fcontext.fxp_errtype:=cDummyClearedErrorCode; // "clear" error field
  res:=tgsftp_ls(PAnsiChar(ADirectory),@Fcontext);
  if res<>1 then
     raise TGPuttySFTPException.Create(MakePSFTPErrorMsg('tgsftp_ls'));
  end;

procedure TTGPuttySFTP.MakeDir(const ADirectory: AnsiString);
var res:Integer;
begin
  FLastMessages:='';
  Fcontext.fxp_errtype:=cDummyClearedErrorCode; // "clear" error field
  res:=tgsftp_mkdir(PAnsiChar(ADirectory),@Fcontext);
  if res<>1 then // 1 = success
     raise TGPuttySFTPException.Create(MakePSFTPErrorMsg('tgsftp_mkdir'));
  end;

function TTGPuttySFTP.MakePSFTPErrorMsg(const where: string): string;
begin
  if Fcontext.fxp_errtype>=0 then
     Result:=where+': Error '+IntToStr(Fcontext.fxp_errtype)+', '+Fcontext.fxp_error_message
  else
     Result:=where+': Unknown Error.'+sLineBreak+LastMessages;
  end;

procedure TTGPuttySFTP.Move(const AFromName, AToName: AnsiString);
var res:Integer;
begin
  FLastMessages:='';
  Fcontext.fxp_errtype:=cDummyClearedErrorCode; // "clear" error field
  res:=tgsftp_mv(PAnsiChar(AFromName),PAnsiChar(AToName),@Fcontext);
  if res<>1 then // 1 = success
     raise TGPuttySFTPException.Create(MakePSFTPErrorMsg('tgsftp_mv'));
  end;

function TTGPuttySFTP.OpenFile(const apathname: AnsiString; const anopenflags: Integer; const attrs: Pfxp_attrs): TSFTPFileHandle;
begin
  Result:=tgputty_openfile(PAnsiChar(apathname),anopenflags,attrs,@Fcontext);
  end;

procedure TTGPuttySFTP.RemoveDir(const ADirectory: AnsiString);
var res:Integer;
begin
  FLastMessages:='';
  Fcontext.fxp_errtype:=cDummyClearedErrorCode; // "clear" error field
  res:=tgsftp_rmdir(PAnsiChar(ADirectory),@Fcontext);
  if res<>1 then // 1 = success
     raise TGPuttySFTPException.Create(MakePSFTPErrorMsg('tgsftp_rmdir'));
  end;

procedure TTGPuttySFTP.SetFileSize(const AFileName: AnsiString; const ASize: Int64);
var Attrs:fxp_attrs;
begin
  GetStat(AFileName,Attrs);
  attrs.flags := SSH_FILEXFER_ATTR_SIZE; // set only this
  attrs.size:=ASize;
  SetStat(AFileName,Attrs);
  end;

procedure TTGPuttySFTP.SetKeyfile(const Value: AnsiString);
begin
  tgputty_setkeyfile(PAnsiChar(Value),@Fcontext);
  end;

procedure TTGPuttySFTP.SetModifiedDate(const AFileName: AnsiString;const ATimestamp: TDateTime; const isUTC:Boolean);
var Attrs:fxp_attrs;
begin
  GetStat(AFileName,Attrs);
  attrs.flags := SSH_FILEXFER_ATTR_ACMODTIME; // set only this
  {$ifdef FPC}
  if isUTC then
     attrs.mtime:=DateTimeToUnix(ATimestamp)
  else
     attrs.mtime:=DateTimeToUnix(LocalTimeToUniversal(ATimestamp));
  {$else}
  attrs.mtime:=DateTimeToUnix(ATimestamp,isUTC);
  {$endif}
  SetStat(AFileName,Attrs);
  end;

procedure TTGPuttySFTP.SetStat(const AFileName: AnsiString;const Attrs: fxp_attrs);
begin
  FLastMessages:='';
  Fcontext.fxp_errtype:=cDummyClearedErrorCode; // "clear" error field
  if not tgsftp_setstat(PAnsiChar(AFileName),@Attrs,@Fcontext) then
     raise TGPuttySFTPException.Create(MakePSFTPErrorMsg('tgsftp_setstat'));
  end;

procedure TTGPuttySFTP.SetVerbose(const Value: Boolean);
begin
  tgputty_setverbose(Value);
  FVerbose:=Value;
  end;

procedure TTGPuttySFTP.UploadFile(const ALocalFilename, ARemoteFilename: AnsiString; const anAppend: Boolean);
var res:Integer;
begin
  FLastMessages:='';
  Fcontext.fxp_errtype:=cDummyClearedErrorCode; // "clear" error field
  res:=tgsftp_putfile(PAnsiChar(ALocalFilename),PAnsiChar(ARemoteFilename),anAppend,@Fcontext);
  if res<>1 then
     raise TGPuttySFTPException.Create(MakePSFTPErrorMsg('tgsftp_putfile'));
  end;

procedure TTGPuttySFTP.UploadStream(const ARemoteFilename: AnsiString; const AStream: TStream; const anAppend: Boolean);
var res:Integer;
begin
  FLastMessages:='';
  FUploadStream:=AStream;
  Fcontext.fxp_errtype:=cDummyClearedErrorCode; // "clear" error field
  try
    res:=tgsftp_putfile(nil,PAnsiChar(ARemoteFilename),anAppend,@Fcontext);
    if res<>1 then
       raise TGPuttySFTPException.Create(MakePSFTPErrorMsg('tgsftp_putfile'));
    finally
      FUploadStream:=nil;
    end;
  end;

procedure TTGPuttySFTP.xfer_cleanup(const xfer: TSFTPTransfer);
begin
  tgputty_xfer_cleanup(xfer,@Fcontext);
  end;

function TTGPuttySFTP.xfer_done(const xfer: TSFTPTransfer): Boolean;
begin
  Result:=tgputty_xfer_done(xfer,@Fcontext);
  end;

function TTGPuttySFTP.xfer_ensuredone(const xfer: TSFTPTransfer): Boolean;
begin
  Result:=tgputty_xfer_ensuredone(xfer,@Fcontext);
  end;

procedure TTGPuttySFTP.xfer_upload_data(const xfer: TSFTPTransfer; const buffer: Pointer; const len: Integer; const anoffset: UInt64);
begin
  tgputty_xfer_upload_data(xfer,buffer,len,anoffset,@Fcontext);
  end;

function TTGPuttySFTP.xfer_upload_init(const fh: TSFTPFileHandle; const offset: UInt64): TSFTPTransfer;
begin
  Result:=tgputty_xfer_upload_init(fh,offset,@Fcontext);
  end;

function TTGPuttySFTP.xfer_upload_ready(const xfer: TSFTPTransfer): Boolean;
begin
  Result:=tgputty_xfer_upload_ready(xfer,@Fcontext);
  end;

end.
