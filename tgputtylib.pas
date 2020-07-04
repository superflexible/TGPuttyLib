unit tgputtylib;

{$ifdef FPC}
{$MODE Delphi}
{$else}
{$WARN SYMBOL_PLATFORM OFF}
{$endif}

interface

// Uses tgputtylib.dll, especially for SFTP functionality

// Pascal Units Written and Copyright 2019 by Tobias Giesen

// License: same as putty, can be freely copied, modified and
// used for both open source and closed source commercial projects

// make commands for object files:
// Visual Studio:
// del *.obj
// nmake -f Makefile.vc tgputtylib.dll     (32 bit)
// nmake -f Makefile64.vc tgputtylib.dll   (64 bit)
// (using the Developer Command Prompt in the "windows" subfolder)

uses Classes, SysUtils, SyncObjs;

{.$define USEMEMORYCALLBACKS}

{$ifndef FPC}
{$ifdef CONDITIONALEXPRESSIONS}
{$if CompilerVersion >= 21.0}
{$define HASDELAYED}
{$ifend}
{$endif}
{$endif}

const tgputtydll='tgputtylib.dll';

      cDefaultTimeoutTicks=60000;

{$ifdef USEMEMORYCALLBACKS}
      UseMemoryAllocationCallbacks=false;
      DebugMemory=false;
{$endif}

{$A8}

const
   SSH_FILEXFER_ATTR_SIZE       =$00000001;
   SSH_FILEXFER_ATTR_UIDGID     =$00000002;
   SSH_FILEXFER_ATTR_PERMISSIONS=$00000004;
   SSH_FILEXFER_ATTR_ACMODTIME  =$00000008;
   SSH_FILEXFER_ATTR_EXTENDED   =$80000000;

   SSH_FXF_READ                 =$00000001;
   SSH_FXF_WRITE                =$00000002;
   SSH_FXF_APPEND               =$00000004;
   SSH_FXF_CREAT                =$00000008;
   SSH_FXF_TRUNC                =$00000010;
   SSH_FXF_EXCL                 =$00000020;

   // with these flags, tgsftp_mvex can skip checking whether the destination is an existing folder
   cMoveFlag_DestinationPathIncludesItemName =1;
   cMoveFlag_AddSourceItemNameToDestinationPath =2;

   TYPE_NONE = 0;
   TYPE_BOOL = 1;
   TYPE_INT  = 2;
   TYPE_STR  = 3;
   TYPE_FILENAME = 4;

   txtPuttyConfTypes:array[0..4] of string=('none','Boolean','Integer','AnsiString','Filename');

   cPuttyConf_host='host'; // AnsiString default ''
   cPuttyConf_port='port'; // Integer default 0
   cPuttyConf_protocol='protocol'; // Integer default 0
   cPuttyConf_addressfamily='addressfamily'; // Integer default 0
   cPuttyConf_ping_interval='ping_interval'; // Integer default 0
   cPuttyConf_tcp_nodelay='tcp_nodelay'; // Boolean default TRUE
   cPuttyConf_tcp_keepalives='tcp_keepalives'; // Boolean default FALSE
   cPuttyConf_loghost='loghost'; // AnsiString default ''
   cPuttyConf_proxy_exclude_list='proxy_exclude_list'; // AnsiString default ''
   cPuttyConf_proxy_dns='proxy_dns'; // Integer default 2
   cPuttyConf_even_proxy_localhost='even_proxy_localhost'; // Boolean default FALSE
   cPuttyConf_proxy_type='proxy_type'; // Integer default 0
   cPuttyConf_proxy_host='proxy_host'; // AnsiString default 'proxy'
   cPuttyConf_proxy_port='proxy_port'; // Integer default 80
   cPuttyConf_proxy_username='proxy_username'; // AnsiString default ''
   cPuttyConf_proxy_password='proxy_password'; // AnsiString default ''
   cPuttyConf_proxy_telnet_command='proxy_telnet_command'; // AnsiString default 'connect %host %port\n'
   cPuttyConf_proxy_log_to_term='proxy_log_to_term'; // Integer default 1
   cPuttyConf_remote_cmd='remote_cmd'; // AnsiString default ''
   cPuttyConf_remote_cmd2='remote_cmd2'; // AnsiString default ''
   cPuttyConf_nopty='nopty'; // Boolean default FALSE
   cPuttyConf_compression='compression'; // Boolean default FALSE
   cPuttyConf_ssh_kexlist='ssh_kexlist'; // Integer [Integer]
   cPuttyConf_ssh_hklist='ssh_hklist'; // Integer [Integer]
   cPuttyConf_ssh_prefer_known_hostkeys='ssh_prefer_known_hostkeys'; // Boolean default TRUE - added in PuTTY v0.74
   cPuttyConf_ssh_rekey_time='ssh_rekey_time'; // Integer default 60
   cPuttyConf_ssh_rekey_data='ssh_rekey_data'; // AnsiString default '1G'
   cPuttyConf_tryagent='tryagent'; // Boolean default TRUE
   cPuttyConf_agentfwd='agentfwd'; // Boolean default FALSE
   cPuttyConf_change_username='change_username'; // Boolean default FALSE
   cPuttyConf_ssh_cipherlist='ssh_cipherlist'; // Integer [Integer]
   cPuttyConf_keyfile='keyfile'; // Filename
   cPuttyConf_sshprot='sshprot'; // Integer default 3
   cPuttyConf_ssh2_des_cbc='ssh2_des_cbc'; // Boolean default FALSE
   cPuttyConf_ssh_no_userauth='ssh_no_userauth'; // Boolean default FALSE
   cPuttyConf_ssh_show_banner='ssh_show_banner'; // Boolean default TRUE
   cPuttyConf_try_tis_auth='try_tis_auth'; // Boolean default FALSE
   cPuttyConf_try_ki_auth='try_ki_auth'; // Boolean default TRUE
   cPuttyConf_try_gssapi_auth='try_gssapi_auth'; // Boolean default TRUE
   cPuttyConf_try_gssapi_kex='try_gssapi_kex'; // Boolean default TRUE
   cPuttyConf_gssapifwd='gssapifwd'; // Boolean default FALSE
   cPuttyConf_gssapirekey='gssapirekey'; // Integer default 2
   cPuttyConf_ssh_gsslist='ssh_gsslist'; // Integer [Integer]
   cPuttyConf_ssh_gss_custom='ssh_gss_custom'; // Filename
   cPuttyConf_ssh_subsys='ssh_subsys'; // Boolean default FALSE
   cPuttyConf_ssh_subsys2='ssh_subsys2'; // Boolean default
   cPuttyConf_ssh_no_shell='ssh_no_shell'; // Boolean default FALSE
   cPuttyConf_ssh_nc_host='ssh_nc_host'; // AnsiString default ''
   cPuttyConf_ssh_nc_port='ssh_nc_port'; // Integer default
   cPuttyConf_termtype='termtype'; // AnsiString default 'xterm'
   cPuttyConf_termspeed='termspeed'; // AnsiString default '38400,38400'
   cPuttyConf_ttymodes='ttymodes'; // AnsiString [AnsiString]
   cPuttyConf_environmt='environmt'; // AnsiString [AnsiString]
   cPuttyConf_username='username'; // AnsiString default ''
   cPuttyConf_width='width'; // Integer default 80
   cPuttyConf_height='height'; // Integer default 24
   cPuttyConf_logfilename='logfilename'; // Filename
   cPuttyConf_logtype='logtype'; // Integer default 0
   cPuttyConf_logxfovr='logxfovr'; // Integer default -1
   cPuttyConf_logflush='logflush'; // Boolean default TRUE
   cPuttyConf_logheader='logheader'; // Boolean default TRUE
   cPuttyConf_logomitpass='logomitpass'; // Boolean default TRUE
   cPuttyConf_logomitdata='logomitdata'; // Boolean default FALSE
   cPuttyConf_lport_acceptall='lport_acceptall'; // Boolean default FALSE
   cPuttyConf_rport_acceptall='rport_acceptall'; // Boolean default FALSE
   cPuttyConf_portfwd='portfwd'; // AnsiString [AnsiString]
   cPuttyConf_sshbug_ignore1='sshbug_ignore1'; // Integer default 2
   cPuttyConf_sshbug_plainpw1='sshbug_plainpw1'; // Integer default 2
   cPuttyConf_sshbug_rsa1='sshbug_rsa1'; // Integer default 2
   cPuttyConf_sshbug_hmac2='sshbug_hmac2'; // Integer default 2
   cPuttyConf_sshbug_derivekey2='sshbug_derivekey2'; // Integer default 2
   cPuttyConf_sshbug_rsapad2='sshbug_rsapad2'; // Integer default 2
   cPuttyConf_sshbug_pksessid2='sshbug_pksessid2'; // Integer default 2
   cPuttyConf_sshbug_rekey2='sshbug_rekey2'; // Integer default 2
   cPuttyConf_sshbug_maxpkt2='sshbug_maxpkt2'; // Integer default 2
   cPuttyConf_sshbug_ignore2='sshbug_ignore2'; // Integer default 2
   cPuttyConf_sshbug_oldgex2='sshbug_oldgex2'; // Integer default 2
   cPuttyConf_sshbug_winadj='sshbug_winadj'; // Integer default 2
   cPuttyConf_sshbug_chanreq='sshbug_chanreq'; // Integer default 2
   cPuttyConf_ssh_simple='ssh_simple'; // Boolean default FALSE
   cPuttyConf_ssh_connection_sharing='ssh_connection_sharing'; // Boolean default FALSE
   cPuttyConf_ssh_connection_sharing_upstream='ssh_connection_sharing_upstream'; // Boolean default TRUE
   cPuttyConf_ssh_connection_sharing_downstream='ssh_connection_sharing_downstream'; // Boolean default TRUE
   cPuttyConf_ssh_manual_hostkeys='ssh_manual_hostkeys'; // AnsiString [AnsiString]


type
   TProxyTypes=(PROXY_NONE,PROXY_SOCKS4,PROXY_SOCKS5,PROXY_HTTP,PROXY_TELNET,PROXY_CMD,PROXY_FUZZ);

const
   txtProxyTypes:array[TProxyTypes] of string=('NONE','SOCKS4','SOCKS5','HTTP','TELNET','CMD','FUZZ');

type fxp_attrs=record
       flags:Cardinal;
       size: UInt64;
       uid,gid,
       permissions,
       atime,mtime:Cardinal;
       end;
     Pfxp_attrs=^fxp_attrs;

     fxp_name=record
       filename,longname:PAnsiChar;
       attrs:fxp_attrs;
       end;
     Pfxp_name=^fxp_name;
     Tfxp_name_array=array[0..20000000] of fxp_name;
     Pfxp_name_array=^Tfxp_name_array;

     fxp_names=record
       nnames:Integer;
       names:Pfxp_name;
       end;
     Pfxp_names=^fxp_names;

     TSFTPFileHandle=Pointer;
     PSFTPFileHandle=^TSFTPFileHandle;
     TSFTPTransfer=Pointer;

     PTGLibraryContext=^TTGLibraryContext;
     TTGLibraryContext=record
       structsize:Integer;
       Tag:UInt64;
       ls_callback:function(const names:Pfxp_names;const libctx:PTGLibraryContext):Boolean; cdecl;
       getpassword_callback:function(const prompt:PAnsiChar;const echo:Boolean;const cancel:PBoolean;const libctx:PTGLibraryContext):PAnsiChar; cdecl;
       printmessage_callback:procedure(const msg:PAnsiChar;const isstderr:Boolean;const libctx:PTGLibraryContext); cdecl;
       progress_callback:function(const bytescopied:Int64;const isupload:Boolean;const libctx:PTGLibraryContext):Boolean; cdecl;
       read_from_stream:function(const offset:UInt64;const buffer:Pointer;const bufsize:Integer;const libctx:PTGLibraryContext):Integer; cdecl;
       write_to_stream:function(const offset:UInt64;const buffer:Pointer;const bufsize:Integer;const libctx:PTGLibraryContext):Integer; cdecl;
       get_input_callback:function(linebuf:PAnsiChar;const maxchars:Integer;const libctx:PTGLibraryContext):Boolean; cdecl;
       verify_host_key_callback:function(const host:PAnsiChar;const port:Integer;const keytype:PAnsiChar;
                                         const keystr:PAnsiChar;const fingerprint:PAnsiChar;
                                         const verificationstatus:Integer;const storehostkey:PBoolean;
                                         const libctx:PTGLibraryContext):Boolean; cdecl;
       raise_exception_callback:procedure(const msg:PAnsiChar;const srcfile:PAnsiChar;const line:Integer;const libctx:PTGLibraryContext); cdecl;
       entercriticalsection_callback:procedure(const Num:Integer); cdecl;
       leavecriticalsection_callback:procedure(const Num:Integer); cdecl;

       // these aren't really good for much ...
       mode:Integer;
       modeflags:Integer;
       batchfile:PAnsiChar;

       // sftp client state from psftp.c
       pwd, homedir:PAnsiChar;

       // static items from sftp.c
       fxp_error_message:PAnsiChar;
       fxp_errtype:Integer;

       timeoutticks:Integer;
       connectiontimeoutticks:Integer;
       aborted:Boolean;

{$ifdef USEMEMORYCALLBACKS}
       malloc_callback:function(size:NativeUInt):Pointer;  cdecl;
       free_callback:procedure(ptr:Pointer);  cdecl;
       realloc_callback:function(ptr:Pointer; newsize:NativeUInt):Pointer;  cdecl;

       debug_malloc_callback:function(size:NativeUInt;const srcfile:PAnsiChar;const line:Integer):Pointer;  cdecl;
       debug_free_callback:procedure(ptr:Pointer;const srcfile:PAnsiChar;const line:Integer);  cdecl;
       debug_realloc_callback:function(ptr:Pointer; newsize:NativeUInt;const srcfile:PAnsiChar;const line:Integer):Pointer;  cdecl;

       usememorycallbacks: Boolean;
{$endif}

       reserved:array[0..300] of Byte;

       procedure Init;
       end;

{$ifdef WITHCOMMANDLINE} // TGPuttyLib is compiled without command line support by default
// run the whole psftp interactive commmand prompt
// includes init and cleanup, only one function call needed
function psftp_main(argcparam: Longint; argvparam: ppchar):Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
function tgputty_initwithcmdline(argcparam: Longint; argvparam: ppchar; const libctx:PTGLibraryContext):Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
{$endif}

// basic functions
function tggetlibrarycontextsize:Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
function tgputty_initcontext(const verbose:Boolean;const libctx:PTGLibraryContext):Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
procedure tgputtysetappname(const newappname,appversion:PAnsiChar); cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
procedure tgputty_setverbose(const averbose:Boolean); cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
procedure tgputtyfree(const libctx:PTGLibraryContext); cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
procedure tgputtygetversions(puttyrelease:PDouble; tgputtylibbuild:PInteger); cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif}; // TG 2019
function tgputty_getconfigarrays(types,subtypes,names:Pointer;count:PInteger):Boolean; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};

// run the whole psftp interactive commmand prompt
// after calling tgputtyinit
function tgputtyrunpsftp(const libctx:PTGLibraryContext):Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};

// run psftp command lines
function tgputtysftpcommand(const line:PAnsiChar; const libctx:PTGLibraryContext):Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};

// individual SFTP commands
procedure tgputty_setkeyfile(const aPathname:PAnsiChar;const libctx:PTGLibraryContext); cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
function tgsftp_connect(const aHost,aUser:PAnsiChar;
                        const aPort:Integer;
                        const aPassword:PAnsiChar;const libctx:PTGLibraryContext):Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
procedure tgsftp_close(const libctx:PTGLibraryContext); cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};

function tgsftp_cd(const adir:PAnsiChar; const libctx:PTGLibraryContext):Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
function tgsftp_ls(const adir:PAnsiChar; const libctx:PTGLibraryContext):Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};

function tgsftp_rm(const afile:PAnsiChar; const libctx:PTGLibraryContext):Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
function tgsftp_rmdir(const adir:PAnsiChar; const libctx:PTGLibraryContext):Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
function tgsftp_mkdir(const adir:PAnsiChar; const libctx:PTGLibraryContext):Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
function tgsftp_mv(const afrom,ato:PAnsiChar; const libctx:PTGLibraryContext):Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
function tgsftp_mvex(const afrom,ato:PAnsiChar; const moveflags:Integer; const libctx:PTGLibraryContext):Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};

function tgsftp_putfile(const afromfile,atofile:PAnsiChar; const anAppend:Boolean; const libctx:PTGLibraryContext):Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
function tgsftp_getfile(const afromfile,atofile:PAnsiChar; const anAppend:Boolean; const libctx:PTGLibraryContext):Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};

function tgsftp_getstat(const afile:PAnsiChar; attrs:Pfxp_attrs; const libctx:PTGLibraryContext):Boolean; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
function tgsftp_setstat(const afile:PAnsiChar; attrs:Pfxp_attrs; const libctx:PTGLibraryContext):Boolean; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};

function tgputty_openfile(const apathname:PAnsiChar;
                          const anopenflags:Integer;
                          const attrs:Pfxp_attrs;
                          const libctx:PTGLibraryContext):TSFTPFileHandle; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
function tgputty_closefile(const fh:PSFTPFileHandle;
                           const libctx:PTGLibraryContext):Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};

function tgputty_xfer_upload_init(const fh:TSFTPFileHandle;const offset:UInt64; const libctx:PTGLibraryContext):TSFTPTransfer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
function tgputty_xfer_upload_ready(const xfer:TSFTPTransfer; const libctx:PTGLibraryContext):Boolean; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
procedure tgputty_xfer_upload_data(const xfer:TSFTPTransfer;const buffer:Pointer;
                                   const len:Integer;const anoffset:UInt64;
                                   const libctx:PTGLibraryContext); cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
function tgputty_xfer_ensuredone(const xfer:TSFTPTransfer;const libctx:PTGLibraryContext):Boolean; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
function tgputty_xfer_done(const xfer:TSFTPTransfer;const libctx:PTGLibraryContext):Boolean; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
procedure tgputty_xfer_cleanup(const xfer:TSFTPTransfer;const libctx:PTGLibraryContext); cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};

function tgputty_conf_get_bool(key:Integer; const libctx:PTGLibraryContext):Boolean; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
function tgputty_conf_get_int(key:Integer; const libctx:PTGLibraryContext):Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
function tgputty_conf_get_int_int(key,subkey:Integer; const libctx:PTGLibraryContext):Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
// PAnsiChar result still owned by conf
function tgputty_conf_get_str(key:Integer; const libctx:PTGLibraryContext):PAnsiChar; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
function tgputty_conf_get_str_str(key:Integer;const subkey:PAnsiChar; const libctx:PTGLibraryContext):PAnsiChar; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};

procedure tgputty_conf_set_bool(key:Integer; Value: Boolean; const libctx:PTGLibraryContext); cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
procedure tgputty_conf_set_int(key:Integer; Value: Integer; const libctx:PTGLibraryContext); cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
procedure tgputty_conf_set_int_int(key,subkey:Integer; Value: Integer; const libctx:PTGLibraryContext); cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
procedure tgputty_conf_set_str(key:Integer; Value: PAnsiChar; const libctx:PTGLibraryContext); cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
procedure tgputty_conf_set_str_str(key:Integer;const subkey,Value:PAnsiChar; const libctx:PTGLibraryContext); cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};

implementation

{
const MaxCritSect=0; // not currently used

var CritSects:array[1..MaxCritSect] of TCriticalSection;

procedure TGPuttyDLLEnterCriticalSection(const Num:Integer); cdecl;
begin
  if (Num<1) or (Num>MaxCritSect) then
     raise Exception.Create('Invalid tgputtydll critical section number '+IntToStr(Num));
  CritSects[Num].Enter;
  end;

procedure TGPuttyDLLLeaveCriticalSection(const Num:Integer); cdecl;
begin
  if (Num<1) or (Num>MaxCritSect) then
     raise Exception.Create('Invalid tgputtydll critical section number '+IntToStr(Num));
  CritSects[Num].Leave;
  end;
}

{$ifdef USEMEMORYCALLBACKS}
function tgputtylib_malloc_callback(size:NativeUInt):Pointer; cdecl;
begin
  Result:=AllocMem(size);
  end;

procedure tgputtylib_free_callback(ptr:Pointer); cdecl;
begin
  FreeMem(ptr);
  end;

function tgputtylib_realloc_callback(ptr:Pointer; newsize:NativeUInt):Pointer; cdecl;
begin
  Result:=ptr;
  ReallocMem(Result,newsize);
  end;


type TStringObject=class(TObject)
       public
         Data:string;
         Tag:NativeUInt;
         constructor Create(const s:string;const t:NativeUInt);
       end;

constructor TStringObject.Create(const s:string;const t:NativeUInt);
begin
  inherited Create;
  Data:=s;
  Tag:=t;
  end;

var MallocLinesList,
    MallocPointerList:TStringList;
    MallocCS:TCriticalSection;
    ForgetPtrErrors:Integer;

procedure RememberPtr(const ptr:Pointer;const size:NativeUInt;const srcfile:PAnsiChar;const line:Integer);
var idx:NativeInt;
    str:string;
begin
  str:=string(AnsiString(srcfile))+':'+IntToStr(line);
  idx:=MallocLinesList.IndexOf(str);
  if idx<0 then
     idx:=MallocLinesList.Add(str);
  // increase counter
  MallocLinesList.Objects[idx]:=TObject(NativeUInt(MallocLinesList.Objects[idx])+1);

  // save pointer with string
  MallocPointerList.AddObject(IntToHex(NativeUInt(ptr)),TStringObject.Create(str,size));
  end;

procedure ForgetPtr(const ptr:Pointer);
var idx,linesidx:NativeInt;
    str:string;
begin
  idx:=MallocPointerList.IndexOf(IntToHex(NativeUInt(ptr)));
  if idx>=0 then begin
     str:=TStringObject(MallocPointerList.Objects[idx]).Data;
     linesidx:=MallocLinesList.IndexOf(str);
     // decreate counter
     if (linesidx>=0) then
        if NativeUInt(MallocLinesList.Objects[linesidx])>0 then
           MallocLinesList.Objects[linesidx]:=TObject(NativeUInt(MallocLinesList.Objects[linesidx])-1)
        else
           Inc(ForgetPtrErrors)
     else
        Inc(ForgetPtrErrors);
     MallocPointerList.Delete(idx);
     end
  else
     Inc(ForgetPtrErrors);
  end;

function tgputtylib_debug_malloc_callback(size:NativeUInt;const srcfile:PAnsiChar;const line:Integer):Pointer; cdecl;
begin
  MallocCS.Enter;
  try
    Result:=AllocMem(size);
    RememberPtr(Result,size,srcfile,line);
    finally
      MallocCS.Leave;
    end;
  end;

procedure tgputtylib_debug_free_callback(ptr:Pointer;const srcfile:PAnsiChar;const line:Integer); cdecl;
begin
  MallocCS.Enter;
  try
    ForgetPtr(ptr);
    FreeMem(ptr);
    finally
      MallocCS.Leave;
    end;
  end;

function tgputtylib_debug_realloc_callback(ptr:Pointer; newsize:NativeUInt;const srcfile:PAnsiChar;const line:Integer):Pointer; cdecl;
var idx:NativeInt;
begin
  MallocCS.Enter;
  try
    ForgetPtr(ptr);
    Result:=ptr;
    ReallocMem(Result,newsize);
    RememberPtr(Result,newsize,srcfile,line);
    finally
      MallocCS.Leave;
    end;
  end;

{$i-}
procedure LogLeaks;
var count,i,j:Integer;
    total:NativeUInt;
    T:Text;
begin
  IOResult;
  Assign(T,ParamStr(0)+'.tgputtylib-memlog.txt');
  Rewrite(T);
  if IOResult<>0 then
     Exit;

  if ForgetPtrErrors>0 then
     WriteLn(T,'ForgetPtrErrors: ',ForgetPtrErrors);

  for i:=0 to MallocLinesList.Count-1 do begin
    count:=NativeInt(MallocLinesList.Objects[i]);
    if count>0 then begin
       total:=0;
       for j:=0 to MallocPointerList.Count-1 do
         if TStringObject(MallocPointerList.Objects[j]).Data=MallocLinesList[i] then
            Inc(total,TStringObject(MallocPointerList.Objects[j]).Tag);
       WriteLn(T,count:5,'x ',total:10,' Bytes Total  ',MallocLinesList[i]);
       end;
    end;
  CloseFile(T);
  end;
{$endif}

{ TTGLibraryContext }

procedure TTGLibraryContext.Init;
begin
  timeoutticks:=cDefaultTimeoutTicks;
  connectiontimeoutticks:=cDefaultTimeoutTicks;

  //entercriticalsection_callback:=TGPuttyDLLEnterCriticalSection;
  //leavecriticalsection_callback:=TGPuttyDLLLeaveCriticalSection;

{$ifdef USEMEMORYCALLBACKS}
  if UseMemoryAllocationCallbacks then begin
     malloc_callback:=tgputtylib_malloc_callback;
     free_callback:=tgputtylib_free_callback;
     realloc_callback:=tgputtylib_realloc_callback;
     debug_malloc_callback:=tgputtylib_debug_malloc_callback;
     debug_free_callback:=tgputtylib_debug_free_callback;
     debug_realloc_callback:=tgputtylib_debug_realloc_callback;
     end;

  usememorycallbacks:=UseMemoryAllocationCallbacks;
{$endif}
  end;

{
procedure AllocateCritSects;
var i:Integer;
begin
  for i:=1 to MaxCritSect do
    CritSects[i]:=TCriticalSection.Create;
  end;

procedure FreeCritSects;
var i:Integer;
begin
  for i:=1 to MaxCritSect do
    FreeAndNil(CritSects[i]);
  end;

initialization
  AllocateCritSects;

finalization
  FreeCritSects;
}

{$ifdef USEMEMORYCALLBACKS}
initialization

  if DebugMemory then begin
     MallocLinesList:=TStringList.Create;
     MallocLinesList.Sorted:=true;
     MallocPointerList:=TStringList.Create;
     MallocPointerList.Sorted:=true;
     MallocPointerList.OwnsObjects:=true;
     MallocCS:=TCriticalSection.Create;
     end;

finalization

  if DebugMemory then begin
     LogLeaks;
     FreeAndNil(MallocPointerList);
     FreeAndNil(MallocLinesList);
     FreeAndNil(MallocCS);
     end;
{$endif}

end.



