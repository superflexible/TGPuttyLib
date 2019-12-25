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

// run the whole psftp interactive commmand prompt
// includes init and cleanup, only one function call needed
function psftp_main(argcparam: Longint; argvparam: ppchar):Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};

// basic functions
function tggetlibrarycontextsize:Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
function tgputty_initwithcmdline(argcparam: Longint; argvparam: ppchar; const libctx:PTGLibraryContext):Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
function tgputty_initcontext(const verbose:Boolean;const libctx:PTGLibraryContext):Integer; cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
procedure tgputtysetappname(const newappname,appversion:PAnsiChar); cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
procedure tgputty_setverbose(const averbose:Boolean); cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
procedure tgputtyfree(const libctx:PTGLibraryContext); cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif};
procedure tgputtygetversions(puttyrelease:PDouble; tgputtylibbuild:PInteger); cdecl; external tgputtydll {$ifdef HASDELAYED}delayed{$endif}; // TG 2019

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



