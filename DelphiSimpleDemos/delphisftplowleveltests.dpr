program delphisftplowleveltests;

{$APPTYPE CONSOLE}

// Test Application for tgputtylib
// not very useful, please use TTGPuttySFTP instead

{$R *.res}

{$POINTERMATH ON}

{.$define USECONSOLEUNIT}

uses
  Classes,
  SysUtils,DateUtils,
  {$ifdef USECONSOLEUNIT}
  Console,
  {$endif }
  tgputtylib in '..\tgputtylib.pas';

const
  RunInteractiveMode=false;
  UseCmdLines=false;

var Params:array[0..0] of PAnsiChar=('-v');
    context1,context2:TTGLibraryContext;

function ls_callback(const names:Pfxp_names;const libctx:PTGLibraryContext):Boolean; cdecl;
var i:Integer;
begin
  for i:=0 to names^.nnames-1 do
    WriteLn(names^.names[i].filename:40,'  ',names^.names[i].attrs.size:10);
  Result:=true;
  end;

var LastPassword:AnsiString;

function getpassword_callback(const prompt:PAnsiChar;const echo:Boolean;const cancel:PBoolean;const libctx:PTGLibraryContext):PAnsiChar; cdecl;
begin
  WriteLn(prompt);
  ReadLn(LastPassword);
  Result:=PAnsiChar(LastPassword);
  cancel^:=false;
  end;

procedure printmessage_callback(const msg:PAnsiChar;const isstderr:Boolean;const libctx:PTGLibraryContext); cdecl;
begin
  if isstderr then
     Write('!! ',msg)
  else
     Write('** ',msg);
  end;

function progress_callback(const bytescopied:Int64;const isupload:Boolean;const libctx:PTGLibraryContext):Boolean; cdecl;
begin
  WriteLn('Copied: ',bytescopied);
  Result:=true;
  {$ifdef USECONSOLEUNIT}
  if KeyPressed and (ReadKey=^[) then
     Result:=false; // ESC = Cancel
  {$endif}
  end;

function get_input_callback(linebuf:PAnsiChar;const maxchars:Integer;const libctx:PTGLibraryContext):Boolean; cdecl;
var line:AnsiString;
begin
  Write('Your Input: ');
  ReadLn(line);
  if Length(line)>maxchars then
     SetLength(line,maxchars);
  if line>'' then begin
     Move(line[1],linebuf^,maxchars);
     linebuf[Length(line)]:=#0;
     end
  else
     linebuf^:=#0;
  Result:=true;
  end;

var FS:TFileStream;

function read_from_stream(const offset:UInt64;const buffer:Pointer;const bufsize:Integer;const libctx:PTGLibraryContext):Integer; cdecl;
begin
  if Assigned(FS) then begin
     FS.Position:=Offset;
     Result:=FS.Read(buffer^,bufsize);
     end
  else
     Result:=0;
  end;

function write_to_stream(const offset:UInt64;const buffer:Pointer;const bufsize:Integer;const libctx:PTGLibraryContext):Integer; cdecl;
begin
  if Assigned(FS) then begin
     FS.Position:=Offset;
     Result:=FS.Write(buffer^,bufsize);
     end
  else
     Result:=0;
  end;

procedure raise_exception_callback(const msg:PAnsiChar;const srcfile:PAnsiChar;const line:Integer;const libctx:PTGLibraryContext); cdecl;
begin
  raise Exception.Create('tgputty exception '+msg+' at line '+IntToStr(line)+' in '+srcfile);
  end;

var res:Integer;
    attrs:fxp_attrs;
    HostName,UserName,Password,Folder,FileToUpload:AnsiString;

begin
  try
    tgputtysetappname('tgdelphisftp','0.10');

    // ExitCode:=psftp_main(1, @Params[0]);

    context1.Init;
    context2.Init;

    context1.structsize:=sizeof(context1);
    if context1.structsize<tggetlibrarycontextsize then
       raise Exception.Create('Incorrect TTGLibraryContext record size');
    context1.ls_callback:=ls_callback;
    context1.getpassword_callback:=getpassword_callback;
    context1.printmessage_callback:=printmessage_callback;
    context1.progress_callback:=progress_callback;
    context1.read_from_stream:=read_from_stream;
    context1.write_to_stream:=write_to_stream;
    context1.get_input_callback:=get_input_callback;
    context1.raise_exception_callback:=raise_exception_callback;

    context2.structsize:=sizeof(context1);
    context2.ls_callback:=ls_callback;
    context2.getpassword_callback:=getpassword_callback;
    context2.printmessage_callback:=printmessage_callback;
    context2.progress_callback:=progress_callback;
    context2.read_from_stream:=read_from_stream;
    context2.write_to_stream:=write_to_stream;
    context2.get_input_callback:=get_input_callback;
    context2.raise_exception_callback:=raise_exception_callback;

    res:=tgputty_initcontext(true, @context1); // TO DO: 1. the param does not work, 2. init with only context1 parameter
    res:=tgputty_initcontext(true, @context2);

    if RunInteractiveMode then begin
       tgputtyrunpsftp(@context1);
       end
    else begin
      Write('Enter Hostname or IP of SFTP Server: ');
      ReadLn(HostName);
      Write('Enter Username: ');
      ReadLn(UserName);
      Write('Enter Password: ');
      ReadLn(Password);
      if UseCmdLines then begin
         tgputtysftpcommand(PAnsiChar(AnsiString('open '+UserName+'@'+HostName)),@context1);
         Write('Enter Folder Path to CD into: ');
         ReadLn(Folder);
         tgputtysftpcommand(PAnsiChar(AnsiString(AnsiString('cd ')+Utf8Encode(Folder))),@context1);
         Write('Enter local File Path to upload: ');
         ReadLn(FileToUpload);
         tgputtysftpcommand(PAnsiChar(AnsiString(AnsiString('put ')+Utf8Encode(FileToUpload))),@context1);
         tgputtysftpcommand('quit',@context1);
         end
      else begin
        try
          res:=tgsftp_connect(PAnsiChar(HostName),PAnsiChar(UserName),22,PAnsiChar(Password),@context1);
          except on E:Exception do WriteLn('EXCEPTION: ',E.Message);
        end;

        if res<>0 then begin
           WriteLn('Connection failed, Exiting. Press ENTER.');
           ReadLn;
           Halt(1);
           end;

        WriteLn('Home Directory: ',context1.homedir);
        WriteLn('Current Directory: ',context1.pwd);

        Write('Enter Folder Path to CD into: ');
        ReadLn(Folder);

        try
          tgsftp_cd(PAnsiChar(Utf8Encode(Folder)),@context1);
          except on E:Exception do WriteLn('EXCEPTION: ',E.Message);
          end;

        try
          tgsftp_ls('.',@context1);
            except on E:Exception do WriteLn('EXCEPTION: ',E.Message);
          end;

        Write('Enter local File Path to upload: ');
        ReadLn(FileToUpload);

        try
          tgsftp_putfile(PAnsiChar(Utf8Encode(FileToUpload)), PAnsiChar(Utf8Encode(ExtractFileName(FileToUpload))),false,@context1);
          except on E:Exception do WriteLn('EXCEPTION: ',E.Message);
          end;

        try
          tgsftp_ls('.',@context1);
          except on E:Exception do WriteLn('EXCEPTION: ',E.Message);
          end;

        if tgsftp_getstat(PAnsiChar(Utf8Encode(ExtractFileName(FileToUpload))),@attrs,@context1) then
           WriteLn('Got Stat for '+ExtractFileName(FileToUpload)+': size ',attrs.size,
                   ', mtime: ',DateTimeToStr(UnixToDateTime(attrs.mtime)),' UTC')
        else
           WriteLn('Failed to get stat');

        WriteLn('Setting timestamp to 03.10.2019');
        attrs.mtime := DateTimeToUnix(StrToDate('03.10.2019'));
        attrs.flags := SSH_FILEXFER_ATTR_ACMODTIME; // set only this
        if tgsftp_setstat(PAnsiChar(Utf8Encode(ExtractFileName(FileToUpload))),@attrs,@context1) then
           WriteLn('Set Stat OK.')
        else
           WriteLn('Failed to set stat.');

        if tgsftp_getstat(PAnsiChar(Utf8Encode(ExtractFileName(FileToUpload))),@attrs,@context1) then
           WriteLn('Got Stat for '+ExtractFileName(FileToUpload)+': size ',attrs.size,
                   ', mtime: ',DateTimeToStr(UnixToDateTime(attrs.mtime)),' UTC')
        else
           WriteLn('Failed to get stat.');

        {
        more examples:

        WriteLn('About to download '+ExtractFileName(FileToUpload)+' ... press ENTER.');
        ReadLn;
        tgsftp_getfile(Utf8Encode(ExtractFileName(FileToUpload)),FileToUpload+'.downloaded',false,@context1);
        tgsftp_mkdir('blabla',@context1);
        tgsftp_ls('/share/CACHEDEV1_DATA/Public',@context1);

        WriteLn('About to upload stream ... balenaEtcher-Setup-1.5.51.exe ... press ENTER.');
        ReadLn;
        FS:=TFileStream.Create('D:\Downloads\balenaEtcher-Setup-1.5.51.exe',fmOpenRead);
        try
          tgsftp_putfile(nil, 'etcher.exe',false,@context1);
          finally
            FreeAndNil(FS);
          end;

        WriteLn('About to download stream ... press ENTER.');
        ReadLn;
        FS:=TFileStream.Create('D:\Downloads\Stream.exe',fmCreate);
        try
          tgsftp_getfile('etcher.exe',nil,false,@context1);
          finally
            FreeAndNil(FS);
          end;

        WriteLn('Proceed? ... press ENTER.');
        ReadLn;

        tgsftp_mv('test.iso','blabla',@context1);
        tgsftp_cd('/share/CACHEDEV1_DATA/Public/blabla',@context1);
        WriteLn('Now removing uploaded test.iso file.');
        tgsftp_rm('test.iso',@context1);
        tgsftp_cd('/share/CACHEDEV1_DATA/Public',@context1);
        tgsftp_rmdir('blabla',@context1);

        WriteLn('Also connect to 192.168.178.40? ... press ENTER.');
        ReadLn;

        tgsftp_connect('192.168.178.40','admin',22,'',@context2);
        tgsftp_cd('/share/CACHEDEV1_DATA/MainData',@context2);
        tgsftp_ls('/share/CACHEDEV1_DATA/MainData',@context2);

        tgsftp_cd('/share/CACHEDEV1_DATA/MainData',@context1);
        tgsftp_ls('/share/CACHEDEV1_DATA/MainData',@context1);

        tgsftp_putfile('D:\Downloads\CoreTemp64.zip','CoreTemp64.zip',false,@context1);
        tgsftp_putfile('D:\Downloads\domainhostingview.zip','domainhostingview.zip',false,@context2);
        tgsftp_getfile('CoreTemp64.zip','D:\Tests\CoreTemp64.zip',false,@context1);
        }
        tgsftp_close(@context1);
        {
        tgsftp_close(@context2);
        }
        end;

      end;
    except
      on E:Exception do
         WriteLn('EXCEPTION: ',E.Message);
    end;

  tgputtyfree(@context1);
  tgputtyfree(@context2);

  WriteLn('delphisftplowleveltests are done ... press ENTER.');
  ReadLn;
  end.
