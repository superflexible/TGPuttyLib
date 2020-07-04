program fpcsftpclasstest;

{$APPTYPE CONSOLE}
{$MODE Delphi}

// Test Application for tgputtylib
// not very useful, please use TTGPuttySFTP instead

{$POINTERMATH ON}

uses
  Classes,
  SysUtils,
  DateUtils,
  tgputtylib in '..\tgputtylib.pas',
  tgputtysftp in '..\tgputtysftp.pas';

type TTester=class(TObject)
      TGPSFTP,TGPSFTP2:TTGPuttySFTP;
      HostName,UserName,Password,Folder,FileToUpload:AnsiString;
      procedure Go;
      function ListingCallback(const names:Pfxp_names):Boolean;
      procedure MessageCallback(const Msg:AnsiString;const isstderr:Boolean);
      function ProgressCallback(const bytescopied:Int64;const isupload:Boolean):Boolean;
      function VerifyHostKey(const host:PAnsiChar;const port:Integer;
                             const fingerprint:PAnsiChar;
                             const verificationstatus:Integer;
                             var storehostkey:Boolean):Boolean;
      end;

function TTester.ListingCallback(const names:Pfxp_names):Boolean;
var i:Integer;
begin
  for i:=0 to names^.nnames-1 do
    WriteLn(names^.names[i].filename:40,'  ',names^.names[i].attrs.size:10);
  Result:=true;
  end;

procedure TTester.MessageCallback(const Msg: AnsiString;const isstderr: Boolean);
begin
  Write(Msg);
  end;

function TTester.ProgressCallback(const bytescopied: Int64;const isupload: Boolean): Boolean;
begin
  WriteLn('Copied ',bytescopied,' Bytes');
  Result:=true;
  end;

function TTester.VerifyHostKey(const host: PAnsiChar; const port: Integer;
                               const fingerprint: PAnsiChar; const verificationstatus: Integer;
                               var storehostkey: Boolean): Boolean;
begin
  Result:=true;
  storehostkey:=true;
  end;

procedure TTester.Go;
var attrs:fxp_attrs;
begin
  tgputtysetappname('tgdelphisftp','0.10');

  TGPSFTP:=TTGPuttySFTP.Create(true);
  try
     try
       TGPSFTP.OnListing:=ListingCallback;
       TGPSFTP.OnMessage:=MessageCallback;
       TGPSFTP.OnProgress:=ProgressCallback;
       TGPSFTP.OnVerifyHostKey:=VerifyHostKey;

       Write('Enter Hostname or IP of SFTP Server: ');
       ReadLn(HostName);
       if HostName='' then begin
          WriteLn('Exiting ...');
          Exit;
          end;
       Write('Enter Username: ');
       ReadLn(UserName);
       Write('Enter Password: ');
       ReadLn(Password);

       TGPSFTP.HostName:=HostName;
       TGPSFTP.UserName:=UserName;
       TGPSFTP.Password:=Password;

       try
         TGPSFTP.Connect;
         except on E:Exception do WriteLn('EXCEPTION: ',E.Message);
         end;
       WriteLn('Home Directory: ',TGPSFTP.HomeDir);
       WriteLn('Current Directory: ',TGPSFTP.WorkDir);
       Write('Enter Folder Path to CD into: ');
       ReadLn(Folder);
       try
         if Folder<>'' then
            TGPSFTP.ChangeDir(Utf8Encode(Folder));
         except on E:Exception do WriteLn('EXCEPTION: ',E.Message);
         end;
       WriteLn('Current Directory: ',TGPSFTP.WorkDir);

       try
         TGPSFTP.ListDir('.');
         except on E:Exception do WriteLn('EXCEPTION: ',E.Message);
         end;

       Write('Enter local File Path to upload: ');
       ReadLn(FileToUpload);
       try
         TGPSFTP.UploadFile(FileToUpload, Utf8Encode(ExtractFileName(FileToUpload)),false);
         except on E:Exception do WriteLn('EXCEPTION: ',E.Message);
         end;

       try
         TGPSFTP.ListDir('.');
         except on E:Exception do WriteLn('EXCEPTION: ',E.Message);
         end;

       try
         TGPSFTP.GetStat(Utf8Encode(ExtractFileName(FileToUpload)),attrs);
         WriteLn('Got Stat for '+ExtractFileName(FileToUpload)+': size ',attrs.size,
                 ', mtime:',DateTimeToStr(UnixToDateTime(attrs.mtime)),' UTC')
         except
            on E:Exception do WriteLn('Failed to get Stat, EXCEPTION: ',E.Message);
         end;

       WriteLn('Setting timestamp to 03.10.2019');
       attrs.mtime := DateTimeToUnix(StrToDate('03.10.2019'));
       attrs.flags := SSH_FILEXFER_ATTR_ACMODTIME; // set only this
       try
         TGPSFTP.SetStat(Utf8Encode(ExtractFileName(FileToUpload)),attrs);
         WriteLn('Set Stat.');
         except
            on E:Exception do WriteLn('Failed to set Stat, EXCEPTION: ',E.Message);
         end;

       try
         TGPSFTP.GetStat(Utf8Encode(ExtractFileName(FileToUpload)),attrs);
         WriteLn('Got Stat: size ',attrs.size,', mtime:',DateTimeToStr(UnixToDateTime(attrs.mtime)),' UTC')
         except
            on E:Exception do WriteLn('Failed to get Stat, EXCEPTION: ',E.Message);
         end;


       WriteLn('About to download the file again ... press ENTER.');
       ReadLn;

       TGPSFTP.DownloadFile(Utf8Encode(ExtractFileName(FileToUpload)),FileToUpload+'.downloaded',false);


       WriteLn('Making directory blabla ... press ENTER.');
       ReadLn;
       try
         TGPSFTP.MakeDir('blabla');
         except
            on E:Exception do WriteLn('MkDir EXCEPTION: ',E.Message);
         end;

       TGPSFTP.ListDir('.');

       {
       more examples:

       WriteLn('About to upload stream ... balenaEtcher-Setup-1.5.51.exe ... press ENTER.');
       ReadLn;
       FS:=TFileStream.Create('D:\Downloads\balenaEtcher-Setup-1.5.51.exe',fmOpenRead);
       try
         TGPSFTP.UploadStream('etcher.exe',FS,false);
         finally
           FreeAndNil(FS);
         end;

       WriteLn('About to download stream ... press ENTER.');
       ReadLn;
       FS:=TFileStream.Create('D:\Downloads\Stream.exe',fmCreate);
       try
         TGPSFTP.DownloadStream('etcher.exe',FS,false);
         finally
           FreeAndNil(FS);
         end;

       WriteLn('Proceed? ... press ENTER.');
       ReadLn;

       try
         TGPSFTP.Move('test.iso','blabla');
         except on E:Exception do WriteLn('EXCEPTION: ',E.Message);
         end;

       try
         TGPSFTP.ChangeDir('/share/CACHEDEV1_DATA/Public/blabla');
         except on E:Exception do WriteLn('EXCEPTION: ',E.Message);
         end;

       WriteLn('Now removing uploaded test.iso file.');
       try
         TGPSFTP.DeleteFile('test.iso');
         except on E:Exception do WriteLn('EXCEPTION: ',E.Message);
         end;

       try
         TGPSFTP.ChangeDir('/share/CACHEDEV1_DATA/Public');
         except on E:Exception do WriteLn('EXCEPTION: ',E.Message);
         end;

       try
         TGPSFTP.RemoveDir('blabla');
         except on E:Exception do WriteLn('EXCEPTION: ',E.Message);
         end;

       WriteLn('Also connect to 192.168.178.40? ... press ENTER.');
       ReadLn;

       TGPSFTP2:=TTGPuttySFTP.Create(true);
       try
          TGPSFTP2.OnListing:=ListingCallback;
          TGPSFTP2.OnMessage:=MessageCallback;
          TGPSFTP2.OnProgress:=ProgressCallback;

          TGPSFTP2.HostName:='192.168.178.40';
          TGPSFTP2.UserName:='admin';
          TGPSFTP2.Password:='';

          TGPSFTP2.Connect;

          TGPSFTP2.ChangeDir('/share/CACHEDEV1_DATA/MainData');
          TGPSFTP2.ListDir('/share/CACHEDEV1_DATA/MainData');

          TGPSFTP2.UploadFile('D:\Downloads\CoreTemp64.zip','CoreTemp64.zip',false);
          TGPSFTP2.UploadFile('D:\Downloads\domainhostingview.zip','domainhostingview.zip',false);
          TGPSFTP2.DownloadFile('CoreTemp64.zip','D:\Tests\CoreTemp64.zip',false);
          finally
            FreeAndNil(TGPSFTP2);
          end;
      }
      TGPSFTP.Disconnect;
      finally
        FreeAndNil(TGPSFTP);
      end;

    except
      on E:Exception do
         WriteLn('EXCEPTION: ',E.Message);
    end;
  end;

var Tester:TTester;

begin
  Tester:=TTester.Create;
  try
    Tester.Go;
    finally
      FreeAndNil(Tester);
    end;
  WriteLn('Testing is done ... press ENTER.');
  ReadLn;
  end.

