unit VCLClientMainForm;

interface

{$POINTERMATH ON}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.FileCtrl, Vcl.Grids,
  DateUtils,IOUtils,Registry,
  tgputtylib,tgputtysftp, Vcl.ComCtrls;

type
  TVCLSFTPClientDemoForm = class(TForm)
    DirectoryListBox1: TDirectoryListBox;
    DriveComboBox1: TDriveComboBox;
    FileListBox1: TFileListBox;
    Label1: TLabel;
    Label2: TLabel;
    edURL: TEdit;
    Label3: TLabel;
    edPort: TEdit;
    Label4: TLabel;
    edUserName: TEdit;
    Label5: TLabel;
    edPassword: TEdit;
    Label6: TLabel;
    edFolderPath: TEdit;
    Label7: TLabel;
    sgRemoteFiles: TStringGrid;
    Label8: TLabel;
    btConnect: TButton;
    btDisconnect: TButton;
    Label9: TLabel;
    cbVerbose: TCheckBox;
    memLog: TMemo;
    btUpload: TButton;
    btDownload: TButton;
    cbSavePassword: TCheckBox;
    ProgressBar1: TProgressBar;
    btDeleteLocal: TButton;
    btDeleteRemote: TButton;
    btMkDir: TButton;
    btRemoveDir: TButton;
    btMove: TButton;
    FileOpenDialog1: TFileOpenDialog;
    edKeyFile: TEdit;
    Label10: TLabel;
    btnSelectKeyFile: TButton;
    procedure btConnectClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    function ListingCallback(const names:Pfxp_names):Boolean;
    procedure MessageCallback(const Msg:AnsiString;const isstderr:Boolean);
    function ProgressCallback(const bytescopied:Int64;const isupload:Boolean):Boolean;
    function GetInputCallback(var cancel:Boolean):AnsiString;
    function VerifyHostKeyCallback(const host:PAnsiChar;const port:Integer;const fingerprint:PAnsiChar;const verificationstatus:Integer;var storehostkey:Boolean):Boolean;
    procedure btDisconnectClick(Sender: TObject);
    procedure sgRemoteFilesDblClick(Sender: TObject);
    procedure btDownloadClick(Sender: TObject);
    procedure btUploadClick(Sender: TObject);
    procedure edFolderPathExit(Sender: TObject);
    procedure btDeleteLocalClick(Sender: TObject);
    procedure btDeleteRemoteClick(Sender: TObject);
    procedure DirectoryListBox1Change(Sender: TObject);
    procedure btMkDirClick(Sender: TObject);
    procedure btRemoveDirClick(Sender: TObject);
    procedure btMoveClick(Sender: TObject);
    procedure btnSelectKeyFileClick(Sender: TObject);
    procedure cbVerboseClick(Sender: TObject);
  private
    { Private declarations }
    PSFTP:TTGPuttySFTP;
    FTotalToCopy:Int64;
    FInLoadSettings:Boolean;
    procedure SaveSettings;
    procedure LoadSettings;
    procedure GetListing;
  public
    { Public declarations }
  end;

var
  VCLSFTPClientDemoForm: TVCLSFTPClientDemoForm;

implementation

{$R *.dfm}

procedure TVCLSFTPClientDemoForm.btConnectClick(Sender: TObject);
begin
  SaveSettings;
  with PSFTP do begin
    HostName:=Utf8Encode(edURL.Text);
    UserName:=Utf8Encode(edUserName.Text);
    if edKeyFile.Text='' then
       Password:=Utf8Encode(edPassword.Text)
    else begin
       Keyfile:=Utf8Encode(edKeyFile.Text);
       KeyPassword:=Utf8Encode(edPassword.Text);
       end;
    Port:=StrToIntDef(edPort.Text,22);
    Connect;

    if edFolderPath.Text<>'' then
       ChangeDir(Utf8Encode(edFolderPath.Text));

    edFolderPath.Text:=Utf8ToString(WorkDir);
    end;

  GetListing;
  end;

procedure TVCLSFTPClientDemoForm.btDeleteLocalClick(Sender: TObject);
var i,count:Integer;
    APath:string;
begin
  count:=0;
  for i:=0 to FileListBox1.Count-1 do
    if FileListBox1.Selected[i] then
       Inc(count);

  if count=0 then
     Exit;

  if Application.MessageBox(PWideChar('Please confirm deleting '+IntToStr(count)+' file locally (left side)'),
                            'Confirm Deletion',
                            MB_YESNO or MB_ICONQUESTION)=IDYES then begin
     for i:=0 to FileListBox1.Count-1 do
       if FileListBox1.Selected[i] then begin
          APath:=DirectoryListBox1.Directory+PathDelim+FileListBox1.Items[i];
          DeleteFile(APath);
          end;
     FileListBox1.Update;
     end;
  end;

procedure TVCLSFTPClientDemoForm.btDisconnectClick(Sender: TObject);
begin
  PSFTP.Disconnect;
  sgRemoteFiles.RowCount:=0;
  sgRemoteFiles.ColCount:=0;
  end;

procedure TVCLSFTPClientDemoForm.btDownloadClick(Sender: TObject);
var i:Integer;
    DownloadStream:TStream;
    APath:UnicodeString;
begin
  for i:=sgRemoteFiles.Selection.Top to sgRemoteFiles.Selection.Bottom do begin
     if sgRemoteFiles.Cells[2,i]<>'<dir>' then begin
        APath:=DirectoryListBox1.Directory+PathDelim+sgRemoteFiles.Cells[0,i];
        DownloadStream:=TFileStream.Create(APath,fmCreate);
        try
          FTotalToCopy:=StrToInt64Def(sgRemoteFiles.Cells[2,i],0);
          ProgressBar1.Min:=0;
          ProgressBar1.Max:=FTotalToCopy div 1024;
          ProgressBar1.Position:=0;
          ProgressBar1.Visible:=true;
          Application.ProcessMessages;
          PSFTP.DownloadStream(Utf8Encode(sgRemoteFiles.Cells[0,i]),
                               DownloadStream,
                               false);
          FileListBox1.Update;
          finally
            ProgressBar1.Visible:=false;
            FreeAndNil(DownloadStream);
          end;
        end;
     end;
  end;

procedure TVCLSFTPClientDemoForm.btMkDirClick(Sender: TObject);
var AName:string;
begin
  AName:=InputBox('Make Directory','Enter new Directory Name:','');
  if AName<>'' then begin
     PSFTP.MakeDir(Utf8Encode(AName));
     GetListing;
     end;
  end;

procedure TVCLSFTPClientDemoForm.btMoveClick(Sender: TObject);
var count,i:Integer;
    AName:string;
begin
  AName:=InputBox('Renaem / Move','Enter new name and/or destination directory:','');
  if AName<>'' then begin
     count:=sgRemoteFiles.Selection.Bottom-sgRemoteFiles.Selection.Top+1;
     if count=0 then
        Exit;

     if Application.MessageBox(PWideChar('Please confirm moving '+IntToStr(count)+' item(s) remotely (right side)'),
                               'Confirm Moving',
                               MB_YESNO or MB_ICONQUESTION)=IDYES then begin
        for i:=sgRemoteFiles.Selection.Top to sgRemoteFiles.Selection.Bottom do
           if sgRemoteFiles.Cells[2,i]<>'<dir>' then
              PSFTP.Move(Utf8Encode(sgRemoteFiles.Cells[0,i]),Utf8Encode(AName));
        end;
     GetListing;
     end;
  end;

procedure TVCLSFTPClientDemoForm.btUploadClick(Sender: TObject);
var i:Integer;
    APath:string;
    UploadStream:TStream;
    LDateTime:TDateTimeInfoRec;
begin
  for i:=0 to FileListBox1.Count-1 do
    if FileListBox1.Selected[i] then begin
       APath:=DirectoryListBox1.Directory+PathDelim+FileListBox1.Items[i];
       FileGetDateTimeInfo(APath,LDateTime);
       UploadStream:=TFileStream.Create(APath,fmOpenRead);
       try
         FTotalToCopy:=UploadStream.Size;
         ProgressBar1.Min:=0;
         ProgressBar1.Max:=FTotalToCopy div 1024;
         ProgressBar1.Position:=0;
         ProgressBar1.Visible:=true;
         Application.ProcessMessages;
         PSFTP.UploadStream(Utf8Encode(FileListBox1.Items[i]),UploadStream,false);
         PSFTP.SetModifiedDate(Utf8Encode(FileListBox1.Items[i]),LDateTime.TimeStamp,false);
         GetListing;
         finally
           FreeAndNil(UploadStream);
           ProgressBar1.Visible:=false;
         end;
       end;
  end;

procedure TVCLSFTPClientDemoForm.cbVerboseClick(Sender: TObject);
begin
  PSFTP.Verbose:=cbVerbose.Checked;
  end;

procedure TVCLSFTPClientDemoForm.btRemoveDirClick(Sender: TObject);
var count,i:Integer;
begin
  count:=sgRemoteFiles.Selection.Bottom-sgRemoteFiles.Selection.Top+1;
  if count=0 then
     Exit;

  if Application.MessageBox(PWideChar('Please confirm deleting '+IntToStr(count)+' directory remotely (right side)'),
                            'Confirm Deletion',
                            MB_YESNO or MB_ICONQUESTION)=IDYES then begin
     for i:=sgRemoteFiles.Selection.Top to sgRemoteFiles.Selection.Bottom do
        if sgRemoteFiles.Cells[2,i]='<dir>' then
           PSFTP.RemoveDir(Utf8Encode(sgRemoteFiles.Cells[0,i]));
     end;
  GetListing;
  end;

procedure TVCLSFTPClientDemoForm.DirectoryListBox1Change(Sender: TObject);
begin
  if not FInLoadSettings then
     SaveSettings;
  end;

procedure TVCLSFTPClientDemoForm.btDeleteRemoteClick(Sender: TObject);
var count,i:Integer;
begin
  count:=sgRemoteFiles.Selection.Bottom-sgRemoteFiles.Selection.Top+1;
  if count=0 then
     Exit;

  if Application.MessageBox(PWideChar('Please confirm deleting '+IntToStr(count)+' file(s) remotely (right side)'),
                            'Confirm Deletion',
                            MB_YESNO or MB_ICONQUESTION)=IDYES then begin
     for i:=sgRemoteFiles.Selection.Top to sgRemoteFiles.Selection.Bottom do
        if sgRemoteFiles.Cells[2,i]<>'<dir>' then
           PSFTP.DeleteFile(Utf8Encode(sgRemoteFiles.Cells[0,i]));
     end;
  GetListing;
  end;

procedure TVCLSFTPClientDemoForm.btnSelectKeyFileClick(Sender: TObject);
begin
  if FileOpenDialog1.Execute then begin
    edKeyFile.Text := FileOpenDialog1.FileName;
  end;
end;

procedure TVCLSFTPClientDemoForm.edFolderPathExit(Sender: TObject);
begin
  if PSFTP.Connected then begin
     try
       if edFolderPath.Text<>'' then
          PSFTP.ChangeDir(Utf8Encode(edFolderPath.Text));

       edFolderPath.Text:=Utf8ToString(PSFTP.WorkDir);
       GetListing;
       except
         on E:Exception do
            Application.MessageBox(PWideChar(E.Message),'Error');
       end;
     end;
  end;

procedure TVCLSFTPClientDemoForm.FormCreate(Sender: TObject);
begin
  PSFTP:=TTGPuttySFTP.Create(true);
  PSFTP.OnListing:=ListingCallback;
  PSFTP.OnMessage:=MessageCallback;
  PSFTP.OnProgress:=ProgressCallback;
  PSFTP.OnGetInput:=GetInputCallback;
  PSFTP.OnVerifyHostKey:=VerifyHostKeyCallback;
  end;

procedure TVCLSFTPClientDemoForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(PSFTP);
  end;

procedure TVCLSFTPClientDemoForm.FormShow(Sender: TObject);
begin
  LoadSettings;
  ProgressBar1.Visible:=false;
  memLog.Lines.Add('Library version: '+string(PSFTP.LibVersion));
  end;

function TVCLSFTPClientDemoForm.GetInputCallback(var cancel: Boolean): AnsiString;
begin
  Result:=''; // this event will not normally fire
  cancel:=false;
  memLog.Lines.Add('Replying with empty line.');
  end;

procedure TVCLSFTPClientDemoForm.GetListing;
begin
  sgRemoteFiles.RowCount:=1;
  sgRemoteFiles.ColCount:=3;
  sgRemoteFiles.ColWidths[0]:=480;
  sgRemoteFiles.ColWidths[1]:=300;
  sgRemoteFiles.ColWidths[2]:=150;
  sgRemoteFiles.Cells[0,0]:='Name';
  sgRemoteFiles.Cells[1,0]:='Timestamp';
  sgRemoteFiles.Cells[2,0]:='Size';
  PSFTP.ListDir('');
  if sgRemoteFiles.RowCount>1 then
     sgRemoteFiles.FixedRows:=1;
  sgRemoteFiles.FixedCols:=0;
  end;

function TVCLSFTPClientDemoForm.ListingCallback(const names: Pfxp_names): Boolean;
var StartRow,i:Integer;
begin
  StartRow:=sgRemoteFiles.RowCount;
  sgRemoteFiles.RowCount:=StartRow+names.nnames;
  for i:=0 to names.nnames-1 do begin
    sgRemoteFiles.Cells[0,StartRow+i]:=Utf8ToString(names.names[i].filename);
    sgRemoteFiles.Cells[1,StartRow+i]:=DateTimeToStr(TTimeZone.Local.ToLocalTime(UnixToDateTime(names.names[i].attrs.mtime)));
    if names.names[i].attrs.permissions and $F000 = $4000 then
       sgRemoteFiles.Cells[2,StartRow+i]:='<dir>'
    else
       sgRemoteFiles.Cells[2,StartRow+i]:=IntToStr(names.names[i].attrs.size);
    end;
  Result:=true;
  end;

procedure TVCLSFTPClientDemoForm.LoadSettings;
var Reg:TRegistry;
begin
  Reg:=TRegistry.Create;
  FInLoadSettings:=true;
  try
    with Reg do begin
      RootKey:=HKEY_CURRENT_USER;
      if OpenKey('SOFWARE\tgputty',true) then begin
         edURL.Text:=ReadString('URL');
         edUserName.Text:=ReadString('UserName');
         edPassword.Text:=ReadString('Password');
         cbSavePassword.Checked:=edPassword.Text<>'';
         try
           edPort.Text:=IntToStr(ReadInteger('Port'));
           except
             edPort.Text:='22';
           end;
         edFolderPath.Text:=ReadString('FolderPath');
         DirectoryListBox1.Directory:=ReadString('LocalPath');
         edKeyFile.Text:=ReadString('PrivateKey');
         end;
      end;
    finally
      FreeAndNil(Reg);
      FInLoadSettings:=false;
    end;
  end;

procedure TVCLSFTPClientDemoForm.MessageCallback(const Msg: AnsiString;const isstderr: Boolean);
begin
  memLog.Lines.Add(Utf8ToString(Msg));
  end;

function TVCLSFTPClientDemoForm.ProgressCallback(const bytescopied: Int64;const isupload: Boolean): Boolean;
begin
  ProgressBar1.Position:=bytescopied div 1024;
  Application.ProcessMessages;
  Result:=true;
  end;

procedure TVCLSFTPClientDemoForm.SaveSettings;
var Reg:TRegistry;
begin
  Reg:=TRegistry.Create;
  try
    with Reg do begin
      RootKey:=HKEY_CURRENT_USER;
      if OpenKey('SOFWARE\tgputty',true) then begin
         WriteString('URL',edURL.Text);
         WriteString('UserName',edUserName.Text);
         if cbSavePassword.Checked then
            WriteString('Password',edPassword.Text)
         else
            DeleteValue('Password');
         WriteInteger('Port',StrToIntDef(edPort.Text,22));
         WriteString('FolderPath',edFolderPath.Text);
         WriteString('LocalPath',DirectoryListBox1.Directory);
         WriteString('PrivateKey', edKeyFile.Text);
         end;
      end;
    finally
      FreeAndNil(Reg);
    end;
  end;

procedure TVCLSFTPClientDemoForm.sgRemoteFilesDblClick(Sender: TObject);
begin
  if sgRemoteFiles.Selection.Top=sgRemoteFiles.Selection.Bottom then begin
     if sgRemoteFiles.Cells[2,sgRemoteFiles.Selection.Top]='<dir>' then begin
        PSFTP.ChangeDir(Utf8Encode(sgRemoteFiles.Cells[0,sgRemoteFiles.Selection.Top]));
        edFolderPath.Text:=Utf8ToString(PSFTP.WorkDir);
        SaveSettings;
        GetListing;
        end;
     end;
  end;

function TVCLSFTPClientDemoForm.VerifyHostKeyCallback(const host: PAnsiChar; const port: Integer;
                                                      const fingerprint: PAnsiChar;
                                                      const verificationstatus:Integer;
                                                      var storehostkey: Boolean): Boolean;
begin
  if verificationstatus=0 then begin
     Result:=true;
     Exit;
     end;

  Result:=Application.MessageBox(PWideChar(WideString(
                'Please confirm the SSH host key fingerprint for '+Utf8ToString(AnsiString(host))+
                ', port '+IntToStr(port)+':'+sLineBreak+
                Utf8ToString(AnsiString(fingerprint)))),
                'Server Verification',
                MB_YESNO or MB_ICONQUESTION) = IDYES;
  storehostkey:=Result;
  end;

end.
