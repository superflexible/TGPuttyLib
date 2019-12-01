unit VCLSFTPClientComponentMainForm;

interface

{$POINTERMATH ON}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.FileCtrl, Vcl.Grids, Vcl.ComCtrls,
  DateUtils, IOUtils, Registry,
  tgputtylib, tgputtysftp, tgputtysftpclient;

type
  TVCLSFTPClientComponentDemoForm = class(TForm)
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
    TGPuttySFTPClient1: TTGPuttySFTPClient;
    procedure btConnectClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
    procedure cbVerboseClick(Sender: TObject);
    function TGPuttySFTPClient1SFTPGetInput(Sender: TObject; var cancel: Boolean): string;
    function TGPuttySFTPClient1SFTPListing(Sender: TObject; const Items: TSFTPItems): Boolean;
    procedure TGPuttySFTPClient1SFTPMessage(Sender: TObject; const Msg: string; const isstderr: Boolean);
    function TGPuttySFTPClient1SFTPProgress(Sender: TObject; const bytescopied: Int64; const isupload: Boolean): Boolean;
    function TGPuttySFTPClient1SFTPVerifyHostKey(Sender: TObject; const host: string; const port: Integer;
                                                 const fingerprint: string; const verificationstatus: Integer;
                                                 var storehostkey: Boolean): Boolean;
  private
    { Private declarations }
    FTotalToCopy:Int64;
    FInLoadSettings:Boolean;
    procedure SaveSettings;
    procedure LoadSettings;
    procedure GetListing;
  public
    { Public declarations }
  end;

var
  VCLSFTPClientComponentDemoForm: TVCLSFTPClientComponentDemoForm;

implementation

{$R *.dfm}

procedure TVCLSFTPClientComponentDemoForm.btConnectClick(Sender: TObject);
begin
  SaveSettings;
  with TGPuttySFTPClient1 do begin
    HostName:=edURL.Text;
    UserName:=edUserName.Text;
    Password:=edPassword.Text;
    Port:=StrToIntDef(edPort.Text,22);
    Connect;

    if edFolderPath.Text<>'' then
       ChangeDir(edFolderPath.Text);

    edFolderPath.Text:=WorkDir;
    end;

  GetListing;
  end;

procedure TVCLSFTPClientComponentDemoForm.btDeleteLocalClick(Sender: TObject);
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

procedure TVCLSFTPClientComponentDemoForm.btDisconnectClick(Sender: TObject);
begin
  TGPuttySFTPClient1.Disconnect;
  sgRemoteFiles.RowCount:=0;
  sgRemoteFiles.ColCount:=0;
  end;

procedure TVCLSFTPClientComponentDemoForm.btDownloadClick(Sender: TObject);
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
          TGPuttySFTPClient1.DownloadStream(sgRemoteFiles.Cells[0,i],
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

procedure TVCLSFTPClientComponentDemoForm.btMkDirClick(Sender: TObject);
var AName:string;
begin
  AName:=InputBox('Make Directory','Enter new Directory Name:','');
  if AName<>'' then begin
     TGPuttySFTPClient1.MakeDir(AName);
     GetListing;
     end;
  end;

procedure TVCLSFTPClientComponentDemoForm.btMoveClick(Sender: TObject);
var count,i:Integer;
    AName:string;
begin
  AName:=InputBox('Rename / Move','Enter new name and/or destination directory:','');
  if AName<>'' then begin
     count:=sgRemoteFiles.Selection.Bottom-sgRemoteFiles.Selection.Top+1;
     if count=0 then
        Exit;

     if Application.MessageBox(PWideChar('Please confirm moving '+IntToStr(count)+' file(s) remotely (right side)'),
                               'Confirm Moving',
                               MB_YESNO or MB_ICONQUESTION)=IDYES then begin
        for i:=sgRemoteFiles.Selection.Top to sgRemoteFiles.Selection.Bottom do
           if sgRemoteFiles.Cells[2,i]<>'<dir>' then
              TGPuttySFTPClient1.Move(sgRemoteFiles.Cells[0,i],AName);
        end;
     GetListing;
     end;
  end;

procedure TVCLSFTPClientComponentDemoForm.btUploadClick(Sender: TObject);
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
         TGPuttySFTPClient1.UploadStream(FileListBox1.Items[i],UploadStream,false);
         TGPuttySFTPClient1.SetModifiedDate(FileListBox1.Items[i],LDateTime.TimeStamp,false);
         GetListing;
         finally
           FreeAndNil(UploadStream);
           ProgressBar1.Visible:=false;
         end;
       end;
  end;

procedure TVCLSFTPClientComponentDemoForm.cbVerboseClick(Sender: TObject);
begin
  TGPuttySFTPClient1.Verbose:=cbVerbose.Checked;
  end;

procedure TVCLSFTPClientComponentDemoForm.btRemoveDirClick(Sender: TObject);
var count,i:Integer;
begin
  count:=sgRemoteFiles.Selection.Bottom-sgRemoteFiles.Selection.Top+1;
  if count=0 then
     Exit;

  if Application.MessageBox(PWideChar('Please confirm deleting '+IntToStr(count)+' directory/ies remotely (right side)'),
                            'Confirm Deletion',
                            MB_YESNO or MB_ICONQUESTION)=IDYES then begin
     for i:=sgRemoteFiles.Selection.Top to sgRemoteFiles.Selection.Bottom do
        if sgRemoteFiles.Cells[2,i]='<dir>' then
           TGPuttySFTPClient1.RemoveDir(sgRemoteFiles.Cells[0,i]);
     end;
  GetListing;
  end;

procedure TVCLSFTPClientComponentDemoForm.DirectoryListBox1Change(Sender: TObject);
begin
  if not FInLoadSettings then
     SaveSettings;
  end;

procedure TVCLSFTPClientComponentDemoForm.btDeleteRemoteClick(Sender: TObject);
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
           TGPuttySFTPClient1.DeleteFile(sgRemoteFiles.Cells[0,i]);
     end;
  GetListing;
  end;

procedure TVCLSFTPClientComponentDemoForm.edFolderPathExit(Sender: TObject);
begin
  if TGPuttySFTPClient1.Connected then begin
     try
       if edFolderPath.Text<>'' then
          TGPuttySFTPClient1.ChangeDir(edFolderPath.Text);

       edFolderPath.Text:=TGPuttySFTPClient1.WorkDir;
       GetListing;
       except
         on E:Exception do
            Application.MessageBox(PWideChar(E.Message),'Error');
       end;
     end;
  end;

procedure TVCLSFTPClientComponentDemoForm.FormCreate(Sender: TObject);
begin
  // nothing to do
  end;

procedure TVCLSFTPClientComponentDemoForm.FormDestroy(Sender: TObject);
begin
  // nothing to do
  end;

procedure TVCLSFTPClientComponentDemoForm.FormShow(Sender: TObject);
begin
  LoadSettings;
  ProgressBar1.Visible:=false;
  memLog.Lines.Add('Library version: '+TGPuttySFTPClient1.LibVersion);
  end;

procedure TVCLSFTPClientComponentDemoForm.GetListing;
begin
  sgRemoteFiles.RowCount:=1;
  sgRemoteFiles.ColCount:=3;
  sgRemoteFiles.ColWidths[0]:=480;
  sgRemoteFiles.ColWidths[1]:=300;
  sgRemoteFiles.ColWidths[2]:=150;
  sgRemoteFiles.Cells[0,0]:='Name';
  sgRemoteFiles.Cells[1,0]:='Timestamp';
  sgRemoteFiles.Cells[2,0]:='Size';
  TGPuttySFTPClient1.ListDir('');
  if sgRemoteFiles.RowCount>1 then
     sgRemoteFiles.FixedRows:=1;
  sgRemoteFiles.FixedCols:=0;
  end;

procedure TVCLSFTPClientComponentDemoForm.LoadSettings;
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
         end;
      end;
    finally
      FreeAndNil(Reg);
      FInLoadSettings:=false;
    end;
  end;

procedure TVCLSFTPClientComponentDemoForm.SaveSettings;
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
         end;
      end;
    finally
      FreeAndNil(Reg);
    end;
  end;

procedure TVCLSFTPClientComponentDemoForm.sgRemoteFilesDblClick(Sender: TObject);
begin
  if sgRemoteFiles.Selection.Top=sgRemoteFiles.Selection.Bottom then begin
     if sgRemoteFiles.Cells[2,sgRemoteFiles.Selection.Top]='<dir>' then begin
        TGPuttySFTPClient1.ChangeDir(sgRemoteFiles.Cells[0,sgRemoteFiles.Selection.Top]);
        edFolderPath.Text:=TGPuttySFTPClient1.WorkDir;
        SaveSettings;
        GetListing;
        end;
     end;
  end;

function TVCLSFTPClientComponentDemoForm.TGPuttySFTPClient1SFTPGetInput(Sender: TObject; var cancel: Boolean): string;
begin
  Result:=''; // this event will not normally fire
  cancel:=false;
  memLog.Lines.Add('Replying with empty line.');
  end;

function TVCLSFTPClientComponentDemoForm.TGPuttySFTPClient1SFTPListing(Sender: TObject; const Items: TSFTPItems): Boolean;
var StartRow,i:Integer;
begin
  StartRow:=sgRemoteFiles.RowCount;
  sgRemoteFiles.RowCount:=StartRow+Length(Items);
  for i:=0 to Length(Items)-1 do begin
    sgRemoteFiles.Cells[0,StartRow+i]:=Items[i].filename;
    sgRemoteFiles.Cells[1,StartRow+i]:=DateTimeToStr(TTimeZone.Local.ToLocalTime(UnixToDateTime(Items[i].attrs.mtime)));
    if Items[i].attrs.permissions and $F000 = $4000 then
       sgRemoteFiles.Cells[2,StartRow+i]:='<dir>'
    else
       sgRemoteFiles.Cells[2,StartRow+i]:=IntToStr(Items[i].attrs.size);
    end;
  Result:=true;
  end;

procedure TVCLSFTPClientComponentDemoForm.TGPuttySFTPClient1SFTPMessage(Sender: TObject; const Msg: string; const isstderr: Boolean);
begin
  if Assigned(memLog) then
     memLog.Lines.Add(Msg);
  end;

function TVCLSFTPClientComponentDemoForm.TGPuttySFTPClient1SFTPProgress(Sender: TObject; const bytescopied: Int64; const isupload: Boolean): Boolean;
begin
  ProgressBar1.Position:=bytescopied div 1024;
  Application.ProcessMessages;
  Result:=true;
  end;

function TVCLSFTPClientComponentDemoForm.TGPuttySFTPClient1SFTPVerifyHostKey(Sender: TObject; const host: string;
            const port: Integer;
            const fingerprint: string; const verificationstatus: Integer;
            var storehostkey: Boolean): Boolean;
begin
  if verificationstatus=0 then begin
     Result:=true;
     Exit;
     end;

  Result:=Application.MessageBox(PWideChar(WideString(
                'Please confirm the SSH host key fingerprint for '+host+
                ', port '+IntToStr(port)+':'+sLineBreak+
                fingerprint)),
                'Server Verification',
                MB_YESNO or MB_ICONQUESTION) = IDYES;
  storehostkey:=Result;
  end;


end.
