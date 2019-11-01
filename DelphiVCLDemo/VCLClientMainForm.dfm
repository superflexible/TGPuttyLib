object VCLSFTPClientDemoForm: TVCLSFTPClientDemoForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'VCL SFTP Client Demo'
  ClientHeight = 646
  ClientWidth = 864
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 107
    Height = 13
    Caption = 'Local Folders and Files'
  end
  object Label2: TLabel
    Left = 376
    Top = 16
    Width = 94
    Height = 13
    Caption = 'Remote Connection'
  end
  object Label3: TLabel
    Left = 376
    Top = 41
    Width = 85
    Height = 13
    Caption = 'SFTP Server URL:'
  end
  object Label4: TLabel
    Left = 376
    Top = 68
    Width = 24
    Height = 13
    Caption = 'Port:'
  end
  object Label5: TLabel
    Left = 376
    Top = 97
    Width = 56
    Height = 13
    Caption = 'User Name:'
  end
  object Label6: TLabel
    Left = 376
    Top = 127
    Width = 50
    Height = 13
    Caption = 'Password:'
  end
  object Label7: TLabel
    Left = 376
    Top = 169
    Width = 59
    Height = 13
    Caption = 'Folder Path:'
  end
  object Label8: TLabel
    Left = 376
    Top = 220
    Width = 120
    Height = 13
    Caption = 'Remote Folders and Files'
  end
  object Label9: TLabel
    Left = 24
    Top = 444
    Width = 17
    Height = 13
    Caption = 'Log'
  end
  object DirectoryListBox1: TDirectoryListBox
    Left = 24
    Top = 65
    Width = 241
    Height = 168
    FileList = FileListBox1
    TabOrder = 0
    OnChange = DirectoryListBox1Change
  end
  object DriveComboBox1: TDriveComboBox
    Left = 24
    Top = 40
    Width = 241
    Height = 19
    DirList = DirectoryListBox1
    TabOrder = 1
  end
  object FileListBox1: TFileListBox
    Left = 24
    Top = 239
    Width = 241
    Height = 199
    ItemHeight = 18
    MultiSelect = True
    TabOrder = 2
  end
  object edURL: TEdit
    Left = 473
    Top = 38
    Width = 216
    Height = 21
    TabOrder = 3
  end
  object edPort: TEdit
    Left = 473
    Top = 65
    Width = 48
    Height = 21
    TabOrder = 4
  end
  object edUserName: TEdit
    Left = 473
    Top = 94
    Width = 216
    Height = 21
    TabOrder = 5
  end
  object edPassword: TEdit
    Left = 473
    Top = 124
    Width = 180
    Height = 21
    PasswordChar = '*'
    TabOrder = 6
  end
  object edFolderPath: TEdit
    Left = 473
    Top = 166
    Width = 383
    Height = 21
    TabOrder = 7
    OnExit = edFolderPathExit
  end
  object sgRemoteFiles: TStringGrid
    Left = 376
    Top = 239
    Width = 480
    Height = 199
    ColCount = 3
    DefaultRowHeight = 17
    DrawingStyle = gdsGradient
    FixedCols = 0
    RowCount = 11
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect, goFixedHotTrack]
    TabOrder = 8
    OnDblClick = sgRemoteFilesDblClick
  end
  object btConnect: TButton
    Left = 279
    Top = 63
    Width = 84
    Height = 25
    Caption = 'Connect'
    TabOrder = 9
    OnClick = btConnectClick
  end
  object btDisconnect: TButton
    Left = 279
    Top = 94
    Width = 84
    Height = 25
    Caption = 'Disconnect'
    TabOrder = 10
    OnClick = btDisconnectClick
  end
  object cbVerbose: TCheckBox
    Left = 57
    Top = 443
    Width = 97
    Height = 17
    Caption = 'Verbose'
    Checked = True
    State = cbChecked
    TabOrder = 11
    OnClick = cbVerboseClick
  end
  object memLog: TMemo
    Left = 24
    Top = 461
    Width = 832
    Height = 169
    TabOrder = 12
  end
  object btUpload: TButton
    Left = 279
    Top = 234
    Width = 84
    Height = 25
    Caption = 'Upload'
    TabOrder = 13
    OnClick = btUploadClick
  end
  object btDownload: TButton
    Left = 279
    Top = 265
    Width = 84
    Height = 25
    Caption = 'Download'
    TabOrder = 14
    OnClick = btDownloadClick
  end
  object cbSavePassword: TCheckBox
    Left = 673
    Top = 126
    Width = 123
    Height = 17
    Caption = 'Save (unencrypted)'
    TabOrder = 15
  end
  object ProgressBar1: TProgressBar
    Left = 376
    Top = 195
    Width = 480
    Height = 17
    TabOrder = 16
  end
  object btDeleteLocal: TButton
    Left = 279
    Top = 296
    Width = 84
    Height = 25
    Caption = 'Delete Local'
    TabOrder = 17
    OnClick = btDeleteLocalClick
  end
  object btDeleteRemote: TButton
    Left = 279
    Top = 327
    Width = 84
    Height = 25
    Caption = 'Delete Remote'
    TabOrder = 18
    OnClick = btDeleteRemoteClick
  end
  object btMkDir: TButton
    Left = 279
    Top = 358
    Width = 84
    Height = 25
    Caption = 'Make Dir'
    TabOrder = 19
    OnClick = btMkDirClick
  end
  object btRemoveDir: TButton
    Left = 279
    Top = 388
    Width = 84
    Height = 25
    Caption = 'Remove Dir'
    TabOrder = 20
    OnClick = btRemoveDirClick
  end
  object btMove: TButton
    Left = 279
    Top = 419
    Width = 84
    Height = 25
    Caption = 'Rename/Move'
    TabOrder = 21
    OnClick = btMoveClick
  end
end
