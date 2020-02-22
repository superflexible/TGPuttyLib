unit tgputtysftpclient;

interface

uses
  SysUtils, Classes,
  tgputtylib,tgputtysftp;

{$if not defined(FPC) and not defined(UNICODE)}
type UnicodeString=WideString;
{$ifend}
  
type
  TSFTPItem=record
       filename,longname:UnicodeString;
       attrs:fxp_attrs;
       end;
  TSFTPItems=array of TSFTPItem;

  TOnSFTPMessage=procedure(Sender: TObject;const Msg:UnicodeString;const isstderr:Boolean) of object;
  TOnSFTPProgress=function(Sender: TObject;const bytescopied:Int64;const isupload:Boolean):Boolean of object;
  TOnSFTPListing=function(Sender: TObject;const Items:TSFTPItems):Boolean of object;
  TOnSFTPGetInput=function(Sender: TObject;var cancel:Boolean):UnicodeString of object;
  TOnSFTPVerifyHostKey=function(Sender: TObject;
                                const host:UnicodeString;const port:Integer;
                                const fingerprint:UnicodeString;
                                const verificationstatus:Integer;
                                var storehostkey:Boolean):Boolean of object;

  TTGPuttySFTPClient = class(TComponent)
  private
    { Private declarations }
    FTGPuttySFTP:TTGPuttySFTP;

    FOnSFTPMessage: TOnSFTPMessage;
    FOnSFTPListing: TOnSFTPListing;
    FOnSFTPGetInput: TOnSFTPGetInput;
    FOnSFTPProgress: TOnSFTPProgress;
    FOnSFTPVerifyHostKey: TOnSFTPVerifyHostKey;

    function GetConnected: Boolean;
    function GetErrorCode: Integer;
    function GetErrorMessage: UnicodeString;
    function GetHomeDir: UnicodeString;
    function GetHostName: UnicodeString;
    function GetKeyPassword: UnicodeString;
    function GetLastMessages: UnicodeString;
    function GetLibVersion: UnicodeString;
    function GetPassword: UnicodeString;
    function GetPort: Integer;
    function GetUserName: UnicodeString;
    function GetVerbose: Boolean;
    function GetWorkDir: UnicodeString;
    procedure SetHostName(const Value: UnicodeString);
    procedure SetKeyfile(const Value: UnicodeString);
    procedure SetKeyPassword(const Value: UnicodeString);
    procedure SetLastMessages(const Value: UnicodeString);
    procedure SetPassword(const Value: UnicodeString);
    procedure SetPort(const Value: Integer);
    procedure SetUserName(const Value: UnicodeString);
    procedure SetVerbose(const Value: Boolean);
    function GetAborted: Boolean;
    function GetConnectionTimeoutTicks: Integer;
    function GetTimeoutTicks: Integer;
    procedure SetAborted(const Value: Boolean);
    procedure SetConnectionTimeoutTicks(const Value: Integer);
    procedure SetTimeoutTicks(const Value: Integer);

    function ListingCallback(const names:Pfxp_names):Boolean;
    procedure MessageCallback(const Msg:AnsiString;const isstderr:Boolean);
    function ProgressCallback(const bytescopied:Int64;const isupload:Boolean):Boolean;
    function GetInputCallback(var cancel:Boolean):AnsiString;
    function VerifyHostKeyCallback(const host:PAnsiChar;const port:Integer;const fingerprint:PAnsiChar;const verificationstatus:Integer;var storehostkey:Boolean):Boolean;
    function GetProxyHost: string;
    function GetProxyPassword: string;
    function GetProxyPort: Integer;
    function GetProxyType: TProxyTypes;
    function GetProxyUserName: string;
    procedure SetProxyHost(const Value: string);
    procedure SetProxyPassword(const Value: string);
    procedure SetProxyPort(const Value: Integer);
    procedure SetProxyType(const Value: TProxyTypes);
    procedure SetProxyUserName(const Value: string);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    procedure Connect;
    procedure Disconnect;

    procedure ChangeDir(const ADirectory:UnicodeString);
    procedure MakeDir(const ADirectory:UnicodeString);
    procedure RemoveDir(const ADirectory:UnicodeString);
    procedure ListDir(const ADirectory:UnicodeString);

    procedure GetStat(const AFileName:UnicodeString;var Attrs:fxp_attrs);
    procedure SetStat(const AFileName:UnicodeString;const Attrs:fxp_attrs);
    procedure SetModifiedDate(const AFileName:UnicodeString;const ATimestamp:TDateTime; const isUTC:Boolean);
    procedure SetFileSize(const AFileName:UnicodeString;const ASize:Int64);
    procedure Move(const AFromName,AToName:UnicodeString);
    procedure DeleteFile(const AName:UnicodeString);

    procedure UploadFile(const ALocalFilename,ARemoteFilename:UnicodeString;const anAppend:Boolean);
    procedure DownloadFile(const ARemoteFilename,ALocalFilename:UnicodeString;const anAppend:Boolean);

    procedure UploadStream(const ARemoteFilename:UnicodeString;const AStream:TStream; const anAppend:Boolean);
    procedure DownloadStream(const ARemoteFilename:UnicodeString;const AStream:TStream; const anAppend:Boolean);

    property HomeDir:UnicodeString read GetHomeDir;
    property WorkDir:UnicodeString read GetWorkDir;
    property LibVersion:UnicodeString read GetLibVersion;

    property Connected:Boolean read GetConnected;
    property Keyfile:UnicodeString write SetKeyfile;
    property LastMessages:UnicodeString read GetLastMessages write SetLastMessages;
    property ErrorCode:Integer read GetErrorCode;
    property ErrorMessage:UnicodeString read GetErrorMessage;
    property Aborted:Boolean read GetAborted write SetAborted;

  published
    { Published declarations }
    property HostName:UnicodeString read GetHostName write SetHostName;
    property UserName:UnicodeString read GetUserName write SetUserName;
    property Port:Integer read GetPort write SetPort;
    property Password:UnicodeString read GetPassword write SetPassword;
    property KeyPassword:UnicodeString read GetKeyPassword write SetKeyPassword;
    property Verbose:Boolean read GetVerbose write SetVerbose;
    property TimeoutTicks:Integer read GetTimeoutTicks write SetTimeoutTicks;
    property ConnectionTimeoutTicks:Integer read GetConnectionTimeoutTicks write SetConnectionTimeoutTicks;

    property ProxyType:TProxyTypes read GetProxyType write SetProxyType;
    property ProxyHost:string read GetProxyHost write SetProxyHost;
    property ProxyPort:Integer read GetProxyPort write SetProxyPort;
    property ProxyUserName:string read GetProxyUserName write SetProxyUserName;
    property ProxyPassword:string read GetProxyPassword write SetProxyPassword;


    property OnSFTPMessage:TOnSFTPMessage read FOnSFTPMessage write FOnSFTPMessage;
    property OnSFTPProgress:TOnSFTPProgress read FOnSFTPProgress write FOnSFTPProgress;
    property OnSFTPListing:TOnSFTPListing read FOnSFTPListing write FOnSFTPListing;
    property OnSFTPGetInput:TOnSFTPGetInput read FOnSFTPGetInput write FOnSFTPGetInput;
    property OnSFTPVerifyHostKey:TOnSFTPVerifyHostKey read FOnSFTPVerifyHostKey write FOnSFTPVerifyHostKey;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TGPuttyLib', [TTGPuttySFTPClient]);
end;

{$if not defined(UNICODE)}
function Utf8ToString(const utf:AnsiString):UnicodeString;
begin
  Result:=Utf8Decode(utf);
  end;
{$ifend}


{ TTGPuttySFTPClient }

procedure TTGPuttySFTPClient.ChangeDir(const ADirectory: UnicodeString);
begin
  FTGPuttySFTP.ChangeDir(Utf8Encode(ADirectory));
  end;

procedure TTGPuttySFTPClient.Connect;
begin
  FTGPuttySFTP.Connect;
  end;

constructor TTGPuttySFTPClient.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FTGPuttySFTP:=TTGPuttySFTP.Create(false);
  FTGPuttySFTP.OnListing:=ListingCallback;
  FTGPuttySFTP.OnMessage:=MessageCallback;
  FTGPuttySFTP.OnProgress:=ProgressCallback;
  FTGPuttySFTP.OnGetInput:=GetInputCallback;
  FTGPuttySFTP.OnVerifyHostKey:=VerifyHostKeyCallback;
  end;

procedure TTGPuttySFTPClient.DeleteFile(const AName: UnicodeString);
begin
  FTGPuttySFTP.DeleteFile(Utf8Encode(AName));
  end;

destructor TTGPuttySFTPClient.Destroy;
begin
  FreeAndNil(FTGPuttySFTP);
  inherited;
  end;

procedure TTGPuttySFTPClient.Disconnect;
begin
  FTGPuttySFTP.Disconnect;
  end;

procedure TTGPuttySFTPClient.DownloadFile(const ARemoteFilename, ALocalFilename: UnicodeString; const anAppend: Boolean);
var LStream:TFileStream;
begin
  if anAppend then begin
     LStream:=TFileStream.Create(ALocalFilename,fmOpenReadWrite);
     LStream.Seek(0,soEnd);
     end
  else
     LStream:=TFileStream.Create(ALocalFilename,fmCreate);
  try
    FTGPuttySFTP.DownloadStream(Utf8Encode(ARemoteFilename),LStream,anAppend);
    finally
      FreeAndNil(LStream);
    end;
  end;

procedure TTGPuttySFTPClient.DownloadStream(const ARemoteFilename: UnicodeString; const AStream: TStream;
                                            const anAppend: Boolean);
begin
  FTGPuttySFTP.DownloadStream(Utf8Encode(ARemoteFilename),AStream,anAppend);
  end;

function TTGPuttySFTPClient.GetAborted: Boolean;
begin
  Result:=FTGPuttySFTP.Aborted;
  end;

function TTGPuttySFTPClient.GetConnected: Boolean;
begin
  Result:=FTGPuttySFTP.Connected;
  end;

function TTGPuttySFTPClient.GetConnectionTimeoutTicks: Integer;
begin
  Result:=FTGPuttySFTP.ConnectionTimeoutTicks;
  end;

function TTGPuttySFTPClient.GetErrorCode: Integer;
begin
  Result:=FTGPuttySFTP.ErrorCode;
  end;

function TTGPuttySFTPClient.GetErrorMessage: UnicodeString;
begin
  Result:=Utf8ToString(FTGPuttySFTP.ErrorMessage);
  end;

function TTGPuttySFTPClient.GetHomeDir: UnicodeString;
begin
  Result:=Utf8ToString(FTGPuttySFTP.HomeDir);
  end;

function TTGPuttySFTPClient.GetHostName: UnicodeString;
begin
  Result:=Utf8ToString(FTGPuttySFTP.HostName);
  end;

function TTGPuttySFTPClient.GetInputCallback(var cancel: Boolean): AnsiString;
begin
  if Assigned(FOnSFTPGetInput) then
     Result:=Utf8Encode(FOnSFTPGetInput(self,cancel))
  else begin
     Result:='';
     cancel:=true;
     end;
  end;

function TTGPuttySFTPClient.GetKeyPassword: UnicodeString;
begin
  Result:=Utf8ToString(FTGPuttySFTP.KeyPassword);
  end;

function TTGPuttySFTPClient.GetLastMessages: UnicodeString;
begin
  Result:=Utf8ToString(FTGPuttySFTP.LastMessages);
  end;

function TTGPuttySFTPClient.GetLibVersion: UnicodeString;
begin
  Result:=Utf8ToString(FTGPuttySFTP.LibVersion);
  end;

function TTGPuttySFTPClient.GetPassword: UnicodeString;
begin
  Result:=Utf8ToString(FTGPuttySFTP.Password);
  end;

function TTGPuttySFTPClient.GetPort: Integer;
begin
  Result:=FTGPuttySFTP.Port;
  end;

function TTGPuttySFTPClient.GetProxyHost: string;
begin
  Result:=Utf8ToString(FTGPuttySFTP.ProxyHost);
  end;

function TTGPuttySFTPClient.GetProxyPassword: string;
begin
  Result:=Utf8ToString(FTGPuttySFTP.ProxyPassword);
  end;

function TTGPuttySFTPClient.GetProxyPort: Integer;
begin
  Result:=FTGPuttySFTP.ProxyPort;
  end;

function TTGPuttySFTPClient.GetProxyType: TProxyTypes;
begin
  Result:=FTGPuttySFTP.ProxyType;
  end;

function TTGPuttySFTPClient.GetProxyUserName: string;
begin
  Result:=Utf8ToString(FTGPuttySFTP.ProxyUserName);
  end;

procedure TTGPuttySFTPClient.GetStat(const AFileName: UnicodeString; var Attrs: fxp_attrs);
begin
  FTGPuttySFTP.GetStat(Utf8Encode(AFileName),Attrs);
  end;

function TTGPuttySFTPClient.GetTimeoutTicks: Integer;
begin
  Result:=FTGPuttySFTP.TimeoutTicks;
  end;

function TTGPuttySFTPClient.GetUserName: UnicodeString;
begin
  Result:=Utf8ToString(FTGPuttySFTP.UserName);
  end;

function TTGPuttySFTPClient.GetVerbose: Boolean;
begin
  Result:=FTGPuttySFTP.Verbose;
  end;

function TTGPuttySFTPClient.GetWorkDir: UnicodeString;
begin
  Result:=Utf8ToString(FTGPuttySFTP.WorkDir);
  end;

procedure TTGPuttySFTPClient.ListDir(const ADirectory: UnicodeString);
begin
  FTGPuttySFTP.ListDir(Utf8Encode(ADirectory));
  end;

function TTGPuttySFTPClient.ListingCallback(const names: Pfxp_names): Boolean;
var Unames:TSFTPItems;
    i:Integer;
begin
  if Assigned(FOnSFTPListing) then begin
     SetLength(Unames,names.nnames);
     for i:=0 to names.nnames-1 do
       with Pfxp_name_array(names^.names)^[i] do begin
         Unames[i].filename:=Utf8ToString(filename);
         Unames[i].longname:=Utf8ToString(longname);
         Unames[i].attrs:=attrs;
         end;
     Result:=FOnSFTPListing(self,Unames);
     end
  else
     Result:=true;
  end;

procedure TTGPuttySFTPClient.MakeDir(const ADirectory: UnicodeString);
begin
  FTGPuttySFTP.MakeDir(Utf8Encode(ADirectory));
  end;

procedure TTGPuttySFTPClient.MessageCallback(const Msg: AnsiString; const isstderr: Boolean);
begin
  if Assigned(FOnSFTPMessage) then
     FOnSFTPMessage(self,Utf8ToString(Msg),isstderr);
  end;

procedure TTGPuttySFTPClient.Move(const AFromName, AToName: UnicodeString);
begin
  FTGPuttySFTP.Move(Utf8Encode(AFromName),Utf8Encode(AToName));
  end;

function TTGPuttySFTPClient.ProgressCallback(const bytescopied: Int64; const isupload: Boolean): Boolean;
begin
  if Assigned(FOnSFTPProgress) then
     Result:=FOnSFTPProgress(self,bytescopied,isupload)
  else
     Result:=true;
  end;

procedure TTGPuttySFTPClient.RemoveDir(const ADirectory: UnicodeString);
begin
  FTGPuttySFTP.RemoveDir(Utf8Encode(ADirectory));
  end;

procedure TTGPuttySFTPClient.SetAborted(const Value: Boolean);
begin
  FTGPuttySFTP.Aborted:=Value;
  end;

procedure TTGPuttySFTPClient.SetConnectionTimeoutTicks(const Value: Integer);
begin
  FTGPuttySFTP.ConnectionTimeoutTicks:=Value;
  end;

procedure TTGPuttySFTPClient.SetFileSize(const AFileName: UnicodeString; const ASize: Int64);
begin
  FTGPuttySFTP.SetFileSize(Utf8Encode(AFileName),ASize);
  end;

procedure TTGPuttySFTPClient.SetHostName(const Value: UnicodeString);
begin
  FTGPuttySFTP.HostName:=Utf8Encode(Value);
  end;

procedure TTGPuttySFTPClient.SetKeyfile(const Value: UnicodeString);
begin
  FTGPuttySFTP.Keyfile:=Utf8Encode(Value);
  end;

procedure TTGPuttySFTPClient.SetKeyPassword(const Value: UnicodeString);
begin
  FTGPuttySFTP.KeyPassword:=Utf8Encode(Value);
  end;

procedure TTGPuttySFTPClient.SetLastMessages(const Value: UnicodeString);
begin
  FTGPuttySFTP.LastMessages:=Utf8Encode(Value);
  end;

procedure TTGPuttySFTPClient.SetModifiedDate(const AFileName: UnicodeString; const ATimestamp: TDateTime; const isUTC: Boolean);
begin
  FTGPuttySFTP.SetModifiedDate(Utf8Encode(AFileName),ATimestamp,isUTC);
  end;

procedure TTGPuttySFTPClient.SetPassword(const Value: UnicodeString);
begin
  FTGPuttySFTP.Password:=Utf8Encode(Value);
  end;

procedure TTGPuttySFTPClient.SetPort(const Value: Integer);
begin
  FTGPuttySFTP.Port:=Value;
  end;

procedure TTGPuttySFTPClient.SetProxyHost(const Value: string);
begin
  FTGPuttySFTP.ProxyHost:=Utf8Encode(Value);
  end;

procedure TTGPuttySFTPClient.SetProxyPassword(const Value: string);
begin
  FTGPuttySFTP.ProxyPassword:=Utf8Encode(Value);
  end;

procedure TTGPuttySFTPClient.SetProxyPort(const Value: Integer);
begin
  FTGPuttySFTP.ProxyPort:=Value;
  end;

procedure TTGPuttySFTPClient.SetProxyType(const Value: TProxyTypes);
begin
  FTGPuttySFTP.ProxyType:=Value;
  end;

procedure TTGPuttySFTPClient.SetProxyUserName(const Value: string);
begin
  FTGPuttySFTP.ProxyUserName:=Utf8Encode(Value);
  end;

procedure TTGPuttySFTPClient.SetStat(const AFileName: UnicodeString;const Attrs: fxp_attrs);
begin
  FTGPuttySFTP.SetStat(Utf8Encode(AFileName),Attrs);
  end;

procedure TTGPuttySFTPClient.SetTimeoutTicks(const Value: Integer);
begin
  FTGPuttySFTP.TimeoutTicks:=Value;
  end;

procedure TTGPuttySFTPClient.SetUserName(const Value: UnicodeString);
begin
  FTGPuttySFTP.UserName:=Utf8Encode(Value);
  end;

procedure TTGPuttySFTPClient.SetVerbose(const Value: Boolean);
begin
  FTGPuttySFTP.Verbose:=Value;
  end;

procedure TTGPuttySFTPClient.UploadFile(const ALocalFilename, ARemoteFilename: UnicodeString; const anAppend: Boolean);
var LStream:TFileStream;
begin
  LStream:=TFileStream.Create(ALocalFilename,fmOpenRead);
  try
    FTGPuttySFTP.UploadStream(Utf8Encode(ARemoteFilename),LStream,anAppend);
    finally
      FreeAndNil(LStream);
    end;
  end;

procedure TTGPuttySFTPClient.UploadStream(const ARemoteFilename: UnicodeString; const AStream: TStream; const anAppend: Boolean);
begin
  FTGPuttySFTP.UploadStream(Utf8Encode(ARemoteFilename),AStream,anAppend);
  end;

function TTGPuttySFTPClient.VerifyHostKeyCallback(const host: PAnsiChar;
                                                  const port: Integer;
                                                  const fingerprint: PAnsiChar;
                                                  const verificationstatus: Integer;
                                                  var storehostkey: Boolean): Boolean;
begin
  if Assigned(FOnSFTPVerifyHostKey) then
     Result:=FOnSFTPVerifyHostKey(self,Utf8ToString(host),port,Utf8ToString(fingerprint),verificationstatus,storehostkey)
  else begin
     Result:=true;
     storehostkey:=false;
     end;
  end;

end.

