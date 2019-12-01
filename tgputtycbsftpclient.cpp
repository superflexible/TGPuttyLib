//---------------------------------------------------------------------------

#include <vcl.h>
#include <System.DateUtils.hpp>

#pragma hdrstop

#include "tgputtycbsftpclient.h"
#pragma package(smart_init)

//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TTGPuttyCBSFTPClient *)
{
    new TTGPuttyCBSFTPClient(NULL);
}
//---------------------------------------------------------------------------
__fastcall TTGPuttyCBSFTPClient::TTGPuttyCBSFTPClient(TComponent* Owner)
    : TComponent(Owner)
{
  try
  {
     FTGPuttySFTP = new TTGPuttySFTP(false);

     FTGPuttySFTP->OnVerifyHostKey = DoVerifyHostKey;
     FTGPuttySFTP->OnMessage = DoMessage;
     FTGPuttySFTP->OnProgress = DoProgress;
     FTGPuttySFTP->OnListing = DoListing;
     FTGPuttySFTP->OnGetInput = DoGetInput;
  }
  catch (...)
  {
    Application->MessageBox(L"Exception creating TGPuttySFTP - tgputtylib.dll missing or wrong version?",L"Error",0);
    FTGPuttySFTP=NULL;
  }

}

__fastcall TTGPuttyCBSFTPClient::~TTGPuttyCBSFTPClient()
{
  delete FTGPuttySFTP;
}

bool TTGPuttyCBSFTPClient::DoVerifyHostKey(const char* host, const int port,
                                 const char * fingerprint, const int verificationstatus,
                                 bool &storehostkey)
{
  if (FOnSFTPVerifyHostKey)
     return FOnSFTPVerifyHostKey(this,UTF8ToString(host),port,UTF8ToString(fingerprint),verificationstatus,storehostkey);
  else
  {
     storehostkey=false;
     return true;
  }
}

void TTGPuttyCBSFTPClient::DoMessage(const char* Msg, const bool isstderr)
{
  if (FOnSFTPMessage)
     FOnSFTPMessage(this,UTF8ToString(Msg),isstderr);
}

bool TTGPuttyCBSFTPClient::DoProgress(const __int64 bytescopied, const bool isupload)
{
  if (FOnSFTPProgress)
  {
     TSFTPProgressInfo *ProgressInfo=new TSFTPProgressInfo;
     try
     {
       ProgressInfo->bytescopied = bytescopied;
       ProgressInfo->isupload = isupload;
       return FOnSFTPProgress(this,ProgressInfo);
     }
     __finally
     {
       delete ProgressInfo;
     }
  }
  else
     return true;
}

bool TTGPuttyCBSFTPClient::DoListing(const struct fxp_names* names)
{
   if (FOnSFTPListing)
   {
      TSFTPItems *Unames=new TSFTPItems;
      try
      {
         Unames->SFTPItems.Length = names->nnames;
         for (int i=0;i<names->nnames;i++)
         {
            Unames->SFTPItems[i].filename = UTF8ToString(names->names[i].filename);
            Unames->SFTPItems[i].longname = UTF8ToString(names->names[i].longname);
            Unames->SFTPItems[i].attrs = names->names[i].attrs;
         }
         return FOnSFTPListing(this,Unames);
      }
      __finally
      {
        delete Unames;
      }
   }
   else
     return true;
}

bool TTGPuttyCBSFTPClient::DoGetInput(char* linebuf, const int maxchars)
{
  if (FOnSFTPGetInput)
  {
     bool cancel=false;
     UnicodeString input;
     cancel = !FOnSFTPGetInput(this,input);
     AnsiString line=UTF8Encode(input);
     if (cancel)
        return false;

     if (line.Length()>maxchars)
        line.SetLength(maxchars);
     if (line.Length()>0)
        strncpy(linebuf,line.c_str(),maxchars);
     else
        *linebuf = '\0';
     return true;
  }
  else
  {
	printf("Please provide input: ");
	fgets(linebuf, maxchars, stdin);
    return true;
  }
}

bool __fastcall TTGPuttyCBSFTPClient::GetConnected()
{
  return (FTGPuttySFTP) ? FTGPuttySFTP->Connected : false;
}

int __fastcall TTGPuttyCBSFTPClient::GetErrorCode()
{
  return (FTGPuttySFTP) ? FTGPuttySFTP->ErrorCode : 0;
}

System::UnicodeString __fastcall TTGPuttyCBSFTPClient::GetErrorMessage()
{
  return (FTGPuttySFTP) ? UTF8ToString(FTGPuttySFTP->ErrorMessage) : "";
}

System::UnicodeString __fastcall TTGPuttyCBSFTPClient::GetHomeDir()
{
  return (FTGPuttySFTP) ? UTF8ToString(FTGPuttySFTP->HomeDir) : "";
}

System::UnicodeString __fastcall TTGPuttyCBSFTPClient::GetHostName()
{
  return (FTGPuttySFTP) ? UTF8ToString(FTGPuttySFTP->HostName.c_str()) : "";
}

System::UnicodeString __fastcall TTGPuttyCBSFTPClient::GetKeyPassword()
{
  return (FTGPuttySFTP) ? UTF8ToString(FTGPuttySFTP->KeyPassword.c_str()) : "";
}

System::UnicodeString __fastcall TTGPuttyCBSFTPClient::GetLastMessages()
{
  return (FTGPuttySFTP) ? UTF8ToString(FTGPuttySFTP->LastMessages.c_str()) : "";
}

System::UnicodeString __fastcall TTGPuttyCBSFTPClient::GetLibVersion()
{
  return (FTGPuttySFTP) ? UTF8ToString(FTGPuttySFTP->LibVersion) : "";
}

System::UnicodeString __fastcall TTGPuttyCBSFTPClient::GetPassword()
{
  return (FTGPuttySFTP) ? UTF8ToString(FTGPuttySFTP->Password.c_str()) : "";
}

int __fastcall TTGPuttyCBSFTPClient::GetPort()
{
  return (FTGPuttySFTP) ? FTGPuttySFTP->Port : 22;
}

System::UnicodeString __fastcall TTGPuttyCBSFTPClient::GetUserName()
{
  return (FTGPuttySFTP) ? UTF8ToString(FTGPuttySFTP->UserName.c_str()) : "";
}

bool __fastcall TTGPuttyCBSFTPClient::GetVerbose()
{
  return (FTGPuttySFTP) ? FTGPuttySFTP->Verbose : false;
}

System::UnicodeString __fastcall TTGPuttyCBSFTPClient::GetWorkDir()
{
  return (FTGPuttySFTP) ? UTF8ToString(FTGPuttySFTP->WorkDir) : "";
}

void __fastcall TTGPuttyCBSFTPClient::SetHostName(const System::UnicodeString Value)
{
  if (FTGPuttySFTP) FTGPuttySFTP->HostName = UTF8Encode(Value).c_str();
}
void __fastcall TTGPuttyCBSFTPClient::SetKeyfile(const System::UnicodeString Value)
{
  if (FTGPuttySFTP) FTGPuttySFTP->Keyfile= UTF8Encode(Value).c_str();
}
void __fastcall TTGPuttyCBSFTPClient::SetKeyPassword(const System::UnicodeString Value)
{
  if (FTGPuttySFTP) FTGPuttySFTP->KeyPassword= UTF8Encode(Value).c_str();
}
void __fastcall TTGPuttyCBSFTPClient::SetLastMessages(const System::UnicodeString Value)
{
  if (FTGPuttySFTP) FTGPuttySFTP->LastMessages= UTF8Encode(Value).c_str();
}
void __fastcall TTGPuttyCBSFTPClient::SetPassword(const System::UnicodeString Value)
{
  if (FTGPuttySFTP) FTGPuttySFTP->Password= UTF8Encode(Value).c_str();
}
void __fastcall TTGPuttyCBSFTPClient::Set_Port(const int Value)
{
  if (FTGPuttySFTP) FTGPuttySFTP->Port= Value;
}
void __fastcall TTGPuttyCBSFTPClient::SetUserName(const System::UnicodeString Value)
{
  if (FTGPuttySFTP) FTGPuttySFTP->UserName= UTF8Encode(Value).c_str();
}
void __fastcall TTGPuttyCBSFTPClient::SetVerbose(const bool Value)
{
  if (FTGPuttySFTP) FTGPuttySFTP->Verbose= Value;
}


//---------------------------------------------------------------------------

void __fastcall TTGPuttyCBSFTPClient::Connect()
{
  FTGPuttySFTP->Connect();
}

void __fastcall TTGPuttyCBSFTPClient::Disconnect()
{
  FTGPuttySFTP->Disconnect();
}

void __fastcall TTGPuttyCBSFTPClient::ChangeDir(const System::UnicodeString ADirectory)
{
  FTGPuttySFTP->ChangeDir(UTF8Encode(ADirectory).c_str());
}

void __fastcall TTGPuttyCBSFTPClient::MakeDir(const System::UnicodeString ADirectory)
{
  FTGPuttySFTP->MakeDir(UTF8Encode(ADirectory).c_str());
}

void __fastcall TTGPuttyCBSFTPClient::RemoveDir(const System::UnicodeString ADirectory)
{
  FTGPuttySFTP->RemoveDir(UTF8Encode(ADirectory).c_str());
}

void __fastcall TTGPuttyCBSFTPClient::ListDir(const System::UnicodeString ADirectory)
{
  FTGPuttySFTP->ListDir(UTF8Encode(ADirectory).c_str());
}

void __fastcall TTGPuttyCBSFTPClient::GetStat(const System::UnicodeString AFileName, fxp_attrs &Attrs)
{
  FTGPuttySFTP->GetStat(UTF8Encode(AFileName).c_str(),&Attrs);
}

void __fastcall TTGPuttyCBSFTPClient::SetStat(const System::UnicodeString AFileName, fxp_attrs &Attrs)
{
  FTGPuttySFTP->SetStat(UTF8Encode(AFileName).c_str(),&Attrs);
}

void __fastcall TTGPuttyCBSFTPClient::SetModifiedDate(const System::UnicodeString AFileName, const System::TDateTime ATimestamp, const bool isUTC)
{
  unsigned long unixtime=DateTimeToUnix(ATimestamp,isUTC);

  FTGPuttySFTP->SetModifiedDate(UTF8Encode(AFileName).c_str(),unixtime);
}

void __fastcall TTGPuttyCBSFTPClient::SetFileSize(const System::UnicodeString AFileName, const __int64 ASize)
{
  FTGPuttySFTP->SetFileSize(UTF8Encode(AFileName).c_str(),ASize);
}

void __fastcall TTGPuttyCBSFTPClient::Move(const System::UnicodeString AFromName, const System::UnicodeString AToName)
{
  FTGPuttySFTP->Move(UTF8Encode(AFromName).c_str(),UTF8Encode(AToName).c_str());
}

void __fastcall TTGPuttyCBSFTPClient::Delete_File(const System::UnicodeString AName)
{
  FTGPuttySFTP->Delete_File(UTF8Encode(AName).c_str());
}

void __fastcall TTGPuttyCBSFTPClient::UploadFile(const System::UnicodeString ALocalFilename, const System::UnicodeString ARemoteFilename, const bool anAppend)
{
  TFileStream *LStream=new TFileStream(ALocalFilename,fmOpenRead);
  try
  {
     FTGPuttySFTP->UploadStream(UTF8Encode(ARemoteFilename).c_str(),LStream,anAppend);
  }
  __finally
  {
     delete LStream;
  }
}

void __fastcall TTGPuttyCBSFTPClient::DownloadFile(const System::UnicodeString ARemoteFilename, const System::UnicodeString ALocalFilename, const bool anAppend)
{
  TFileStream *LStream;
  if (anAppend)
  {
     LStream=new TFileStream(ALocalFilename,fmOpenReadWrite);
     LStream->Seek(__int64(0),soEnd);
  }
  else
     LStream=new TFileStream(ALocalFilename,fmCreate);
  try
  {
    FTGPuttySFTP->DownloadStream(UTF8Encode(ARemoteFilename).c_str(),LStream,anAppend);
  }
  __finally
  {
     delete LStream;
  }
}

void __fastcall TTGPuttyCBSFTPClient::UploadStream(const System::UnicodeString ARemoteFilename, System::Classes::TStream* const AStream, const bool anAppend)
{
   FTGPuttySFTP->UploadStream(UTF8Encode(ARemoteFilename).c_str(),AStream,anAppend);
}

void __fastcall TTGPuttyCBSFTPClient::DownloadStream(const System::UnicodeString ARemoteFilename, System::Classes::TStream* const AStream, const bool anAppend)
{
   FTGPuttySFTP->DownloadStream(UTF8Encode(ARemoteFilename).c_str(),AStream,anAppend);
}



//---------------------------------------------------------------------------
namespace Tgputtycbsftpclient
{
    void __fastcall PACKAGE Register()
    {
         TComponentClass classes[1] = {__classid(TTGPuttyCBSFTPClient)};
         RegisterComponents(L"TGPuttyLib", classes, 0);
    }
}
//---------------------------------------------------------------------------
