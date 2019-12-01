//---------------------------------------------------------------------------

#pragma hdrstop

#include <stdio.h>
#include "tgputtylibcbclass.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

// callbacks

bool ls_callback(const struct fxp_names *names,const void *libctx)
{
  TTGPuttySFTP *TGPSFTP = (TTGPuttySFTP*)((TTGLibraryContext *)libctx)->tag;

  if (TGPSFTP->OnListing!=NULL)
     return TGPSFTP->OnListing(names);
  else
     return true;
}

const char* getpassword_callback(const char *prompt, const bool echo, bool *cancel, const void *libctx)
{
  TTGPuttySFTP *TGPSFTP = (TTGPuttySFTP*)((TTGLibraryContext *)libctx)->tag;
  TGPSFTP->FPasswordAttempts++;
  if (TGPSFTP->FPasswordAttempts>3)
  {
     (*cancel) = true;
     if (TGPSFTP->OnMessage!=NULL)
        TGPSFTP->OnMessage("Password was rejected, or no password given.",true);
     return NULL;
  }
  else
  {
    (*cancel) = false;
    if (strstr(prompt,"Passphrase for key")!=NULL)
       return TGPSFTP->KeyPassword.c_str();
    else
       return TGPSFTP->Password.c_str();
  }
}

void printmessage_callback(const char *msg, const bool isstderr, const void *libctx)
{
  TTGPuttySFTP *TGPSFTP = (TTGPuttySFTP*)((TTGLibraryContext *)libctx)->tag;
  if (TGPSFTP->OnMessage != NULL)
     TGPSFTP->OnMessage(msg,isstderr);

  TGPSFTP->LastMessages += msg;
}

bool progress_callback(const uint64_t bytescopied, const bool isupload, const void *libctx)
{
  TTGPuttySFTP *TGPSFTP = (TTGPuttySFTP*)((TTGLibraryContext *)libctx)->tag;
  if (TGPSFTP->OnProgress != NULL)
     return TGPSFTP->OnProgress(bytescopied,isupload);
  else
     return true;
}

int read_from_stream(const uint64_t offset,void *buffer,const int bufsize, const void *libctx)
{
#ifdef SUPPORTDELPHISTREAMS
  TTGPuttySFTP *TGPSFTP = (TTGPuttySFTP*)((TTGLibraryContext *)libctx)->tag;
  if (TGPSFTP->FUploadStream)
  {
     TGPSFTP->FUploadStream->Position=offset;
     return TGPSFTP->FUploadStream->Read(buffer,bufsize);
  }
  else
#endif
     return 0;
}

int write_to_stream(const uint64_t offset,void *buffer,const int bufsize, const void *libctx)
{
#ifdef SUPPORTDELPHISTREAMS
  TTGPuttySFTP *TGPSFTP = (TTGPuttySFTP*)((TTGLibraryContext *)libctx)->tag;
  if (TGPSFTP->FDownloadStream)
  {
     TGPSFTP->FDownloadStream->Position=offset;
     return TGPSFTP->FDownloadStream->Write(buffer,bufsize);
  }
  else
#endif
     return 0;
}

bool get_input_callback(char *linebuf,const int maxchars, const void *libctx)
{
  TTGPuttySFTP *TGPSFTP = (TTGPuttySFTP*)((TTGLibraryContext *)libctx)->tag;
  if (TGPSFTP->OnGetInput != NULL)
     return TGPSFTP->OnGetInput(linebuf,maxchars);
  else
  {
     fgets(linebuf,maxchars,stdin);
     return true;
  }
}

bool verify_host_key_callback(const char *host,const int port,const char *keytype,
                              const char *keystr,const char *fingerprint,
                              const int verificationstatus,
                              bool *storehostkey, const void *libctx)
{
  TTGPuttySFTP *TGPSFTP = (TTGPuttySFTP*)((TTGLibraryContext *)libctx)->tag;
  if (TGPSFTP->OnVerifyHostKey != NULL)
     return TGPSFTP->OnVerifyHostKey(host,port,fingerprint,
                                     verificationstatus,*storehostkey);
  else
     return false;
}

void raise_exception_callback(const char *msg,const char *srcfile,const int line,const void *libctx)
{
  TTGPuttySFTP *TGPSFTP = (TTGPuttySFTP*)((TTGLibraryContext *)libctx)->tag;
  throw TTGPuttySFTPException((std::string("TTGPuttySFTP exception ")+
                       std::string(msg)+
                       std::string(" at line ")+
                       std::to_string(line)+
                       std::string(" in ")+
                       std::string(srcfile)).c_str());

}


TTGPuttySFTP::TTGPuttySFTP(const bool verbose)
{
  double puttyversion;
  int tgputtylibbuild;

  if (!LoadTGPuttyLib())
     throw TTGPuttySFTPException("tgputtylib could not be loaded");

  tgputtygetversions(&puttyversion,&tgputtylibbuild);
  if (tgputtylibbuild<MinimumLibraryBuildNum)
     throw TTGPuttySFTPException("tgputtylib is too old");

  FVerbose=verbose;
  FPort = 22;

  memset(&Fcontext, 0, sizeof(Fcontext));
  Fcontext.structsize=sizeof(Fcontext);
  if (Fcontext.structsize<tggetlibrarycontextsize())
     throw TTGPuttySFTPException("Incorrect TTGLibraryContext record size");

  Fcontext.tag=(__int64)this;
  Fcontext.ls_callback=ls_callback;
  Fcontext.getpassword_callback=getpassword_callback;
  Fcontext.printmessage_callback=printmessage_callback;
  Fcontext.progress_callback=progress_callback;
  Fcontext.read_from_stream=read_from_stream;
  Fcontext.write_to_stream=write_to_stream;
  Fcontext.get_input_callback=get_input_callback;
  Fcontext.raise_exception_callback=raise_exception_callback;
  Fcontext.verify_host_key_callback=verify_host_key_callback;

  if (tgputty_initcontext(verbose,&Fcontext)!=0)
     throw TTGPuttySFTPException("tgputty_initcontext failed - incorrect DLL version?");
}

TTGPuttySFTP::~TTGPuttySFTP()
{
  Disconnect();
  tgputtyfree(&Fcontext);
}

void TTGPuttySFTP::ClearStatus()
{
  FLastMessages[0]='\0';
  Fcontext.fxp_errtype=cDummyClearedErrorCode; // "clear" error field
}

char *TTGPuttySFTP::GetHomeDir()
{
  return Fcontext.homedir;
}

char *TTGPuttySFTP::GetWorkDir()
{
  return Fcontext.pwd;
}

void TTGPuttySFTP::SetVerbose(const bool Value)
{
  tgputty_setverbose(Value);
  FVerbose=Value;
}

void TTGPuttySFTP::SetKeyfile(const char *Value)
{
  tgputty_setkeyfile(Value,&Fcontext);
}

#define cMaxVersionLen 100
char GVersionBuf[cMaxVersionLen];

char *TTGPuttySFTP::GetLibVersion()
{
  double puttyversion;
  int tgputtylibbuild;

  tgputtygetversions(&puttyversion,&tgputtylibbuild);

  snprintf(GVersionBuf,cMaxVersionLen,"tgputtylib build %d based on PuTTY Release %f",
                       tgputtylibbuild,puttyversion);
  return GVersionBuf;
}

int TTGPuttySFTP::GetErrorCode()
{
  return Fcontext.fxp_errtype;
}

const char *TTGPuttySFTP::GetErrorMessage()
{
  return Fcontext.fxp_error_message;
}

std::string TTGPuttySFTP::MakePSFTPErrorMsg(const char *where)
{
  if (Fcontext.fxp_errtype>=0)
     return std::string(where)+
            std::string(": Error ")+
            std::to_string(Fcontext.fxp_errtype)+
            std::string(", ")+
            std::string(Fcontext.fxp_error_message);
  else
     return std::string(where)+
            std::string(": Unknown Error.\n")+
            LastMessages;
}

void TTGPuttySFTP::Connect()
{
  FPasswordAttempts=0;
  ClearStatus();
  int res=tgsftp_connect(FHostName.c_str(),FUserName.c_str(),FPort,FPassword.c_str(),&Fcontext);
  FConnected=(res==0); // 0 = success
  if (!FConnected)
     throw TTGPuttySFTPException(MakePSFTPErrorMsg("tgsftp_connect"));
}

void TTGPuttySFTP::Disconnect()
{
  if (FConnected)
  {
     tgsftp_close(&Fcontext);
     FConnected=false;
  }
}

void TTGPuttySFTP::ChangeDir(const char *ADirectory)
{
  ClearStatus();
  int res=tgsftp_cd(ADirectory,&Fcontext);
  if (res!=1)  // 1 = success
     throw TTGPuttySFTPException(MakePSFTPErrorMsg("tgsftp_cd"));
}

void TTGPuttySFTP::MakeDir(const char *ADirectory)
{
  ClearStatus();
  int res=tgsftp_mkdir(ADirectory,&Fcontext);
  if (res!=1)  // 1 = success
     throw TTGPuttySFTPException(MakePSFTPErrorMsg("tgsftp_mkdir"));
}

void TTGPuttySFTP::RemoveDir(const char *ADirectory)
{
  ClearStatus();
  int res=tgsftp_rmdir(ADirectory,&Fcontext);
  if (res!=1)  // 1 = success
     throw TTGPuttySFTPException(MakePSFTPErrorMsg("tgsftp_rmdir"));
}

void TTGPuttySFTP::ListDir(const char *ADirectory)
{
  ClearStatus();
  int res=tgsftp_ls(ADirectory,&Fcontext);
  if (res!=1)  // 1 = success
     throw TTGPuttySFTPException(MakePSFTPErrorMsg("tgsftp_ls"));
}

void TTGPuttySFTP::GetStat(const char *AFileName, Tfxp_attrs *Attrs)
{
  ClearStatus();
  if (!tgsftp_getstat(AFileName,Attrs,&Fcontext))
     throw TTGPuttySFTPException(MakePSFTPErrorMsg("tgsftp_getstat"));
}

void TTGPuttySFTP::SetStat(const char *AFileName, struct fxp_attrs *Attrs)
{
  ClearStatus();
  if (!tgsftp_setstat(AFileName,Attrs,&Fcontext))
     throw TTGPuttySFTPException(MakePSFTPErrorMsg("tgsftp_setstat"));
}

void TTGPuttySFTP::SetModifiedDate(const char *AFileName, const unsigned long unixtime)
{
  ClearStatus();
  struct fxp_attrs attrs;

  GetStat(AFileName,&attrs);
  attrs.flags = SSH_FILEXFER_ATTR_ACMODTIME; // set only this
  attrs.mtime = unixtime;

  SetStat(AFileName,&attrs);
}

void TTGPuttySFTP::SetFileSize(const char *AFileName, const __int64 ASize)
{
  ClearStatus();
  struct fxp_attrs attrs;

  GetStat(AFileName,&attrs);
  attrs.flags = SSH_FILEXFER_ATTR_SIZE; // set only this
  attrs.size = ASize;
  SetStat(AFileName,&attrs);
}

void TTGPuttySFTP::Move(const char *AFromName, const char *AToName)
{
  ClearStatus();
  int res=tgsftp_mv(AFromName,AToName,&Fcontext);
  if (res!=1)  // 1 = success
     throw TTGPuttySFTPException(MakePSFTPErrorMsg("tgsftp_mv"));
}

void TTGPuttySFTP::Delete_File(const char *AName)
{
  ClearStatus();
  int res=tgsftp_rm(AName,&Fcontext);
  if (res!=1)  // 1 = success
     throw TTGPuttySFTPException(MakePSFTPErrorMsg("tgsftp_rm"));
}

void TTGPuttySFTP::UploadFile(const char *ALocalFilename, const char *ARemoteFilename, const bool anAppend)
{
  ClearStatus();
  int res=tgsftp_putfile(ALocalFilename, ARemoteFilename, anAppend, &Fcontext);
  if (res!=1)  // 1 = success
     throw TTGPuttySFTPException(MakePSFTPErrorMsg("tgsftp_putfile"));
}

void TTGPuttySFTP::DownloadFile(const char *ARemoteFilename, const char *ALocalFilename, const bool anAppend)
{
  ClearStatus();
  int res=tgsftp_getfile(ARemoteFilename, ALocalFilename, anAppend, &Fcontext);
  if (res!=1)  // 1 = success
     throw TTGPuttySFTPException(MakePSFTPErrorMsg("tgsftp_getfile"));
}

#ifdef SUPPORTDELPHISTREAMS
void TTGPuttySFTP::UploadStream(const char *ARemoteFilename, System::Classes::TStream* const AStream, const bool anAppend)
{
  ClearStatus();
  FUploadStream=AStream;
  try
  {
    int res=tgsftp_putfile(NULL,ARemoteFilename,anAppend,&Fcontext);
    if (res!=1)
       throw TTGPuttySFTPException(MakePSFTPErrorMsg("tgsftp_putfile"));
  }
  __finally
  {
      FUploadStream=NULL;
  }
}

void TTGPuttySFTP::DownloadStream(const char *ARemoteFilename, System::Classes::TStream* const AStream, const bool anAppend)
{
  ClearStatus();
  FDownloadStream=AStream;
  try
  {
    int res=tgsftp_getfile(ARemoteFilename,NULL,anAppend,&Fcontext);
    if (res!=1)
       throw TTGPuttySFTPException(MakePSFTPErrorMsg("tgsftp_getfile"));
  }
  __finally
  {
      FDownloadStream=NULL;
  }
}
#endif

void *TTGPuttySFTP::OpenFile(const char *apathname, const int anopenflags, const Pfxp_attrs attrs)
{
  return tgputty_openfile(apathname,anopenflags,attrs,&Fcontext);
}

int TTGPuttySFTP::CloseFile(struct fxp_handle *&fh)
{
  return tgputty_closefile(&fh,&Fcontext);
}

void *TTGPuttySFTP::xfer_upload_init(struct fxp_handle *fh, const unsigned __int64 offset)
{
  return tgputty_xfer_upload_init(fh,offset,&Fcontext);
}

bool TTGPuttySFTP::xfer_upload_ready(struct fxp_xfer *xfer)
{
  return tgputty_xfer_upload_ready(xfer,&Fcontext);
}

void TTGPuttySFTP::xfer_upload_data(struct fxp_xfer *xfer, char *buffer, const int len, const unsigned __int64 anoffset)
{
  tgputty_xfer_upload_data(xfer,buffer,len,anoffset,&Fcontext);
}

bool TTGPuttySFTP::xfer_ensuredone(struct fxp_xfer *xfer)
{
  return tgputty_xfer_ensuredone(xfer,&Fcontext);
}

bool TTGPuttySFTP::xfer_done(struct fxp_xfer *xfer)
{
  return tgputty_xfer_done(xfer,&Fcontext);
}

void TTGPuttySFTP::xfer_cleanup(struct fxp_xfer *xfer)
{
  tgputty_xfer_cleanup(xfer,&Fcontext);
}


