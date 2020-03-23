#ifndef tgputtylibcbclassH
#define tgputtylibcbclassH

// this can only be used in VCL projects
// #define SUPPORTDELPHISTREAMS

#include <string>

#ifdef SUPPORTDELPHISTREAMS
#include <System.Classes.hpp>
#endif

#include "ctgputtylib.h"

#define cDefaultTimeoutTicks 60000

typedef void (__closure *TOnMessage)(const char *Msg, const bool isstderr);
typedef bool (__closure *TOnProgress)(const __int64 bytescopied, const bool isupload);
typedef bool (__closure *TOnListing)(const struct fxp_names *names);
typedef bool (__closure *TOnGetInput)(char *linebuf,const int maxchars);
typedef bool (__closure *TOnVerifyHostKey)
                   (const char * host, const int port,
                    const char * fingerprint, const int verificationstatus, bool &storehostkey);

static const int MinimumLibraryBuildNum = 0x1;
static const short cDummyClearedErrorCode = short(-1000);  // this error code means there was no real error code

class TTGPuttySFTPException : public std::runtime_error 
      { 
	  public:
	     explicit TTGPuttySFTPException(const std::string what_arg) : std::runtime_error(what_arg) {  };
      };

class TTGPuttySFTP
{
  public:
	TTGLibraryContext Fcontext;
	bool FVerbose;
	std::string FHostName;
	std::string FUserName;
	std::string FPassword;
	std::string FKeyPassword;
	int FPort;
	TOnMessage FOnMessage;
	TOnProgress FOnProgress;
	TOnListing FOnListing;
	TOnGetInput FOnGetInput;
	TOnVerifyHostKey FOnVerifyHostKey;
#ifdef SUPPORTDELPHISTREAMS
	System::Classes::TStream* FUploadStream;
	System::Classes::TStream* FDownloadStream;
#endif
	bool FConnected;
	int FPasswordAttempts;

	std::string FLastMessages;

	char *GetHomeDir();
	char *GetWorkDir();
	void SetVerbose(const bool Value);
	void SetKeyfile(const char *Value);
	char *GetLibVersion();
	int GetErrorCode();
	const char *GetErrorMessage();

    int GetTimeoutTicks() { return Fcontext.timeoutticks; }
    void SetTimeoutTicks(const int Value) { Fcontext.timeoutticks=Value; }
    int GetConnectionTimeoutTicks()  { return Fcontext.connectiontimeoutticks; }
    void SetConnectionTimeoutTicks(const int Value) { Fcontext.connectiontimeoutticks=Value; }
    bool GetAborted()  { return Fcontext.aborted; }
    void SetAborted(const bool Value) { Fcontext.aborted=Value; }

	TTGPuttySFTP(const bool verbose);
	virtual ~TTGPuttySFTP();
    void ClearStatus();
	std::string MakePSFTPErrorMsg(const char *where);
	void Connect();
	void Disconnect();
	void ChangeDir(const char *ADirectory);
	void MakeDir(const char *ADirectory);
	void RemoveDir(const char *ADirectory);
	void ListDir(const char *ADirectory);
	void GetStat(const char *AFileName, Tfxp_attrs *Attrs);
	void SetStat(const char *AFileName, struct fxp_attrs *Attrs);
	void SetModifiedDate(const char *AFileName, const unsigned long unixtime);
	void SetFileSize(const char *AFileName, const __int64 ASize);
	void Move(const char *AFromName, const char *AToName);
	void Delete_File(const char *AName);

	void UploadFile(const char *ALocalFilename, const char *ARemoteFilename, const bool anAppend);
	void DownloadFile(const char *ARemoteFilename, const char *ALocalFilename, const bool anAppend);

#ifdef SUPPORTDELPHISTREAMS
	void UploadStream(const char *ARemoteFilename, System::Classes::TStream* const AStream, const bool anAppend);
	void DownloadStream(const char *ARemoteFilename, System::Classes::TStream* const AStream, const bool anAppend);
#endif

	void * OpenFile(const char *apathname, const int anopenflags, const Pfxp_attrs attrs);
	int CloseFile(struct fxp_handle * &fh);
	void * xfer_upload_init(struct fxp_handle *fh, const unsigned __int64 offset);
	bool xfer_upload_ready(struct fxp_xfer *xfer);
	void xfer_upload_data(struct fxp_xfer *xfer, char *buffer, const int len, const unsigned __int64 anoffset);
	bool xfer_ensuredone(struct fxp_xfer *xfer);
	bool xfer_done(struct fxp_xfer *xfer);
	void xfer_cleanup(struct fxp_xfer *xfer);

	__property std::string HostName = {read=FHostName, write=FHostName};
	__property std::string UserName = {read=FUserName, write=FUserName};
	__property int Port = {read=FPort, write=FPort};
	__property std::string Password = {read=FPassword, write=FPassword};
	__property std::string KeyPassword = {read=FKeyPassword, write=FKeyPassword};
	__property char *HomeDir = {read=GetHomeDir};
	__property char *WorkDir = {read=GetWorkDir};
	__property char *LibVersion = {read=GetLibVersion};
	__property bool Connected = {read=FConnected};
	__property bool Verbose = {read=FVerbose, write=SetVerbose};
	__property char *Keyfile = {write=SetKeyfile};
	__property std::string LastMessages = {read=FLastMessages, write=FLastMessages};
	__property int ErrorCode = {read=GetErrorCode};
	__property const char *ErrorMessage = {read=GetErrorMessage};

    __property int TimeoutTicks = {read=GetTimeoutTicks, write=SetTimeoutTicks};
    __property int ConnectionTimeoutTicks = {read=GetConnectionTimeoutTicks, write=SetConnectionTimeoutTicks};
    __property bool Aborted = {read=GetAborted, write=SetAborted};

	__property TOnMessage OnMessage = {read=FOnMessage, write=FOnMessage};
	__property TOnProgress OnProgress = {read=FOnProgress, write=FOnProgress};
	__property TOnListing OnListing = {read=FOnListing, write=FOnListing};
	__property TOnGetInput OnGetInput = {read=FOnGetInput, write=FOnGetInput};
	__property TOnVerifyHostKey OnVerifyHostKey = {read=FOnVerifyHostKey, write=FOnVerifyHostKey};
};
#endif
