#pragma once

#include <string>
#include <stdexcept>
#include <functional>
#include "ctgputtylib.h"


typedef std::function<void(const char* Msg, const bool isstderr)> TOnMessage;
typedef std::function<bool(const __int64 bytescopied, const bool isupload)> TOnProgress;
typedef std::function<bool(const struct fxp_names* names)> TOnListing;
typedef std::function<bool(char* linebuf, const int maxchars)> TOnGetInput;
typedef std::function<bool(const char* host, const int port,const char* fingerprint, const int verificationstatus, bool& storehostkey)> TOnVerifyHostKey;

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
	//System::Classes::TStream* FUploadStream;
	//System::Classes::TStream* FDownloadStream;
	bool FConnected;
	int FPasswordAttempts;

	std::string FLastMessages;

	char* GetHomeDir();
	char* GetWorkDir();
	void SetVerbose(const bool Value);
	void SetKeyfile(const char* Value);
	const int GetPort() { return FPort; }
	void SetPort(const int Value) { FPort = Value; }
	const std::string GetHostName() { return FHostName; }
	void SetHostName(const std::string Value) { FHostName = Value; }
	const std::string GetPassword() { return FPassword; }
	void SetPassword(const std::string Value) { FPassword = Value; }
	const std::string GetUserName() { return FUserName; }
	void SetUserName(const std::string Value) { FUserName = Value; }
	const std::string GetKeyPassword() { return FKeyPassword; }
	void SetKeyPassword(const std::string Value) { FKeyPassword = Value; }
	const std::string GetLastMessages() { return FLastMessages; }
	void SetLastMessages(const std::string Value) { FLastMessages = Value; }

	TOnMessage GetOnMessage() { return FOnMessage; }
	void SetOnMessage(TOnMessage AnOnMessage) { FOnMessage = AnOnMessage; }

	TOnProgress GetOnProgress() { return FOnProgress; }
	void SetOnProgress(TOnProgress AnOnProgress) { FOnProgress = AnOnProgress; }
	TOnListing GetOnListing() { return FOnListing; }
	void SetOnListing(TOnListing AnOnListing) { FOnListing = AnOnListing; }
	TOnGetInput GetOnGetInput() { return FOnGetInput; }
	void SetOnGetInput(TOnGetInput AnOnGetInput) { FOnGetInput = AnOnGetInput; }
	TOnVerifyHostKey GetOnVerifyHostKey() { return FOnVerifyHostKey; }
	void SetOnVerifyHostKey(TOnVerifyHostKey AnOnVerifyHostKey) { FOnVerifyHostKey = AnOnVerifyHostKey; }

	char* GetLibVersion();
	int GetErrorCode();
	const char* GetErrorMessage();

	TTGPuttySFTP(const bool verbose);
	virtual ~TTGPuttySFTP();
	void ClearStatus();
	std::string MakePSFTPErrorMsg(const char* where);
	void Connect();
	void Disconnect();
	void ChangeDir(const char* ADirectory);
	void MakeDir(const char* ADirectory);
	void RemoveDir(const char* ADirectory);
	void ListDir(const char* ADirectory);
	void GetStat(const char* AFileName, Tfxp_attrs* Attrs);
	void SetStat(const char* AFileName, struct fxp_attrs* Attrs);
	void SetModifiedDate(const char* AFileName, const unsigned long unixtime);
	void SetFileSize(const char* AFileName, const __int64 ASize);
	void Move(const char* AFromName, const char* AToName);
	void DeleteFile(const char* AName);

	void UploadFile(const char* ALocalFilename, const char* ARemoteFilename, const bool anAppend);
	void DownloadFile(const char* ARemoteFilename, const char* ALocalFilename, const bool anAppend);

	//void UploadStream(const char *ARemoteFilename, System::Classes::TStream* const AStream, const bool anAppend);
	//void DownloadStream(const char *ARemoteFilename, System::Classes::TStream* const AStream, const bool anAppend);

	void* OpenFile(const char* apathname, const int anopenflags, const Pfxp_attrs attrs);
	int CloseFile(struct fxp_handle*& fh);
	void* xfer_upload_init(struct fxp_handle* fh, const unsigned __int64 offset);
	bool xfer_upload_ready(struct fxp_xfer* xfer);
	void xfer_upload_data(struct fxp_xfer* xfer, char* buffer, const int len, const unsigned __int64 anoffset);
	bool xfer_ensuredone(struct fxp_xfer* xfer);
	bool xfer_done(struct fxp_xfer* xfer);
	void xfer_cleanup(struct fxp_xfer* xfer);

	__declspec(property(get = GetHostName, put = SetHostName)) std::string HostName;
	__declspec(property(get = GetuserName, put = SetUserName)) std::string UserName;
	__declspec(property(get = GetPort, put = SetPort)) int Port;
	__declspec(property(get = GetPassword, put = SetPassword)) std::string Password;
	__declspec(property(get = GetKeyPassword, put = SetKeyPassword)) std::string KeyPassword;
	__declspec(property(get = GetHomeDir)) char* HomeDir;
	__declspec(property(get = GetWorkDir)) char* WorkDir;
	__declspec(property(get = GetLibVersion)) char* LibVersion;
	__declspec(property(get = GetConnected)) bool Connected;
	__declspec(property(get = GetVerbose, put = SetSetVerbose)) bool Verbose;
	__declspec(property(put = SetKeyfile)) char* Keyfile;
	__declspec(property(get = GetLastMessages, put = SetLastMessages)) std::string LastMessages;
	__declspec(property(get = GetErrorCode)) int ErrorCode;
	__declspec(property(get = GetErrorMessage)) const char* ErrorMessage;
	__declspec(property(get = GetOnMessage, put = SetOnMessage)) TOnMessage OnMessage;
	__declspec(property(get = GetOnProgress, put = SetOnProgress)) TOnProgress OnProgress;
	__declspec(property(get = GetOnListing, put = SetOnListing)) TOnListing OnListing;
	__declspec(property(get = GetOnGetInput, put = SetOnGetInput)) TOnGetInput OnGetInput;
	__declspec(property(get = GetOnVerifyHostKey, put = SetOnVerifyHostKey)) TOnVerifyHostKey OnVerifyHostKey;
};
