//---------------------------------------------------------------------------

#ifndef tgputtycbsftpclientH
#define tgputtycbsftpclientH
//---------------------------------------------------------------------------
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include "ctgputtylib.h"
#include "tgputtylibcbclass.h"
//---------------------------------------------------------------------------

struct TSFTPItem
{
public:
	System::UnicodeString filename;
	System::UnicodeString longname;
	fxp_attrs attrs;
};


class TSFTPItems : public TObject
{
public:
   System::DynamicArray<TSFTPItem> SFTPItems;
};

class TSFTPProgressInfo : public TObject
{
public:
  __int64 bytescopied;
  bool isupload;
};

typedef void __fastcall (__closure *TOnSFTPMessage)(System::TObject* Sender, const System::UnicodeString Msg, const bool isstderr);
typedef bool __fastcall (__closure *TOnSFTPProgress)(System::TObject* Sender, const TSFTPProgressInfo *ProgressInfo);
typedef bool __fastcall (__closure *TOnSFTPListing)(System::TObject* Sender, const TSFTPItems *Items);
typedef bool __fastcall (__closure *TOnSFTPGetInput)(System::TObject* Sender, System::UnicodeString &AnInput);
typedef bool __fastcall (__closure *TOnSFTPVerifyHostKey)(System::TObject* Sender, const System::UnicodeString host, const int port, const System::UnicodeString fingerprint, const int verificationstatus, bool &storehostkey);

class PACKAGE TTGPuttyCBSFTPClient : public TComponent
{
private:
    TTGPuttySFTP *FTGPuttySFTP;
	TOnSFTPMessage FOnSFTPMessage;
	TOnSFTPListing FOnSFTPListing;
	TOnSFTPGetInput FOnSFTPGetInput;
	TOnSFTPProgress FOnSFTPProgress;
	TOnSFTPVerifyHostKey FOnSFTPVerifyHostKey;
	bool DoVerifyHostKey(const char* host, const int port,
                         const char * fingerprint, const int verificationstatus,
                         bool &storehostkey);
	void DoMessage(const char* Msg, const bool isstderr);
	bool DoProgress(const __int64 bytescopied, const bool isupload);
	bool DoListing(const struct fxp_names* names);
	bool DoGetInput(char* linebuf, const int maxchars);

	bool __fastcall GetConnected();
	int __fastcall GetErrorCode();
	System::UnicodeString __fastcall GetErrorMessage();
	System::UnicodeString __fastcall GetHomeDir();
	System::UnicodeString __fastcall GetHostName();
	System::UnicodeString __fastcall GetKeyPassword();
	System::UnicodeString __fastcall GetLastMessages();
	System::UnicodeString __fastcall GetLibVersion();
	System::UnicodeString __fastcall GetPassword();
	int __fastcall GetPort();
	System::UnicodeString __fastcall GetUserName();
	bool __fastcall GetVerbose();
	System::UnicodeString __fastcall GetWorkDir();
	void __fastcall SetHostName(const System::UnicodeString Value);
	void __fastcall SetKeyfile(const System::UnicodeString Value);
	void __fastcall SetKeyPassword(const System::UnicodeString Value);
	void __fastcall SetLastMessages(const System::UnicodeString Value);
	void __fastcall SetPassword(const System::UnicodeString Value);
	void __fastcall Set_Port(const int Value);
	void __fastcall SetUserName(const System::UnicodeString Value);
	void __fastcall SetVerbose(const bool Value);

protected:

public:
    __fastcall TTGPuttyCBSFTPClient(TComponent* Owner);
    __fastcall virtual ~TTGPuttyCBSFTPClient();

	void __fastcall Connect();
	void __fastcall Disconnect();
	void __fastcall ChangeDir(const System::UnicodeString ADirectory);
	void __fastcall MakeDir(const System::UnicodeString ADirectory);
	void __fastcall RemoveDir(const System::UnicodeString ADirectory);
	void __fastcall ListDir(const System::UnicodeString ADirectory);
	void __fastcall GetStat(const System::UnicodeString AFileName, fxp_attrs &Attrs);
	void __fastcall SetStat(const System::UnicodeString AFileName, fxp_attrs &Attrs);
	void __fastcall SetModifiedDate(const System::UnicodeString AFileName, const System::TDateTime ATimestamp, const bool isUTC);
	void __fastcall SetFileSize(const System::UnicodeString AFileName, const __int64 ASize);
	void __fastcall Move(const System::UnicodeString AFromName, const System::UnicodeString AToName);
	void __fastcall Delete_File(const System::UnicodeString AName);
	void __fastcall UploadFile(const System::UnicodeString ALocalFilename, const System::UnicodeString ARemoteFilename, const bool anAppend);
	void __fastcall DownloadFile(const System::UnicodeString ARemoteFilename, const System::UnicodeString ALocalFilename, const bool anAppend);
	void __fastcall UploadStream(const System::UnicodeString ARemoteFilename, System::Classes::TStream* const AStream, const bool anAppend);
	void __fastcall DownloadStream(const System::UnicodeString ARemoteFilename, System::Classes::TStream* const AStream, const bool anAppend);

	__property System::UnicodeString HomeDir = {read=GetHomeDir};
	__property System::UnicodeString WorkDir = {read=GetWorkDir};
	__property System::UnicodeString LibVersion = {read=GetLibVersion};
	__property bool Connected = {read=GetConnected, nodefault};
	__property System::UnicodeString Keyfile = {write=SetKeyfile};
	__property System::UnicodeString LastMessages = {read=GetLastMessages, write=SetLastMessages};
	__property int ErrorCode = {read=GetErrorCode, nodefault};
	__property System::UnicodeString ErrorMessage = {read=GetErrorMessage};

__published:
	__property System::UnicodeString HostName = {read=GetHostName, write=SetHostName};
	__property System::UnicodeString UserName = {read=GetUserName, write=SetUserName};
	__property int Port = {read=GetPort, write=Set_Port, nodefault};
	__property System::UnicodeString Password = {read=GetPassword, write=SetPassword};
	__property System::UnicodeString KeyPassword = {read=GetKeyPassword, write=SetKeyPassword};
	__property bool Verbose = {read=GetVerbose, write=SetVerbose, nodefault};
	__property TOnSFTPMessage OnSFTPMessage = {read=FOnSFTPMessage, write=FOnSFTPMessage};
	__property TOnSFTPProgress OnSFTPProgress = {read=FOnSFTPProgress, write=FOnSFTPProgress};
	__property TOnSFTPListing OnSFTPListing = {read=FOnSFTPListing, write=FOnSFTPListing};
	__property TOnSFTPGetInput OnSFTPGetInput = {read=FOnSFTPGetInput, write=FOnSFTPGetInput};
	__property TOnSFTPVerifyHostKey OnSFTPVerifyHostKey = {read=FOnSFTPVerifyHostKey, write=FOnSFTPVerifyHostKey};
};
//---------------------------------------------------------------------------
#endif
