#pragma hdrstop
#pragma argsused

#ifdef _WIN32
#include <tchar.h>
#else
  typedef char _TCHAR;
  #define _tmain main
#endif

#include <stdio.h>
#include <iostream>

#include "tgputtylibcbclass.h"

class TClassDemo
{
  TTGPuttySFTP *PSFTP;
  public:
	bool DoVerifyHostKey(const char* host, const int port,
                         const char * fingerprint, const int verificationstatus,
                         bool &storehostkey);
	void DoMessage(const char* Msg, const bool isstderr);
	bool DoProgress(const __int64 bytescopied, const bool isupload);
	bool DoListing(const struct fxp_names* names);
	bool DoGetInput(char* linebuf, const int maxchars);
	void InitSFTP();
    void Run();
};


bool TClassDemo::DoVerifyHostKey(const char* host, const int port,
                                 const char * fingerprint, const int verificationstatus,
                                 bool &storehostkey)
{
   printf("Blindly accepting SSH fingerprint %s for %s\n",fingerprint,host);
   return true;
}

void TClassDemo::DoMessage(const char* Msg, const bool isstderr)
{
	printf("%s",Msg);
}
bool TClassDemo::DoProgress(const __int64 bytescopied, const bool isupload)
{
	printf("Progress: %I64u\n", bytescopied);
	return true;
}
bool TClassDemo::DoListing(const struct fxp_names* names)
{
	for (int i = 0; i < names->nnames; i++)
	{
		printf("%40s  %10I64u\n",names->names[i].filename,names->names[i].attrs.size);
	}
	return true;
}
bool TClassDemo::DoGetInput(char* linebuf, const int maxchars)
{
	printf("Please provide input: ");
	fgets(linebuf, maxchars, stdin);
	return true;
}
void TClassDemo::InitSFTP()
{
    PSFTP = new TTGPuttySFTP(true);

    printf("Using %s\n",PSFTP->GetLibVersion());

	printf("Enter hostname (URL/IP):\n");
	std::string host;
	std::getline(std::cin, host);

	printf("Enter username:\n");
	std::string user;
	std::getline(std::cin, user);

	PSFTP->HostName = host;
	PSFTP->UserName = user;
    PSFTP->Port = 22;

    printf("Enter password for %s:\n",PSFTP->HostName.c_str());

    std::string pw;
    std::getline(std::cin,pw);

    PSFTP->Password = pw;

    PSFTP->OnVerifyHostKey = DoVerifyHostKey;

	PSFTP->OnMessage = DoMessage;

	PSFTP->OnProgress = DoProgress;
	PSFTP->OnListing = DoListing;
	PSFTP->OnGetInput = DoGetInput;
}

void TClassDemo::Run()
{
	InitSFTP();

    PSFTP->Connect();

    printf("Home Directory: %s\n",PSFTP->HomeDir);
    printf("Current Directory: %s\n",PSFTP->WorkDir);

    PSFTP->ChangeDir("/share/CACHEDEV1_DATA/Public/Tests");
    printf("Current Directory Now: %s\n",PSFTP->WorkDir);

    PSFTP->UploadFile("C:\\Tests\\Test.txt","Test.txt",false);

    printf("File Upload Successful.\n");

	printf("Listing:\n");
	PSFTP->ListDir(".");

    delete PSFTP;
}

int _tmain(int argc, _TCHAR* argv[])
{
    printf("Welcome to the C++ Builder SFTP Class Demo!\n");

    TClassDemo *CD = new TClassDemo;

    try
    {
      CD->Run();
    }
    catch (const std::exception& e)
    {
      printf("Exception: %s\n",e.what());
    }


    printf("Press ENTER to Continue\n");
	char dummy=getchar();

	return 0;
}
