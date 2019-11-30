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
    bool OnVerifyHostKey(const char * host, const int port,
                         const char * fingerprint, const int verificationstatus,
                         bool &storehostkey);
    void Run();
};


bool TClassDemo::OnVerifyHostKey(const char * host, const int port,
                                 const char * fingerprint, const int verificationstatus,
                                 bool &storehostkey)
{
   printf("Blindly accepting SSH fingerprint %s for %s\n",fingerprint,host);
   return true;
}

void TClassDemo::Run()
{
    PSFTP = new TTGPuttySFTP(true);

    printf("Using %s\n",PSFTP->GetLibVersion());

    PSFTP->HostName = "192.168.12.45";
    PSFTP->UserName = "admin";
    PSFTP->Port = 22;

    printf("Enter password for %s:\n",PSFTP->HostName.c_str());

    std::string pw;
    std::getline(std::cin,pw);

    PSFTP->Password = pw.c_str();
    PSFTP->OnVerifyHostKey = OnVerifyHostKey;

    PSFTP->Connect();

    printf("Home Directory: %s\n",PSFTP->HomeDir);
    printf("Current Directory: %s\n",PSFTP->WorkDir);

    PSFTP->ChangeDir("/share/CACHEDEV1_DATA/Public/Tests");
    printf("Current Directory Now: %s\n",PSFTP->WorkDir);

    PSFTP->UploadFile("C:\\Tests\\Test.txt","Test.txt",false);

    printf("File Upload Successful.\n");

    delete PSFTP;
}

int _tmain(int argc, _TCHAR* argv[])
{
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
    getchar();

	return 0;
}
