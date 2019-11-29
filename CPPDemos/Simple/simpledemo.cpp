#pragma hdrstop
#pragma argsused

#include "tgputtylib.hpp"

#ifdef _WIN32
#include <tchar.h>
#else
  typedef char _TCHAR;
  #define _tmain main
#endif

#include <stdio.h>

int _tmain(int argc, _TCHAR* argv[])
{
    if (LoadTGPuttyLib())
       printf("Success loading tgputtylib!\n");
    else
    {
       printf("Failure loading tgputtylib!\n");
       getchar();
       return 1;
    }

    TTGLibraryContext Ctx={sizeof(TTGLibraryContext)};

    int res=tgputty_initcontext(true,&Ctx);
    if (res!=0)
    {
       printf("Failure initializing context!\n");
       getchar();
       return 1;
    }

    res = tgsftp_connect("192.168.12.45","admin",22,"password",&Ctx);
    if (res==0)
       printf("Connected successfully!\n");
    else
    {
       printf("Failure connecting!\n");
       getchar();
       return 1;
    }

    res = tgsftp_cd("/share/CACHEDEV1_DATA/Public/Tests",&Ctx);
    if (res==1)
       printf("cd successful.\n");
    else
       printf("cd failed.\n");

    res = tgsftp_putfile("C:\\Tests\\Test.txt","Test.txt",false,&Ctx);
    if (res==1)
       printf("putfile successful.\n");
    else
       printf("putfile failed.\n");


    tgsftp_close(&Ctx);
    printf("Disconnected successfully!\n");

    printf("Press Any Key to Continue\n");
    getchar();
	return 0;
}
