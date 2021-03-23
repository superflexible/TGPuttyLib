#include <stdio.h>
#include <windows.h>
#include "ctgputtylib.h"

HINSTANCE hPuttyLib = NULL;

#ifdef WITHCOMMANDLINE // TGPuttyLib is compiled without command line support by default
int (*psftp_main)(int argc, char *argv[]);
int (*tgputty_initwithcmdline)(int argc, char *argv[], TTGLibraryContext *libctx);
#endif

int (*tggetlibrarycontextsize)();

int (*tgputty_initcontext)(const char averbose,TTGLibraryContext *libctx);

int (*tgputtyrunpsftp)(TTGLibraryContext *libctx);

void (*tgputtysetappname)(const char *newappname,const char *appversion);

int (*tgputtysftpcommand)(const char *line, TTGLibraryContext *libctx);

int (*tgsftp_connect)(const char *ahost,const char *auser,const int aport,const char *apassword,

					  TTGLibraryContext *libctx);
int (*tgsftp_cd)(const char *adir,TTGLibraryContext *libctx);

int (*tgsftp_rm)(const char *afile,TTGLibraryContext *libctx);

int (*tgsftp_rmdir)(const char *adir,TTGLibraryContext *libctx);

int (*tgsftp_ls)(const char *adir,TTGLibraryContext *libctx);

int (*tgsftp_mkdir)(const char *adir,TTGLibraryContext *libctx);

int (*tgsftp_mv)(const char *afrom,const char *ato,TTGLibraryContext *libctx);

int (*tgsftp_mvex)(const char *afrom,const char *ato,const int moveflags,TTGLibraryContext *libctx);

int (*tgsftp_getstat)(const char *afrom,struct fxp_attrs *attrs,TTGLibraryContext *libctx);

int (*tgsftp_setstat)(const char *afrom,struct fxp_attrs *attrs,TTGLibraryContext *libctx);

int (*tgsftp_putfile)(const char *afromfile,const char *atofile,const bool anappend,TTGLibraryContext *libctx);

int (*tgsftp_getfile)(const char *afromfile,const char *atofile,const bool anappend,TTGLibraryContext *libctx);

void (*tgsftp_close)(TTGLibraryContext *libctx);

void (*tgputty_setverbose)(const bool averbose);

void (*tgputty_setkeyfile)(const char *apathname,TTGLibraryContext *libctx);

struct fxp_handle *(*tgputty_openfile)(const char *apathname,

                                                          const int anopenflags,
                                                          const struct fxp_attrs *attrs,
                                                          TTGLibraryContext *libctx);
int (*tgputty_closefile)(struct fxp_handle **fh,TTGLibraryContext *libctx);

void *(*tgputty_xfer_upload_init)(struct fxp_handle *fh, uint64_t offset,TTGLibraryContext *libctx);

bool (*tgputty_xfer_upload_ready)(struct fxp_xfer *xfer,TTGLibraryContext *libctx);

void (*tgputty_xfer_upload_data)(struct fxp_xfer *xfer, char *buffer, int len, uint64_t anoffset,TTGLibraryContext *libctx);

bool (*tgputty_xfer_ensuredone)(struct fxp_xfer *xfer,TTGLibraryContext *libctx);

bool (*tgputty_xfer_done)(struct fxp_xfer *xfer,TTGLibraryContext *libctx);

void (*tgputty_xfer_cleanup)(struct fxp_xfer *xfer,TTGLibraryContext *libctx);

void (*tgputtygetversions)(double *puttyrelease,int *tgputtylibbuild);

void (*tgputtyfree)(TTGLibraryContext *libctx);


bool GetProc(const LPCSTR lpProcName,void *p)
{
  *(FARPROC*)p = GetProcAddress(hPuttyLib,lpProcName);
  bool res = ((*(void**)p)!=NULL);
  if (!res)
     printf("Function missing in DLL: %s\n",lpProcName);
  return res;
}

bool LoadTGPuttyLib()
{
  if (hPuttyLib)
     return true;

  hPuttyLib = LoadLibraryA("tgputtylib.dll");

  if (!hPuttyLib)
     return false;

  bool res=true;

#ifdef WITHCOMMANDLINE // TGPuttyLib is compiled without command line support by default
  res &= GetProc("psftp_main",&psftp_main);
  res &= GetProc("tgputty_initwithcmdline",&tgputty_initwithcmdline);
#endif
  res &= GetProc("tggetlibrarycontextsize",&tggetlibrarycontextsize);
  res &= GetProc("tgputty_initcontext",&tgputty_initcontext);
  res &= GetProc("tgputtyrunpsftp",&tgputtyrunpsftp);
  res &= GetProc("tgputtysetappname",&tgputtysetappname);
  res &= GetProc("tgputtysftpcommand",&tgputtysftpcommand);
  res &= GetProc("tgsftp_connect",&tgsftp_connect);
  res &= GetProc("tgsftp_cd",&tgsftp_cd);
  res &= GetProc("tgsftp_rm",&tgsftp_rm);
  res &= GetProc("tgsftp_rmdir",&tgsftp_rmdir);
  res &= GetProc("tgsftp_ls",&tgsftp_ls);
  res &= GetProc("tgsftp_mkdir",&tgsftp_mkdir);
  res &= GetProc("tgsftp_mv",&tgsftp_mv);
  res &= GetProc("tgsftp_mvex",&tgsftp_mvex);
  res &= GetProc("tgsftp_getstat",&tgsftp_getstat);
  res &= GetProc("tgsftp_setstat",&tgsftp_setstat);
  res &= GetProc("tgsftp_putfile",&tgsftp_putfile);
  res &= GetProc("tgsftp_getfile",&tgsftp_getfile);
  res &= GetProc("tgsftp_close",&tgsftp_close);
  res &= GetProc("tgputty_setverbose",&tgputty_setverbose);
  res &= GetProc("tgputty_setkeyfile",&tgputty_setkeyfile);
  res &= GetProc("tgputty_openfile",&tgputty_openfile);
  res &= GetProc("tgputty_closefile",&tgputty_closefile);
  res &= GetProc("tgputty_xfer_upload_init",&tgputty_xfer_upload_init);
  res &= GetProc("tgputty_xfer_upload_ready",&tgputty_xfer_upload_ready);
  res &= GetProc("tgputty_xfer_upload_data",&tgputty_xfer_upload_data);
  res &= GetProc("tgputty_xfer_ensuredone",&tgputty_xfer_ensuredone);
  res &= GetProc("tgputty_xfer_done",&tgputty_xfer_done);
  res &= GetProc("tgputty_xfer_cleanup",&tgputty_xfer_cleanup);
  res &= GetProc("tgputtygetversions",&tgputtygetversions);
  res &= GetProc("tgputtyfree",&tgputtyfree);

  if (!res)
  {
     FreeLibrary(hPuttyLib);
     hPuttyLib = NULL;
  }
  return res;
}


