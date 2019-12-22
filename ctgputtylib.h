#ifndef tgputtylibH
#define tgputtylibH

#define tgputtydll_filename "tgputtylib.dll"

#define SSH_FILEXFER_ATTR_SIZE                    0x00000001
#define SSH_FILEXFER_ATTR_UIDGID                  0x00000002
#define SSH_FILEXFER_ATTR_PERMISSIONS             0x00000004
#define SSH_FILEXFER_ATTR_ACMODTIME               0x00000008
#define SSH_FILEXFER_ATTR_EXTENDED                0x80000000

#define SSH_FXF_READ                              0x00000001
#define SSH_FXF_WRITE                             0x00000002
#define SSH_FXF_APPEND                            0x00000004
#define SSH_FXF_CREAT                             0x00000008
#define SSH_FXF_TRUNC                             0x00000010
#define SSH_FXF_EXCL                              0x00000020


typedef unsigned __int64 uint64_t;

struct fxp_attrs {
    unsigned long flags;
    uint64_t size;
    unsigned long uid;
    unsigned long gid;
    unsigned long permissions;
    unsigned long atime;
    unsigned long mtime;
};

typedef struct fxp_attrs Tfxp_attrs;
typedef Tfxp_attrs *Pfxp_attrs;

struct fxp_name {
    char *filename, *longname;
    struct fxp_attrs attrs;
};

struct fxp_names {
    int nnames;
    struct fxp_name *names;
};

typedef struct fxp_names Tfxp_names;
typedef Tfxp_names *Pfxp_names;

typedef void *TSFTPFileHandle;
typedef TSFTPFileHandle *PSFTPFileHandle;
typedef void *TSFTPTransfer;

typedef struct
{
  // PUBLISHED PART OF STRUCT
  int structsize;
  __int64 tag;

  bool (*ls_callback)(const struct fxp_names *names,const void *libctx);
  const char* (*getpassword_callback)(const char *prompt, const bool echo, bool *cancel, const void *libctx);
  void (*printmessage_callback)(const char *msg, const bool isstderr, const void *libctx);
  bool (*progress_callback)(const uint64_t bytescopied, const bool isupload, const void *libctx);
  int (*read_from_stream)(const uint64_t offset,void *buffer,const int bufsize, const void *libctx);
  int (*write_to_stream)(const uint64_t offset,void *buffer,const int bufsize, const void *libctx);
  bool (*get_input_callback)(char *linebuf,const int maxchars, const void *libctx);
  bool (*verify_host_key_callback)(const char *host,const int port,const char *keytype,
                                   const char *keystr,const char *fingerprint,
                                   const int verificationstatus,
                                   bool *storehostkey, const void *libctx);
  void (*raise_exception_callback)(const char *msg,const char *srcfile,const int line,const void *libctx);
  void (*entercriticalsection_callback)(const int Num); // not currently used because they wouldn't work cross-process
  void (*leavecriticalsection_callback)(const int Num); // not currently used because they wouldn't work cross-process

  // these aren't really good for much ...
  int mode;
  int modeflags;
  char *batchfile;

  // sftp client state from psftp.c
  char *pwd, *homedir; // accessed by host application too

  // static items from sftp.c
  const char *fxp_error_message; // accessed by host application too
  int fxp_errtype; // accessed by host application too

  void* (*malloc_callback) (size_t size);
  void (*free_callback) (void* ptr);
  void* (*realloc_callback)(void* ptr, size_t new_size);

  void* (*debug_malloc_callback) (size_t size, const char* filename, const int line);
  void (*debug_free_callback) (void* ptr, const char* filename, const int line);
  void* (*debug_realloc_callback)(void* ptr, size_t new_size, const char* filename, const int line);

  bool usememorycallbacks;

  // STRICTLY LIBRARY PRIVATE FIELDS FOLLOW
  char reservedbytes[301];

} TTGLibraryContext;



extern int (*psftp_main)(int argc, char *argv[]);

extern int (*tggetlibrarycontextsize)();

extern int (*tgputty_initcontext)(const bool averbose,TTGLibraryContext *libctx);

extern int (*tgputty_initwithcmdline)(int argc, char *argv[], TTGLibraryContext *libctx);

extern int (*tgputtyrunpsftp)(TTGLibraryContext *libctx);

extern void (*tgputtysetappname)(const char *newappname,const char *appversion);

extern int (*tgputtysftpcommand)(const char *line, TTGLibraryContext *libctx);

extern int (*tgsftp_connect)(const char *ahost,const char *auser,const int aport,const char *apassword,

										 TTGLibraryContext *libctx);
extern int (*tgsftp_cd)(const char *adir,TTGLibraryContext *libctx);

extern int (*tgsftp_rm)(const char *afile,TTGLibraryContext *libctx);

extern int (*tgsftp_rmdir)(const char *adir,TTGLibraryContext *libctx);

extern int (*tgsftp_ls)(const char *adir,TTGLibraryContext *libctx);

extern int (*tgsftp_mkdir)(const char *adir,TTGLibraryContext *libctx);

extern int (*tgsftp_mv)(const char *afrom,const char *ato,TTGLibraryContext *libctx);

extern int (*tgsftp_mvex)(const char *afrom,const char *ato,const int moveflags,TTGLibraryContext *libctx);

extern int (*tgsftp_getstat)(const char *afrom,struct fxp_attrs *attrs,TTGLibraryContext *libctx);

extern int (*tgsftp_setstat)(const char *afrom,struct fxp_attrs *attrs,TTGLibraryContext *libctx);

extern int (*tgsftp_putfile)(const char *afromfile,const char *atofile,const bool anappend,TTGLibraryContext *libctx);

extern int (*tgsftp_getfile)(const char *afromfile,const char *atofile,const bool anappend,TTGLibraryContext *libctx);

extern void (*tgsftp_close)(TTGLibraryContext *libctx);

extern void (*tgputty_setverbose)(const bool averbose);

extern void (*tgputty_setkeyfile)(const char *apathname,TTGLibraryContext *libctx);

extern struct fxp_handle *(*tgputty_openfile)(const char *apathname,

                                                          const int anopenflags,
                                                          const struct fxp_attrs *attrs,
                                                          TTGLibraryContext *libctx);
extern int (*tgputty_closefile)(struct fxp_handle **fh,TTGLibraryContext *libctx);

extern void *(*tgputty_xfer_upload_init)(struct fxp_handle *fh, uint64_t offset,TTGLibraryContext *libctx);

extern bool (*tgputty_xfer_upload_ready)(struct fxp_xfer *xfer,TTGLibraryContext *libctx);

extern void (*tgputty_xfer_upload_data)(struct fxp_xfer *xfer, char *buffer, int len, uint64_t anoffset,TTGLibraryContext *libctx);

extern bool (*tgputty_xfer_ensuredone)(struct fxp_xfer *xfer,TTGLibraryContext *libctx);

extern bool (*tgputty_xfer_done)(struct fxp_xfer *xfer,TTGLibraryContext *libctx);

extern void (*tgputty_xfer_cleanup)(struct fxp_xfer *xfer,TTGLibraryContext *libctx);

extern void (*tgputtygetversions)(double *puttyrelease,int *tgputtylibbuild);

extern void (*tgputtyfree)(TTGLibraryContext *libctx);


bool LoadTGPuttyLib();


#endif
