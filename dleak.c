#define _GNU_SOURCE
#include <malloc.h>
#include <string.h>
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "dleak.h"
#include "pthread.h"

#ifndef TRUE
#define TRUE  1
#define FALSE 0
#endif

#undef EXIT_DUMP_CONTEXTS

static int   DL_context_depth = 6;
static char *DL_logfile = "dleak.log";

static FILE *logfd;
static pthread_mutex_t locklogfd = PTHREAD_MUTEX_INITIALIZER;
static __thread int no_hook = 0;

static void* (*callocp)(size_t,size_t);
static void* (*mallocp)(size_t);
static void* (*reallocp)(void*,size_t);
static void  (*freep)(void*);

#ifdef EXIT_DUMP_CONTEXTS
static void  dump_contexts(void);
#endif

FILE *
DL_get_logfd(void)
{ pthread_mutex_lock(&locklogfd);
  no_hook++;
  return logfd;
}

void
DL_release_logfd(void)
{ pthread_mutex_unlock(&locklogfd);
  no_hook--;
}


static int
setup_options(void)
{ char *s;
  int d;

  if ( (s = getenv("DLEAK_DEPTH")) && (d=atoi(s)) > 0 )
    DL_context_depth = d;
  if ( (s = getenv("DLEAK_FILE")) )
    DL_logfile = strdup(s);

  return TRUE;
}


static void __attribute__((constructor))
init(void)
{ callocp   = (void*(*)(size_t,size_t))dlsym(RTLD_NEXT, "calloc");
  mallocp   = (void*(*)(size_t))       dlsym(RTLD_NEXT, "malloc");
  reallocp  = (void*(*)(void*,size_t)) dlsym(RTLD_NEXT, "realloc");
  freep     = (void (*)(void *))       dlsym(RTLD_NEXT, "free");

  no_hook = TRUE;
  setup_options();
  logfd = fopen(DL_logfile, "w");
  no_hook = FALSE;

  unsetenv("LD_PRELOAD");		/* do not inject in sub-processes */
#ifdef EXIT_DUMP_CONTEXTS
  atexit(dump_contexts);
#endif
}

void *
malloc(size_t len)
{ if ( no_hook )
  { return (*mallocp)(len);
  } else
  { void *ptr;
    int cctx;
    FILE *fd;

    no_hook = TRUE;
    cctx = DL_calling_context(DL_context_depth);
    ptr = (*mallocp)(len);
    if ( (fd=DL_get_logfd()) )
    { fprintf(fd, "malloc(%d,%ld,%p).\n", cctx, (long)len, ptr);
      DL_release_logfd();
    }
    no_hook = FALSE;

    return ptr;
  }
}

#define CALLOC1_SIZE 1024

static char calloc1_buf[CALLOC1_SIZE];
static char *calloc1_p = calloc1_buf;

static void *
calloc1(size_t nmemb, size_t size)
{ size_t len = nmemb*size;
  void * ptr = calloc1_p;
  calloc1_p += len;
  assert(calloc1_p < calloc1_buf+CALLOC1_SIZE);

  return ptr;
}


void *
calloc(size_t nmemb, size_t size)
{ if ( !callocp )
    return calloc1(nmemb, size);

  if ( no_hook )
  { return (*callocp)(nmemb, size);
  } else
  { void *ptr;
    int cctx;
    FILE *fd;

    no_hook = TRUE;
    cctx = DL_calling_context(DL_context_depth);
    ptr = (*callocp)(nmemb, size);
    if ( (fd=DL_get_logfd()) )
    { fprintf(fd, "calloc(%d,%ld,%ld,%p).\n",
	      cctx, (long)nmemb, (long)size, ptr);
      DL_release_logfd();
    }
    no_hook = FALSE;

    return ptr;
  }
}


void *
realloc(void *ptr, size_t size)
{ if ( no_hook )
  { return (*realloc)(ptr, size);
  } else
  { void *nptr;
    int cctx;
    FILE *fd;

    no_hook = TRUE;
    cctx = DL_calling_context(DL_context_depth);
    nptr = (*reallocp)(ptr, size);
    if ( (fd=DL_get_logfd()) )
    { fprintf(fd, "realloc(%d,%p,%ld,%p).\n", cctx, ptr, (long)size, nptr);
      DL_release_logfd();
    }
    no_hook = FALSE;

    return nptr;
  }
}


void
free(void *ptr)
{ if ( no_hook )
  { (*freep)(ptr);
  } else
  { int cctx;
    FILE *fd;

    no_hook = TRUE;
    cctx = DL_calling_context(DL_context_depth);
    (*freep)(ptr);
    if ( (fd=DL_get_logfd()) )
    { fprintf(fd, "free(%d,%p).\n", cctx, ptr);
      DL_release_logfd();
    }
    no_hook = FALSE;
  }
}


#ifdef EXIT_DUMP_CONTEXTS
static void
dump_contexts(void)
{ no_hook = TRUE;
  DL_dump_contexts();
  no_hook = FALSE;
}
#endif
