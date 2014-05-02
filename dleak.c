#define _GNU_SOURCE
#include <malloc.h>
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "callcontext.h"

#ifndef TRUE
#define TRUE  1
#define FALSE 0
#endif

static void* (*callocp)(size_t,size_t);
static void* (*mallocp)(size_t);
static void* (*reallocp)(void*,size_t);
static void  (*freep)(void*);

static void  dump_contexts(void);

static void __attribute__((constructor))
init(void)
{ callocp   = (void*(*)(size_t,size_t))dlsym(RTLD_NEXT, "calloc");
  mallocp   = (void*(*)(size_t))       dlsym(RTLD_NEXT, "malloc");
  reallocp  = (void*(*)(void*,size_t)) dlsym(RTLD_NEXT, "realloc");
  freep     = (void (*)(void *))       dlsym(RTLD_NEXT, "free");

  atexit(dump_contexts);
}

static __thread int no_hook;
static int DL_context_depth = 5;

void *
malloc(size_t len)
{ if ( no_hook )
  { return (*mallocp)(len);
  } else
  { void *ptr;

    no_hook = TRUE;
    ptr = (*mallocp)(len);
    fprintf(stderr, "[%d] malloc(%ld) --> %p\n",
	    DL_calling_context(DL_context_depth),
	    (long)len, ptr);
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

    no_hook = TRUE;
    ptr = (*callocp)(nmemb, size);
    fprintf(stderr, "calloc(%ld,%ld) --> %p\n", (long)nmemb, (long)size, ptr);
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

    no_hook = TRUE;
    nptr = (*reallocp)(ptr, size);
    fprintf(stderr, "realloc(%p,%ld) --> %p\n", ptr, (long)size, nptr);
    no_hook = FALSE;

    return nptr;
  }
}


void
free(void *ptr)
{ if ( no_hook )
  { (*freep)(ptr);
  } else
  { no_hook = TRUE;
    (*freep)(ptr);
    fprintf(stderr, "free(%p)\n", ptr);
    no_hook = FALSE;
  }
}


static void
dump_contexts(void)
{ no_hook = TRUE;
  DL_dump_contexts();
  no_hook = FALSE;
}
