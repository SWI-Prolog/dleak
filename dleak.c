#define _GNU_SOURCE
#include <malloc.h>
#include <dlfcn.h>
#include <stdio.h>
#include <assert.h>

#ifndef TRUE
#define TRUE  1
#define FALSE 0
#endif

static void* (*callocp)(size_t,size_t);
static void* (*mallocp)(size_t);
static void* (*reallocp)(void*,size_t);
static void  (*freep)(void*);

static void __attribute__((constructor))
init(void)
{ callocp   = (void*(*)(size_t,size_t))dlsym(RTLD_NEXT, "calloc");
  mallocp   = (void*(*)(size_t))       dlsym(RTLD_NEXT, "malloc");
  reallocp  = (void*(*)(void*,size_t)) dlsym(RTLD_NEXT, "realloc");
  freep     = (void (*)(void *))       dlsym(RTLD_NEXT, "free");
}

typedef struct no_hook_t
{ int calloc;
  int malloc;
  int realloc;
  int free;
} no_hook_t;

static __thread no_hook_t no_hook;

void *
malloc(size_t len)
{ if ( no_hook.malloc )
  { return (*mallocp)(len);
  } else
  { void *ptr;

    no_hook.malloc = TRUE;
    ptr = (*mallocp)(len);
    fprintf(stderr, "malloc(%ld) --> %p\n", (long)len, ptr);
    no_hook.malloc = FALSE;

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

  if ( no_hook.calloc )
  { return (*callocp)(nmemb, size);
  } else
  { void *ptr;

    no_hook.calloc = TRUE;
    ptr = (*callocp)(nmemb, size);
    fprintf(stderr, "calloc(%ld,%ld) --> %p\n", (long)nmemb, (long)size, ptr);
    no_hook.calloc = FALSE;

    return ptr;
  }
}


void *
realloc(void *ptr, size_t size)
{ if ( no_hook.realloc )
  { return (*realloc)(ptr, size);
  } else
  { void *nptr;

    no_hook.realloc = TRUE;
    nptr = (*reallocp)(ptr, size);
    fprintf(stderr, "realloc(%p,%ld) --> %p\n", ptr, (long)size, nptr);
    no_hook.realloc = FALSE;

    return nptr;
  }
}


void
free(void *ptr)
{ void *r;

  if ( no_hook.free )
  { (*freep)(ptr);
  } else
  { no_hook.free = TRUE;
    (*freep)(ptr);
    fprintf(stderr, "free(%p)\n", ptr);
    no_hook.free = FALSE;
  }
}


