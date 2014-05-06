#define _GNU_SOURCE
#include <dlfcn.h>
#include <execinfo.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/mman.h>
#include "callcontext.h"
#include <pthread.h>

#ifndef TRUE
#define TRUE  1
#define FALSE 0
#endif

#include "murmur.c"

static pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;
#define LOCK()   pthread_mutex_lock(&lock)
#define UNLOCK() pthread_mutex_unlock(&lock)

typedef size_t count_t;

typedef struct dcell
{ struct dcell *next;
  int		depth;			/* depth of call-stack */
  void	      **stack;			/* Return addresses */
  int		id;
  count_t	count;			/* number of times seen */
} dcell;

#define DIGEST_HASH_SIZE 8192
#define MAXCMD		 1024

static dcell *buckets[DIGEST_HASH_SIZE];
static int    context_id = 0;

#ifdef O_DEBUG
static int    debug_level = 1;
#define DEBUG(l,g) do { if ( (l) <= debug_level ) {g;} } while(0)
#else
#define DEBUG(l,g) (void)0
#endif

#if 0
static char *digest_chars(DL_digest *digest);
#endif

#define CHUNK_SIZE 8192
static char    *our_chunk;
static size_t  our_left;

static void *
our_malloc(size_t size)
{ void *ptr;

  size = ((size+sizeof(void*)-1)/sizeof(void*))*sizeof(void*);

  if ( our_left < size )
  { our_chunk = mmap(NULL, CHUNK_SIZE, (PROT_READ|PROT_WRITE),
		     MAP_PRIVATE|MAP_ANONYMOUS, 0, 0);
    if ( MAP_FAILED == our_chunk )
    { perror("mmap");
      abort();
    }
    our_left = CHUNK_SIZE;
  }
  ptr = our_chunk;
  our_chunk += size;
  our_left  -= size;

  return ptr;
}


/* lookup_digest() tries returns the number of times
   we have seen this digest
*/

static unsigned int
stack_hash(int depth, void **stack)
{ return murmer_hash(stack, sizeof(*stack)*depth, 0x14e86b12);
}


static dcell *
lookup_backtrace(int depth, void **stack, int *new)
{ int k = stack_hash(depth, stack) % DIGEST_HASH_SIZE;
  dcell *c;

  for(c=buckets[k]; c; c = c->next)
  { if ( depth == c->depth &&
	 memcmp(stack, c->stack, depth*sizeof(*stack)) == 0 )
    { __sync_fetch_and_add(&c->count, 1);
      DEBUG(1, fprintf(stderr, "Existing context %d\n", c->id));
      *new = FALSE;
      return c;
    }
  }
  LOCK();
  for(c=buckets[k]; c; c = c->next)
  { if ( depth == c->depth &&
	 memcmp(stack, c->stack, depth*sizeof(*stack)) == 0 )
    { UNLOCK();
      __sync_fetch_and_add(&c->count, 1);
      DEBUG(1, fprintf(stderr, "Existing context %d\n", c->id));
      *new = FALSE;
      return c;
    }
  }

  if ( (c = our_malloc(sizeof(*c))) &&
       (c->stack = our_malloc(depth*sizeof(*stack))) )
  { memcpy(c->stack, stack, depth*sizeof(*stack));
    c->depth   = depth;
    c->id      = ++context_id;
    c->count   = 1;
    c->next    = buckets[k];
    buckets[k] = c;
    DEBUG(1, fprintf(stderr, "New context %d\n", c->id));
    UNLOCK();
    *new = TRUE;
    return c;
  }
  UNLOCK();

  return NULL;
}


static void
print_calling_context(FILE *fd, int id, void **ret_addresses, int depth)
{ size_t i;

  DL_locklog();
  fprintf(fd, "cc(%d, [", id);

  for(i=0; i<depth; i++)
  { Dl_info info;
    void *addr = ret_addresses[i];

    if ( i > 0 )
      fprintf(fd, ",");

    if ( dladdr(addr, &info) && info.dli_fname )
    { uintptr_t offset = (uintptr_t)addr - (uintptr_t)info.dli_fbase;

      fprintf(fd, "'%s'+%p",
	      info.dli_fname, (void*)offset);
    } else
    { fprintf(fd, "%p", addr);
    }
  }

  fprintf(fd, "]).\n");
  DL_unlocklog();
}


int
DL_calling_context(FILE *logfd, int depth)
{ void *buffer[depth];
  void **addrs = buffer+1;
  int d;

  if ( (d=backtrace(buffer, depth)-1) > 0 )
  { dcell *c;
    int new;

    if ( (c=lookup_backtrace(d, addrs, &new)) )
    { if ( new )
	print_calling_context(logfd, c->id, addrs, d);
      return c->id;
    }
  }

  return 0;
}


#if 0					/* debugging */
static int
hexdigit(int i)
{ return "0123456789ABCDEF"[i];
}

static char *
digest_chars(DL_digest *digest)
{ const md5_byte_t *s = digest->v.bytes;
  static char msg[33];
  char *o = msg;
  int i;

  for(i=0; i<15; i++)
  { *o++ = hexdigit((s[i]>>4)&0xf);
    *o++ = hexdigit(s[i]&0xf);
  }
  *o = '\0';

  return msg;
}
#endif

static void
dump_context(const dcell *c)
{ fprintf(stderr, "[%d] called %ld times\n", c->id, (long)c->count);
}

void
DL_dump_contexts(void)
{ int k;
  size_t count = 0;

  for(k=0; k<DIGEST_HASH_SIZE; k++)
  { dcell *c;

    for(c = buckets[k]; c; c = c->next)
    { count++;
      dump_context(c);
    }
  }

  fprintf(stderr, "Got %ld contexts\n", (long)count);
}
