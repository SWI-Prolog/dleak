#include <stdlib.h>
#include <stdio.h>
#include "callcontext.h"
#include "md5.h"
#include "pthread.h"

static pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;
#define LOCK()   pthread_mutex_lock(&lock)
#define UNLOCK() pthread_mutex_unlock(&lock)

typedef struct DL_digest
{ union
  { md5_byte_t   bytes[16];
    unsigned int ints[16/sizeof(int)];
  } v;
} DL_digest;

typedef size_t count_t;

typedef struct dcell
{ DL_digest     digest;
  struct dcell *next;
  int		id;
  count_t	count;			/* number of times seen */
} dcell;

#define DIGEST_HASH_SIZE 8192

static dcell *buckets[DIGEST_HASH_SIZE];
static int    context_id = 0;
static int    debug_level = 1;

#define DEBUG(l,g) do { if ( (l) <= debug_level ) {g;} } while(0)

static char *digest_chars(DL_digest *digest);

/* lookup_digest() tries returns the number of times
   we have seen this digest
*/

static dcell *
lookup_digest(DL_digest *digest)
{ int k = digest->v.ints[0] % DIGEST_HASH_SIZE;
  dcell *c;

  for(c=buckets[k]; c; c = c->next)
  { if ( memcmp(digest, &c->digest, sizeof(*digest)) == 0 )
    { __sync_fetch_and_add(&c->count, 1);
      DEBUG(1, fprintf(stderr, "Existing context %d\n", c->id));
      return c;
    }
  }
  LOCK();
  for(c=buckets[k]; c; c = c->next)
  { if ( memcmp(digest, &c->digest, sizeof(*digest)) == 0 )
    { UNLOCK();
      __sync_fetch_and_add(&c->count, 1);
      DEBUG(1, fprintf(stderr, "Existing context %d\n", c->id));
      return c;
    }
  }

  if ( (c = malloc(sizeof(*c))) )
  { c->digest  = *digest;
    c->id      = ++context_id;
    c->count   = 1;
    c->next    = buckets[k];
    buckets[k] = c;
    DEBUG(1, fprintf(stderr, "New context %d\n", c->id));
    UNLOCK();
    return c;
  }
  UNLOCK();

  return NULL;
}


int
DL_calling_context(int depth)
{ void *buffer[depth];
  int d;

  if ( (d=backtrace(buffer, depth)) > 0 )
  { DL_digest digest;
    md5_state_t state;
    dcell *c;

    md5_init(&state);
    md5_append(&state, (const md5_byte_t *)buffer, (int)(d*sizeof(void*)));
    md5_finish(&state, digest.v.bytes);

    int i;
    fprintf(stderr, "Stack:");
    for(i=0; i<d; i++)
      fprintf(stderr, " %p", buffer[i]);
    fprintf(stderr, "; DIGEST: %s\n", digest_chars(&digest));

    if ( (c=lookup_digest(&digest)) )
      return c->id;
  } else
  { return 0;
  }
}


static int
hexdigit(int i)
{ return "0123456789ABCDEF"[i];
}

static char *
digest_chars(DL_digest *digest)
{ const char *s = digest->v.bytes;
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

static void
dump_context(const dcell *c)
{ fprintf(stderr, "[%d] called %ld times\n", c->id, (long)c->count);
}

void
DL_dump_contexts(void)
{ dcell *c;
  int k;
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
