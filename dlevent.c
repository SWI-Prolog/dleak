#define _GNU_SOURCE
#include <SWI-Prolog.h>
#include <stdio.h>
#include <dlfcn.h>
#include "dleak.h"

static FILE* (*DL_get_logfdp)(void);
static void  (*DL_release_logfdp)(void);

static foreign_t
dl_event(term_t t)
{ char *s;
  size_t len;

  if ( DL_get_logfdp )
  { if ( PL_get_nchars(t, &len, &s, CVT_STRING|CVT_EXCEPTION) )
    { FILE *fd;

      if ( (fd=(*DL_get_logfdp)()) )
      { fprintf(fd, "event(%s).\n", s);
	(*DL_release_logfdp)();
	return TRUE;
      }
    }

    return FALSE;
  }

  return TRUE;
}

install_t
install_dlevent(void)
{ DL_get_logfdp     = dlsym(RTLD_DEFAULT, "DL_get_logfd");
  DL_release_logfdp = dlsym(RTLD_DEFAULT, "DL_release_logfd");

  PL_register_foreign("dlevent_", 1, dl_event, 0);
}
