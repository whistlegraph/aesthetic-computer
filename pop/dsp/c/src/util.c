#include "util.h"
#include "acdsp.h"
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

static _Thread_local char g_err[512];

void acdsp_set_error(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(g_err, sizeof(g_err), fmt, ap);
  va_end(ap);
}

const char *acdsp_last_error(void) {
  return g_err[0] ? g_err : NULL;
}

char *acdsp_strtrim(char *s) {
  if (!s) return s;
  while (*s && isspace((unsigned char)*s)) s++;
  if (!*s) return s;
  char *end = s + strlen(s) - 1;
  while (end > s && isspace((unsigned char)*end)) *end-- = '\0';
  return s;
}
