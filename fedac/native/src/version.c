// The build stamp, and the only unit compiled with it.
//
// The Makefile passes -DAC_GIT_HASH / -DAC_BUILD_TS / -DAC_BUILD_NAME here and
// nowhere else, so a new build name recompiles this file alone rather than
// changing the command line of all 23 units. See version.h for the history.

#include "version.h"
#include <stdio.h>

#ifndef AC_BUILD_NAME
#define AC_BUILD_NAME "dev"
#endif
#ifndef AC_GIT_HASH
#define AC_GIT_HASH "unknown"
#endif
#ifndef AC_BUILD_TS
#define AC_BUILD_TS "unknown"
#endif

const char ac_build_name[] = AC_BUILD_NAME;
const char ac_git_hash[]   = AC_GIT_HASH;
const char ac_build_ts[]   = AC_BUILD_TS;

// Composed once on first use. The macros are still literals in this unit, so
// the concatenation the callers used to do at compile time happens here.
const char *ac_version_string(void) {
    static char buf[128];
    if (!buf[0]) snprintf(buf, sizeof(buf), "%s-%s", ac_git_hash, ac_build_ts);
    return buf;
}

const char *ac_version_long(void) {
    static char buf[192];
    if (!buf[0])
        snprintf(buf, sizeof(buf), "%s %s-%s",
                 ac_build_name, ac_git_hash, ac_build_ts);
    return buf;
}
