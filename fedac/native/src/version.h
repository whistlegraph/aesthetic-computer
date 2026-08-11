#ifndef AC_VERSION_H
#define AC_VERSION_H

// Build stamp, as runtime strings.
//
// These used to be -D macros on every translation unit's command line. Because
// the build name is bumped on every make invocation, that made CFLAGS — and so
// the whole-CFLAGS signature the Makefile compares — different every time,
// which deleted every object file and guaranteed a ccache miss on all 23 units
// including quickjs.c. Nothing about the stamp actually varies per file, so it
// lives in one tiny unit that alone recompiles when it changes.
//
// Read them as ordinary strings. They are never NULL: version.c substitutes
// "unknown" / "dev" when the build did not pass a stamp.

extern const char ac_build_name[];   // e.g. "sage-beetle-core", "dev" if unset
extern const char ac_git_hash[];     // short commit sha, "unknown" if unset
extern const char ac_build_ts[];     // UTC "%Y-%m-%dT%H:%M", "unknown" if unset

// "<hash>-<ts>", the form the OTA version file and the boot check compare.
// Points at static storage; do not free.
const char *ac_version_string(void);

// "<name> <hash>-<ts>", what system.version reports to pieces.
const char *ac_version_long(void);

#endif
