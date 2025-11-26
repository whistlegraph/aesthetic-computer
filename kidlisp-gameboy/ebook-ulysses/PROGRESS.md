# Ulysses eBook for Game Boy - Progress Log

## Goal
Create a Game Boy ROM that displays James Joyce's "Ulysses" from Project Gutenberg as an auto-scrolling eBook reader.

## Status: IN PROGRESS (Black Screen Bug)

### What Works
- `pagetest.gb` - Simple 3-page test ROM with hardcoded strings **WORKS**
- `test-simple.gb` - Minimal "Hello Ulysses!" ROM **WORKS**
- Building ROMs with GBDK-2020 for ARM64 Linux **WORKS**
- Fetching and parsing Ulysses from Project Gutenberg **WORKS**
- Generating C header files with paginated text **WORKS**

### What Doesn't Work
- `ulysses.gb` - Shows black screen when loaded
- The only difference from working `pagetest.gb` is using `#include "book_data_small.h"` instead of inline strings

### Suspected Issues
1. The header file may have some issue with how strings are stored/accessed
2. Possibly ROM banking issues (though we're under 32KB now)
3. Something with the `const char* const` array declaration

## Files

### Source Files
- `ulysses2.c` - Current main source (based on working pagetest.c)
- `pagetest.c` - Working test ROM with 3 hardcoded pages
- `test-simple.c` - Minimal working ROM
- `ulysses.c` - Original attempt (broken)
- `ulysses_auto.c` - Auto-scroll version (broken)

### Python Scripts
- `fetch_small.py` - Fetches Ulysses, creates `book_data_small.h` (50 pages)
- `fetch_ulysses.py` - Original fetcher (300 pages, too large)

### Generated Headers
- `book_data_small.h` - 50 pages of Ulysses (~17KB)
- `book_data.h` - 300 pages (too large for simple ROM)

### Built ROMs
- `pagetest.gb` - **WORKS** - 32KB
- `test-simple.gb` - **WORKS** - 32KB  
- `ulysses.gb` - **BROKEN** - 32KB (black screen)

## Build Commands

```fish
# Set up paths
set lcc /workspaces/aesthetic-computer/kidlisp-gameboy/gbdk/bin/lcc
set outdir /workspaces/aesthetic-computer/system/public/assets/gameboy

# Build working test
$lcc -o pagetest.gb pagetest.c
cp pagetest.gb $outdir/

# Build ulysses (currently broken)
$lcc -o ulysses.gb ulysses2.c
cp ulysses.gb $outdir/

# Run in emulator
ac gameboy~pagetest  # Works!
ac gameboy~ulysses   # Black screen :(
```

## Next Steps to Debug

1. **Try inline strings**: Copy first page text directly into ulysses2.c instead of using header
2. **Check array size**: Maybe 50 entries is too many - try 5 pages
3. **Check string length**: Maybe individual strings are too long
4. **Use different storage**: Try `__at` banking or different memory sections
5. **Compare hex dumps**: `od -A x -t x1z pagetest.gb | head -50` vs ulysses.gb
6. **Test in different emulator**: Try BGB or mGBA instead of our web emulator

## Architecture Notes

- GBDK-2020 v4.4.0 for ARM64 Linux
- Using `<gb/gb.h>` (works) NOT `<gbdk/font.h>` (doesn't work in our setup)
- `printf` and `gotoxy` from `<gbdk/console.h>`
- `wait_vbl_done()` for vsync
- ROM fits in 32KB (no banking needed)

## Controls (when it works)
- **A** - Next page
- **B** - Previous page  
- **START** - Toggle auto-scroll
- **SELECT** - Go to page 1
- Auto-scrolls every ~3 seconds

## Project Gutenberg Source
- URL: https://www.gutenberg.org/cache/epub/4300/pg4300.txt
- Full text: ~1.5MB, ~5000 pages when paginated for Game Boy
- Current ROM: 50 pages (first ~50 pages of the book)
