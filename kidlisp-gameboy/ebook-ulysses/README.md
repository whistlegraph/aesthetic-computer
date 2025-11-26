# Ulysses eBook for Game Boy Color

A minimal Game Boy Color ROM containing the complete text of James Joyce's *Ulysses* from Project Gutenberg.

## Building

```bash
# Make build script executable
chmod +x build.fish

# Build the ROM (will auto-fetch text from Project Gutenberg)
./build.fish
```

## How It Works

1. `fetch_ulysses.py` - Downloads Ulysses from Project Gutenberg, cleans the text, and paginates it for the Game Boy screen (18 chars × 16 lines per page)
2. `ulysses.c` - The Game Boy Color ROM source with a sepia color palette for comfortable reading
3. `build.fish` - Automated build script

## Controls

| Button | Action |
|--------|--------|
| A / → | Next page |
| B / ← | Previous page |
| ↑ | Skip 10 pages forward |
| ↓ | Skip 10 pages back |
| SELECT | Return to start |
| START | Begin reading (from title screen) |

## Features

- 📚 Complete text of Ulysses (~2000+ pages on Game Boy)
- 🎨 Sepia color palette for easy reading
- ⚡ CGB double-speed mode for fast page turns
- 📖 Clean text rendering with proper word wrapping

## Requirements

- GBDK (in parent directory as `../gbdk`)
- Python 3 with `requests` module (for fetching book)

## Credits

- Text: Project Gutenberg (https://www.gutenberg.org/ebooks/4300)
- Author: James Joyce (1882-1941)
- ROM: Built with GBDK-2020
