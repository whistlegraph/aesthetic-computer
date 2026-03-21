# KidLisp.com

## Development

### Local Development URL
- **HTTPS**: `https://localhost:8888/kidlisp.com/`
- The site runs on the main aesthetic.computer development server with SSL

### Files
- `index.html` - Main landing page for KidLisp

### Features

#### Theme Support
- Light and dark modes that respect system preferences
- Editor, tabs, and all UI elements adapt automatically
- Monaco editor themes match the exact syntax highlighting colors from `kidlisp.mjs`

#### Syntax Highlighting
- Colors match exactly with `kidlisp.mjs` `getTokenColor()` and `getNormalTokenColor()` methods
- **Light mode colors:**
  - Comments: gray
  - Strings: yellow
  - Numbers: pink (with RGB channel intensity highlighting)
  - Keywords/Functions: cyan (bold)
  - $codes: lime (bold)
  - #codes: magenta prefix, orange identifier
  - Timing tokens: yellow (with red/lime blinks)
- **Synchronized blinking:** The editor receives postMessage signals from the running KidLisp code to highlight timing expressions (like `1s...`, `0.5s`) with red and lime flashes that sync with execution

#### PostMessage API
The iframe communicates with the editor via `postMessage`:
- `kidlisp-reload` - Sent from editor to iframe to update code without page reload
- `kidlisp-highlight` - Sent from iframe to editor with syntax highlighting state during playback
  - Format: `{ type: 'kidlisp-highlight', highlights: [{ token, color, line, column }] }`

### About
KidLisp is a transmedia pattern programming language for everybody. This directory contains the marketing/landing page that introduces users to KidLisp before they try it on aesthetic.computer.

### Related Directories
- `/workspaces/aesthetic-computer/kidlisp/` - Core KidLisp implementation
- `/workspaces/aesthetic-computer/kidlisp-tools/` - Development tools
- `/workspaces/aesthetic-computer/kidlisp-knowledge/` - Documentation and knowledge base
- `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/kidlisp.mjs` - Main KidLisp interpreter with syntax highlighting logic
