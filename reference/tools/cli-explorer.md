# CLI Documentation Explorer for Aesthetic Computer

*Specification for command-line documentation browsing and Emacs integration*

## Overview

A command-line documentation explorer that provides fast access to Aesthetic Computer documentation, similar to the existing `kidlisp-tools` CLI interface. This tool will integrate with Emacs to provide a 'docs' tab alongside or replacing the current 'code' tab.

## CLI Tool Specification

### Command Structure
```bash
ac-docs [command] [options]
```

### Core Commands

#### `ac-docs search <query>`
Search across all documentation:
```bash
ac-docs search "wipe"           # Find all references to wipe function
ac-docs search "pointer"        # Find pointer/input documentation  
ac-docs search "kidlisp circle" # Find KidLisp circle function
```

#### `ac-docs api [function]`
Browse JavaScript API documentation:
```bash
ac-docs api                     # List all API categories
ac-docs api graphics            # Show graphics functions
ac-docs api wipe                # Show detailed wipe function info
ac-docs api --examples box      # Show box function with examples
```

#### `ac-docs kidlisp [topic]`
Browse KidLisp language documentation:
```bash
ac-docs kidlisp                 # Show language overview
ac-docs kidlisp syntax          # Basic syntax guide
ac-docs kidlisp functions       # List all functions
ac-docs kidlisp circle          # Show circle function details
ac-docs kidlisp --examples      # Show example programs
```

#### `ac-docs pieces [name]`
Browse piece documentation:
```bash
ac-docs pieces                  # List documented pieces
ac-docs pieces structure        # Show piece lifecycle info
ac-docs pieces template         # Show piece templates
ac-docs pieces paint-canvas     # Show specific piece docs
```

#### `ac-docs coverage`
Show documentation coverage reports:
```bash
ac-docs coverage                # Overall coverage summary
ac-docs coverage api            # API documentation coverage
ac-docs coverage pieces         # Piece documentation coverage
ac-docs coverage --missing      # Show undocumented functions
```

#### `ac-docs update`
Update documentation cache and analysis:
```bash
ac-docs update                  # Refresh all documentation
ac-docs update api              # Re-analyze API only
ac-docs update --force          # Force full rebuild
```

### Interactive Mode

#### `ac-docs interactive` or `ac-docs`
Launch interactive browser:
```
📚 Aesthetic Computer Documentation Explorer

Categories:
  1. JavaScript API Reference
  2. KidLisp Language Guide  
  3. Piece Documentation
  4. System Architecture
  5. Development Tools

Search: [_________________]

> 
```

Navigation commands:
- `1-5` - Select category
- `/query` - Search
- `help` - Show help
- `back` - Go back
- `quit` - Exit

### Output Formatting

#### Terminal Output
- **Syntax highlighting** for code examples
- **Colored headers** and sections
- **Tables** for function parameters
- **Breadcrumb navigation** showing current location
- **Paging** for long content

#### Example Output
```bash
$ ac-docs api wipe

📚 JavaScript API > Graphics > wipe

wipe(color) → void
Clear the entire screen with the specified color.

Parameters:
  color  string|number|array  Color value (name, RGB, etc.)

Examples:
  wipe("black")           // Clear with black
  wipe(255, 0, 0)        // Clear with red RGB
  wipe([100, 200, 255])  // Clear with RGB array

Related:
  → ink()     Set drawing color
  → box()     Draw filled rectangle  
  → screen    Get screen dimensions

Used in pieces: paint-canvas, drawing-app, generative-art (+15 more)

📖 Full documentation: /reference/api/javascript-api.md#wipe
```

## Emacs Integration

### Configuration Addition
Add to Emacs config to replace or supplement the 'code' tab:

```elisp
;; Aesthetic Computer Documentation Tab
(defun ac-docs-tab ()
  "Open Aesthetic Computer documentation explorer"
  (interactive)
  (split-window-right)
  (other-window 1)
  (ansi-term "ac-docs interactive" "ac-docs"))

;; Key bindings
(global-set-key (kbd "C-c d") 'ac-docs-tab)
(global-set-key (kbd "C-c C-d") 'ac-docs-search)

;; Function to search docs from current word
(defun ac-docs-search ()
  "Search AC docs for word at point"
  (interactive)
  (let ((word (thing-at-point 'word)))
    (if word
        (shell-command (format "ac-docs search '%s'" word))
      (call-interactively 'ac-docs-search-prompt))))

(defun ac-docs-search-prompt ()
  "Prompt for search term and search AC docs"
  (interactive)
  (let ((query (read-string "Search AC docs: ")))
    (shell-command (format "ac-docs search '%s'" query))))
```

### Workflow Integration

#### Development Workflow
1. **Coding**: Write AC piece in main Emacs buffer
2. **Reference**: `C-c d` to open docs tab
3. **Search**: Cursor on function name, `C-c C-d` to search
4. **Browse**: Navigate documentation in side panel
5. **Copy**: Copy examples or syntax back to code

#### Tab Configuration Options
```elisp
;; Option 1: Replace 'code' tab with 'docs' tab
(setq ac-default-tab 'docs)

;; Option 2: Add 'docs' tab alongside 'code' tab  
(setq ac-tabs '(code docs kidlisp))

;; Option 3: Context-sensitive tab switching
(defun ac-smart-tab ()
  "Show docs tab when editing AC pieces, code tab otherwise"
  (if (ac-piece-file-p (buffer-file-name))
      (ac-docs-tab)
    (ac-code-tab)))
```

## Implementation Architecture

### File Structure
```
tools/ac-docs/
├── src/
│   ├── cli.mjs           # Main CLI interface
│   ├── search.mjs        # Search functionality
│   ├── display.mjs       # Terminal output formatting  
│   ├── cache.mjs         # Documentation caching
│   └── interactive.mjs   # Interactive mode
├── templates/            # Output templates
├── cache/               # Cached documentation data
└── package.json
```

### Data Sources
- **API Reference**: Generated from `disk.mjs` analysis
- **KidLisp Docs**: Extracted from `kidlisp.mjs`
- **Piece Docs**: Scanned from `/reference/pieces/`
- **Examples**: Collected from existing pieces
- **Usage Data**: Cross-referenced from piece analysis

### Caching Strategy
- **Fast Startup**: Pre-generated documentation cache
- **Auto-Update**: Detect changes in source files
- **Incremental**: Update only changed sections
- **Fallback**: Graceful degradation if cache is stale

## Dependencies

### Core Dependencies
- **Commander.js**: CLI argument parsing
- **Inquirer.js**: Interactive prompts and menus
- **Chalk**: Terminal colors and formatting
- **Marked**: Markdown parsing and rendering
- **Fuse.js**: Fuzzy search across documentation

### Optional Dependencies
- **Blessed**: Enhanced terminal UI (for advanced interactive mode)
- **Highlight.js**: Syntax highlighting for code examples
- **Terminal-image**: Display images in terminal (for piece previews)

## Development Phases

### Phase 1: Basic CLI
- [ ] Core command structure
- [ ] Search functionality
- [ ] API reference browsing
- [ ] Simple output formatting

### Phase 2: Interactive Mode
- [ ] Menu-driven navigation
- [ ] Enhanced search interface
- [ ] Syntax highlighting
- [ ] Paging and scrolling

### Phase 3: Emacs Integration
- [ ] Emacs Lisp functions
- [ ] Key binding configuration
- [ ] Tab integration
- [ ] Context-sensitive help

### Phase 4: Advanced Features
- [ ] Visual piece previews
- [ ] Usage analytics
- [ ] Bookmark system
- [ ] Documentation editing mode

## Usage Examples

### Developer Workflow Example
```bash
# Starting development
$ cd aesthetic-computer/system/public/aesthetic.computer/disks/
$ emacs my-piece.mjs

# In Emacs: C-c d (open docs tab)
# Terminal shows: ac-docs interactive

# Quick API lookup
$ ac-docs api circle
# Shows circle function documentation

# Search for pattern examples  
$ ac-docs search "animation loop"
# Shows pieces and docs related to animation

# Check what's undocumented
$ ac-docs coverage --missing
# Shows API functions needing documentation
```

### Integration with Existing Tools
- **kidlisp-tools**: Share common CLI patterns and utilities
- **VS Code extension**: Potentially share documentation data
- **Web docs**: Export to same format as docs.js endpoint
- **Development scripts**: Integration with build and test tools

## Future Enhancements

- **Live Documentation**: Real-time docs as you edit code
- **AI Integration**: Natural language queries
- **Visual Browser**: GUI version of the CLI tool
- **Community Docs**: User-contributed documentation system
- **Offline Mode**: Full functionality without network

---

*This specification provides the foundation for a comprehensive CLI documentation system that integrates seamlessly with existing AC development workflows.*