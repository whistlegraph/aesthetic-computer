# VS Code Theme Development Plan

**Goal:** Create custom VS Code color themes based on Aesthetic Computer's prompt.mjs dark and light modes, shipping them with the existing `aesthetic-computer-code` extension.

## 📊 Current State

### Existing Extension Structure
```
vscode-extension/
├── package.json           # Extension manifest
├── extension.ts           # Main extension code
├── syntaxes/              # TextMate grammars
│   ├── PeaceScript.tmGrammar.json
│   ├── pjs-configuration.json
│   ├── KidLisp.tmGrammar.json      ✨ NEW
│   └── kidlisp-configuration.json  ✨ NEW
├── resources/             # Icons and images
├── main.css               # Webview styles
└── out/                   # Compiled output
```

### Language Support
| Language | Extension | Grammar |
|----------|-----------|---------|
| JavaScript | `.mjs` | VS Code built-in |
| PeaceScript | `.pjs` | `source.pjs` |
| **KidLisp** | `.lisp` | `source.kidlisp` ✨ |

### Source Color Schemes (from [prompt.mjs](../system/public/aesthetic.computer/disks/prompt.mjs#L6538))

**Dark Mode:**
| Property | Value | Description |
|----------|-------|-------------|
| `text` | `[255, 100]` | White with alpha |
| `background` | `[70, 50, 100]` | Purple-indigo |
| `prompt` | `[200, 30, 100, 200]` | Pink-magenta |
| `block` | `[200, 30, 100]` | Pink cursor block |
| `highlight` | `[255, 100, 0]` | Orange selection |
| `guideline` | `[0, 0, 255, 64]` | Blue guidelines |
| `auto` | `"white"` | Autocomplete |
| `statusColor` | `"lime"` | Status indicators |
| `focusOutline` | `"brown"` | Focus rings |

**Light Mode:**
| Property | Value | Description |
|----------|-------|-------------|
| `text` | `[40, 30, 90]` | Dark purple-blue |
| `background` | `[252, 247, 197]` | Legal pad yellow |
| `prompt` | `[60, 40, 120]` | Dark purple |
| `block` | `[56, 122, 223]` | Blue cursor block |
| `highlight` | `[246, 253, 195]` | Pale yellow selection |
| `guideline` | `[255, 207, 105]` | Gold guidelines |
| `auto` | `"red"` | Autocomplete |
| `statusColor` | `"darkgreen"` | Status indicators |
| `focusOutline` | `"aqua"` | Focus rings |

### KidLisp Syntax Highlighting Colors (from [kidlisp.mjs](../system/public/aesthetic.computer/lib/kidlisp.mjs#L10268))
| Token Type | Color | Notes |
|------------|-------|-------|
| Comments (`;`) | gray | |
| Strings | yellow | Quoted text |
| Numbers | pink | General numeric |
| RGB R channel | red | RGB value context |
| RGB G channel | green | RGB value context |
| RGB B channel | deepskyblue | RGB value context |
| Timing (s/f) | yellow | `1s`, `0.5s`, `3f` |
| Active timing | red/lime | Blinking states |
| $codes | lime/limegreen | Embedded layers |
| #codes | magenta/orange | Painting refs |
| `rainbow` | RAINBOW | Special marker |
| `zebra` | ZEBRA | Special marker |
| Parentheses | Rainbow nested | Depth-based colors |
| Functions | Context-dependent | |

---

## � KidLisp Syntax Highlighting (NEW)

The extension now includes full KidLisp syntax highlighting for `.lisp` files!

### KidLisp Token Scopes

| Token Type | TextMate Scope | Example |
|------------|----------------|---------|
| Comments | `comment.line.semicolon.kidlisp` | `; comment` |
| Strings | `string.quoted.double/single.kidlisp` | `"hello"` `'world'` |
| Timing (cycle) | `constant.numeric.timing.cycle.kidlisp` | `1s...` `2f...` |
| Timing (delay) | `constant.numeric.timing.delay.kidlisp` | `0.5s` `3f` `1s!` |
| $codes | `variable.other.embedded.kidlisp` | `$abc123` |
| #codes | `entity.name.tag.painting.kidlisp` | `#WDv` |
| Colors | `support.constant.color.kidlisp` | `red` `lime` `deepskyblue` |
| Special | `support.constant.rainbow.kidlisp` | `rainbow` `zebra` |
| Built-ins | `variable.language.builtin.kidlisp` | `width` `height` `frame` |
| Graphics funcs | `entity.name.function.graphics.kidlisp` | `wipe` `ink` `line` `box` |
| Control funcs | `entity.name.function.control.kidlisp` | `def` `repeat` `tap` `jump` |
| Math funcs | `entity.name.function.math.kidlisp` | `wiggle` `sin` `random` |
| Numbers | `constant.numeric.*.kidlisp` | `42` `-3.14` |
| Operators | `keyword.operator.kidlisp` | `+` `-` `*` `/` |
| Parentheses | `punctuation.paren.*.kidlisp` | `(` `)` |

### Example KidLisp with Highlighting

```lisp
; 🎨 Rainbow circles
(wipe "navy")           ; comment = gray
(def size 50)           ; def = cyan, number = pink
(repeat 10 i            ; repeat = cyan, i = orange
  (ink rainbow)         ; ink = cyan, rainbow = special
  (circle              
    (* i 30)            ; operator = yellow
    (+ height/2 (wiggle 20))  ; height = blue, wiggle = magenta
    size))
(1s... (zoom 1.01))     ; timing = yellow
```

---

## �🎯 Implementation Plan

### Phase 1: Theme File Structure

Create theme JSON files in the extension:

```
vscode-extension/
├── themes/
│   ├── aesthetic-dark-color-theme.json
│   └── aesthetic-light-color-theme.json
├── package.json  (updated with theme contributions)
└── ...
```

### Phase 2: Package.json Theme Contribution

Add to `contributes` section:

```json
{
  "contributes": {
    "themes": [
      {
        "label": "Aesthetic Computer Dark",
        "uiTheme": "vs-dark",
        "path": "./themes/aesthetic-dark-color-theme.json"
      },
      {
        "label": "Aesthetic Computer Light",
        "uiTheme": "vs",
        "path": "./themes/aesthetic-light-color-theme.json"
      }
    ]
  }
}
```

### Phase 3: Dark Theme Colors

**Workbench Colors (`colors` section):**
```json
{
  "type": "dark",
  "name": "Aesthetic Computer Dark",
  "colors": {
    "editor.background": "#46326e",
    "editor.foreground": "#ffffff99",
    "editorCursor.foreground": "#c81e64",
    "editor.lineHighlightBackground": "#c81e6420",
    "editor.selectionBackground": "#ff640080",
    "activityBar.background": "#2d1f47",
    "sideBar.background": "#3a2959",
    "statusBar.background": "#00ff00",
    "titleBar.activeBackground": "#46326e",
    "panel.background": "#3a2959",
    "terminal.background": "#46326e",
    "terminal.foreground": "#ffffff"
  }
}
```

### Phase 4: Light Theme Colors

**Workbench Colors:**
```json
{
  "type": "light",
  "name": "Aesthetic Computer Light",
  "colors": {
    "editor.background": "#fcf7c5",
    "editor.foreground": "#281e5a",
    "editorCursor.foreground": "#387adf",
    "editor.lineHighlightBackground": "#ffcf6940",
    "editor.selectionBackground": "#f6fdc380",
    "activityBar.background": "#e8e3b0",
    "sideBar.background": "#f5f0c0",
    "statusBar.background": "#006400",
    "titleBar.activeBackground": "#fcf7c5"
  }
}
```

### Phase 5: Syntax Token Colors

Translate KidLisp highlighting to TextMate scopes:

```json
{
  "tokenColors": [
    {
      "scope": "comment",
      "settings": { "foreground": "#808080" }
    },
    {
      "scope": "string",
      "settings": { "foreground": "#ffff00" }
    },
    {
      "scope": "constant.numeric",
      "settings": { "foreground": "#ffc0cb" }
    },
    {
      "scope": "keyword.control",
      "settings": { "foreground": "#00ff00" }
    },
    {
      "scope": "entity.name.function",
      "settings": { "foreground": "#00ffff" }
    },
    {
      "scope": "variable",
      "settings": { "foreground": "#ffa500" }
    },
    {
      "scope": "support.function",
      "settings": { "foreground": "#ff69b4" }
    }
  ]
}
```

---

## 🔧 Live Development Workflow

### Method 1: Settings-Based Prototyping (Recommended)

1. **Open VS Code settings** (`Cmd/Ctrl + ,`)
2. **Add color customizations** for live preview:
   ```json
   {
     "workbench.colorCustomizations": {
       "editor.background": "#46326e",
       "editor.foreground": "#ffffff99"
     },
     "editor.tokenColorCustomizations": {
       "comments": "#808080",
       "strings": "#ffff00"
     }
   }
   ```
3. **Changes apply instantly** - no reload needed!
4. **Generate theme file** when satisfied:
   - `Cmd/Ctrl + Shift + P` → "Developer: Generate Color Theme from Current Settings"

### Method 2: Extension Development Host

1. **Open extension folder** in VS Code
2. **Press F5** to launch Extension Development Host
3. **Edit theme JSON files** directly
4. **Reload** (`Cmd/Ctrl + R` in dev host) to see changes

### Method 3: Theme JSON Hot Reload

With `-color-theme.json` suffix:
- **Color decorators** show in the JSON
- **Color pickers** for easy adjustment
- **Hover documentation** for color keys

### Method 4: Browser DevTools

For testing specific colors:
1. `Help > Toggle Developer Tools`
2. Inspect VS Code elements
3. Test CSS custom property values

---

## 📦 Development Commands

```bash
cd vscode-extension

# Install dependencies
npm install

# Compile extension
npm run compile

# Build VSIX package
npm run build

# Test in development host
# Press F5 in VS Code with extension folder open

# Reload extension after changes
npm run reload

# Publish to marketplace
npm run publish
```

---

## 🎨 Theme Color Reference

### Full VS Code Theme Color List
See: https://code.visualstudio.com/api/references/theme-color

Key sections to customize:
- **Editor** - Main editing area
- **Sidebar** - File explorer, search
- **Activity Bar** - Left icon bar
- **Status Bar** - Bottom bar
- **Terminal** - Integrated terminal
- **Tabs** - Editor tabs
- **Buttons** - UI buttons
- **Input** - Text inputs
- **Lists** - Dropdown lists
- **Git** - Source control colors

### TextMate Scopes for Syntax
See: https://macromates.com/manual/en/language_grammars

Common scopes:
- `comment` - Code comments
- `string` - String literals
- `constant.numeric` - Numbers
- `keyword` - Language keywords
- `entity.name.function` - Function names
- `variable` - Variables
- `support.function` - Built-in functions
- `punctuation` - Brackets, parens, etc.

---

## 🚀 Release Checklist

- [ ] Create `themes/` directory
- [ ] Create `aesthetic-dark-color-theme.json`
- [ ] Create `aesthetic-light-color-theme.json`
- [ ] Update `package.json` with theme contributions
- [ ] Test in Extension Development Host
- [ ] Test both themes in various file types:
  - [ ] JavaScript/TypeScript (.js, .ts, .mjs)
  - [ ] KidLisp (.lisp) ✨
  - [ ] PeaceScript (.pjs)
  - [ ] JSON
  - [ ] Markdown
- [ ] Update README with theme screenshots
- [ ] Bump version in package.json
- [ ] Build and publish: `npm run publish`

---

## 💡 Advanced Ideas

### Semantic Token Colors
For enhanced JavaScript/TypeScript highlighting:
```json
{
  "semanticHighlighting": true,
  "semanticTokenColors": {
    "variable.readonly": "#00ffff",
    "parameter": "#ffa500",
    "function.declaration": "#ff69b4"
  }
}
```

### KidLisp-Specific Theme Colors

The themes should enhance KidLisp highlighting with these colors:

```json
{
  "tokenColors": [
    {
      "scope": "comment.line.semicolon.kidlisp",
      "settings": { "foreground": "#808080", "fontStyle": "italic" }
    },
    {
      "scope": "constant.numeric.timing.kidlisp",
      "settings": { "foreground": "#ffff00" }
    },
    {
      "scope": "variable.other.embedded.kidlisp",
      "settings": { "foreground": "#32cd32" }
    },
    {
      "scope": "entity.name.tag.painting.kidlisp",
      "settings": { "foreground": "#ff8c00" }
    },
    {
      "scope": "support.constant.rainbow.kidlisp",
      "settings": { "foreground": "#ff69b4", "fontStyle": "bold" }
    },
    {
      "scope": "entity.name.function.graphics.kidlisp",
      "settings": { "foreground": "#00ffff" }
    },
    {
      "scope": "entity.name.function.control.kidlisp",
      "settings": { "foreground": "#00ff00" }
    },
    {
      "scope": "entity.name.function.math.kidlisp",
      "settings": { "foreground": "#ff69b4" }
    },
    {
      "scope": "variable.language.builtin.kidlisp",
      "settings": { "foreground": "#87ceeb" }
    },
    {
      "scope": "support.constant.color.kidlisp",
      "settings": { "foreground": "#ffa500" }
    }
  ]
}
```

### Icon Theme (Future)
Consider creating matching file/folder icons:
```json
{
  "contributes": {
    "iconThemes": [
      {
        "id": "aesthetic-icons",
        "label": "Aesthetic Computer Icons",
        "path": "./icons/aesthetic-icon-theme.json"
      }
    ]
  }
}
```

---

## 📚 Resources

- [VS Code Color Theme Guide](https://code.visualstudio.com/api/extension-guides/color-theme)
- [Theme Color Reference](https://code.visualstudio.com/api/references/theme-color)
- [Syntax Highlighting Guide](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide)
- [TextMate Language Grammars](https://macromates.com/manual/en/language_grammars)
- [Extension Publishing](https://code.visualstudio.com/api/working-with-extensions/publishing-extension)

---

## 🎯 Quick Start

To start live-developing a theme RIGHT NOW:

1. Add this to your VS Code settings (`settings.json`):
```json
{
  "workbench.colorCustomizations": {
    "editor.background": "#46326e",
    "editor.foreground": "#ffffffcc",
    "editorCursor.foreground": "#c81e64",
    "activityBar.background": "#2d1f47",
    "sideBar.background": "#3a2959",
    "statusBar.background": "#32cd32",
    "terminal.background": "#46326e"
  },
  "editor.tokenColorCustomizations": {
    "comments": "#808080",
    "strings": "#ffff00",
    "numbers": "#ffc0cb",
    "keywords": "#00ff00",
    "functions": "#00ffff"
  }
}
```

2. See changes instantly!

3. When happy, run: `Developer: Generate Color Theme from Current Settings`

4. Save the generated JSON to `vscode-extension/themes/`

---

*Created: December 24, 2025*
*For: Aesthetic Computer VS Code Extension*
