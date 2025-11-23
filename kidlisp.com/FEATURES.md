# KidLisp.com Database Integration

## New Feature: Load Saved Code from Database

You can now load saved KidLisp code directly from the database by visiting URLs like:

- `https://kidlisp.com/$46k`
- `https://kidlisp.com/$code` (any valid code)

This works exactly like `https://aesthetic.computer/$46k`, providing a seamless editing experience for saved KidLisp pieces.

## How It Works

### URL Detection

When you visit `kidlisp.com/$46k`, the JavaScript code:

1. **Parses the URL path**: Detects the pattern `/\$([a-zA-Z0-9]+)$` in `window.location.pathname`
2. **Extracts the code**: Gets `46k` from `$46k`
3. **Fetches from database**: Calls `/api/store-kidlisp?code=46k`
4. **Loads into editor**: Sets the retrieved source code in Monaco editor
5. **Triggers preview**: Automatically displays the code in the aesthetic.computer iframe

### Implementation

```javascript
// Check URL path for a kidlisp code (e.g., /kidlisp-com/$46k or /$46k)
const urlPath = window.location.pathname;
const codeMatch = urlPath.match(/\/\$([a-zA-Z0-9]+)$/);

if (codeMatch) {
  const code = codeMatch[1];
  console.log(`🔍 Detected kidlisp code in URL: $${code}`);
  
  // Load code from database
  loadKidlispFromDatabase(code).then(source => {
    if (source && editor) {
      editor.setValue(source);
      updatePreview();
    }
  });
}
```

### API Integration

The `loadKidlispFromDatabase` function:

```javascript
async function loadKidlispFromDatabase(code) {
  const cleanCode = code.startsWith('$') ? code.substring(1) : code;
  const isDev = window.location.hostname === 'localhost';
  const baseUrl = isDev ? 'http://localhost:8888' : 'https://aesthetic.computer';
  const url = `${baseUrl}/api/store-kidlisp?code=${cleanCode}`;
  
  const response = await fetch(url);
  if (response.ok) {
    const data = await response.json();
    return data.source;
  }
  return null;
}
```

## Netlify Redirects

Updated `system/netlify.toml` to handle URL parameters:

```toml
[[redirects]]
from = "https://kidlisp.com/*"
to = "https://aesthetic.computer/kidlisp.com/:splat"
status = 200
force = true
```

This preserves the `/$46k` path when redirecting to the aesthetic.computer hosting.

## Use Cases

1. **Share code snippets**: Send someone `https://kidlisp.com/$abc` to show your work
2. **Edit saved pieces**: Load any cached KidLisp code directly into the editor
3. **Browse examples**: Navigate between different saved codes easily
4. **Unified workflow**: Same URL pattern as aesthetic.computer for consistency

## Testing

To test locally:
1. Start the aesthetic.computer dev server (port 8888)
2. Open `http://localhost:8888/kidlisp.com/$46k`
3. The editor should load the code for `$46k` from your local database

## Future Enhancements

- [ ] Add URL update when code changes (autosave)
- [ ] Add code sharing UI (generate $code from editor)
- [ ] Add recent codes browser/gallery
- [ ] Add syntax error highlighting in editor
