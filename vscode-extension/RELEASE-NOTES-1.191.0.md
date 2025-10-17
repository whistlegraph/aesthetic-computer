# Release Notes - Version 1.191.0

**Release Date:** October 17, 2025

## 🐛 Critical Bug Fixes

### Fixed Double HTTPS Protocol Bug in Codespaces
- **Issue**: Extension was constructing malformed URLs like `https://https://...` in GitHub Codespaces
- **Fix**: Separated protocol construction from domain, preventing double `https://` prefix
- **Impact**: Extension now loads correctly in all Codespace environments

### Fixed Content Security Policy for Dynamic Codespaces
- **Issue**: Each new Codespace has a unique subdomain, causing CSP violations
- **Fix**: Implemented wildcard CSP domains (`https://*.app.github.dev`) to allow any Codespace
- **Impact**: No code changes needed when creating new Codespaces - they just work!

### Fixed Image Loading CSP Violations
- **Issue**: Background images failed to load with "vscode-resource:" protocol errors
- **Fix**: 
  - Updated to use modern `webview.asWebviewUri()` API
  - Moved background image from CSS to inline styles with proper URIs
  - Updated CSP `img-src` to use `webview.cspSource`
- **Impact**: Purple pals background image now loads correctly

### Removed Deprecated Feature Warnings
- **Issue**: Browser console showed "Unrecognized feature: 'pointer-lock'" warning
- **Fix**: Removed deprecated `allow-pointer-lock` and `pointer-lock` iframe attributes
- **Impact**: Cleaner console output, no functionality change

## 📝 Technical Details

### Content Security Policy Updates
- Main webview CSP now uses `style-src ${webview.cspSource} 'unsafe-inline'`
- Image sources updated to `img-src ${webview.cspSource} data:`
- Docs panel CSP updated to `img-src https: data:` for broader compatibility
- Both webviews use wildcard Codespaces domains for frame-src and child-src

### URL Construction Improvements
- Introduced separate `iframeProtocol` variable for cleaner URL assembly
- Codespaces URL detection now properly handles domain without protocol
- Local development URLs unaffected by changes

## 🔧 Files Modified
- `extension.ts` - Core CSP and URL fixes
- `main.css` - Removed hardcoded vscode-resource URL reference
- `CSP-FIX-SUMMARY.md` - Comprehensive documentation of all changes

## ✅ Testing
All changes tested in GitHub Codespaces environment:
- ✓ Webview loads without CSP errors
- ✓ Iframe content loads correctly
- ✓ Background images display properly
- ✓ No console warnings or errors
- ✓ Works across different Codespace instances

## 🙏 Notes
The sandbox warning about `allow-scripts` and `allow-same-origin` is informational and does not affect functionality. This combination is standard for embedded web content that requires both script execution and same-origin access.
