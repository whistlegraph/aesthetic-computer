# Release Notes - Version 1.190.0

## 🚀 New Features

### GitHub Codespaces Support
- **Full Codespaces Integration**: The extension now fully supports GitHub Codespaces with automatic URL detection
- **Smart Environment Detection**: Automatically detects when running in Codespaces vs local laptop
- **Dynamic CSP Updates**: Content Security Policy now dynamically allows Codespaces forwarded URLs
- **Toggle Local Development**: The "Toggle Local Development" feature now works correctly in Codespaces

## 🔧 Technical Improvements

### URL Handling
- Automatically constructs correct URLs for Codespaces port forwarding
- Uses `https://{CODESPACE_NAME}-8888.{DOMAIN}` when in Codespaces
- Maintains `https://localhost:8888` behavior on local machines

### Content Security Policy
- Dynamically adds Codespaces URLs to `frame-src` and `child-src` directives
- Ensures iframes load properly in both local and Codespaces environments
- Applied to both documentation panel and main webview

### Environment Detection
Uses these environment variables for Codespaces detection:
- `CODESPACES` - Set to "true" in Codespaces
- `CODESPACE_NAME` - Unique identifier for the Codespace
- `GITHUB_CODESPACES_PORT_FORWARDING_DOMAIN` - Port forwarding domain

## 📦 Dependency Updates

Updated to latest stable versions:
- `engines.vscode`: `^1.88.0` → `^1.105.0`
- `@types/node`: `^20.14.9` → `^24.7.2`
- `@types/vscode`: `^1.88.0` → `^1.105.0`
- `@vscode/test-web`: `^0.0.53` → `^0.0.74`
- `@vscode/vsce`: `^3.6.0` → `^3.6.2`
- `esbuild`: `^0.20.0` → `^0.25.10`
- `typescript`: `^5.5.2` → `^5.9.3`

## 🐛 Bug Fixes

- Fixed iframe loading in Codespaces environment
- Fixed CSP blocking Codespaces forwarded URLs
- Fixed "Toggle Local Development" command in remote environments

## 📝 Related Changes

This release is part of a larger effort to improve Codespaces support across the entire Aesthetic Computer development environment. See `CODESPACES-SSL-FIX.md` in the main repository for details about coordinated changes to:
- Netlify dev server configuration
- Fish shell functions
- URL generation utilities
