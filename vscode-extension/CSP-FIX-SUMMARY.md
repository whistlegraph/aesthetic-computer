# VS Code Extension CSP & URL Fixes

## Date: October 17, 2025

## Issues Fixed

### 1. 🔴 Double HTTPS Protocol Bug
**Error**: `Refused to frame 'https://https/'`

**Root Cause**: In Codespaces, the `iframeUrl` variable was constructed WITH the `https://` protocol, then the template string added it again.

**Solution**: 
- Separated protocol from URL construction
- Changed from `iframeUrl = https://${codespaceName}-8888.${codespacesDomain}` to just the domain part
- Added separate `iframeProtocol` variable
- Fixed template to use `${iframeProtocol}${iframeUrl}`

### 2. 🔴 Dynamic Codespaces CSP Issue
**Error**: CSP violations for dynamically created Codespace URLs

**Root Cause**: Each new Codespace gets a unique subdomain. Hardcoding specific Codespace names in CSP doesn't work for future Codespaces.

**Solution**: 
- Use **wildcard domains** in CSP: `https://*.${codespacesDomain}`
- This allows ANY Codespace in the GitHub Codespaces domain to load
- No need for a custom TLD proxy!
- Changed from checking `codespaceName` to just `codespacesDomain`

### 3. 🔴 Image CSP Violation
**Error**: `Refused to load the image 'vscode-resource:/purple-pals.svg'`

**Root Cause**: 
- CSS file used outdated `vscode-resource:` protocol syntax
- CSP only allowed `'self' vscode-resource:` which doesn't match modern webview URIs

**Solution**:
- Generate proper webview URI using `webview.asWebviewUri()` API
- Add image as inline style in HTML instead of in external CSS
- Update CSP to use `img-src ${webview.cspSource} data:` for proper webview support
- Add `'unsafe-inline'` to `style-src` for inline styles
- Commented out the problematic line in `main.css`

### 4. ⚠️ Deprecated Feature Warning
**Warning**: `Unrecognized feature: 'pointer-lock'`

**Solution**:
- Removed deprecated `allow-pointer-lock` from iframe sandbox attribute
- Removed `pointer-lock` from iframe allow attribute
- Modern browsers don't need this permission explicitly set

### 5. ⚠️ Sandbox Security Warning
**Warning**: `An iframe which has both allow-scripts and allow-same-origin for its sandbox attribute can escape its sandboxing`

**Status**: This is a known warning when using both attributes together. It's required for the iframe to function properly with the embedded content. The risk is acceptable given:
- The iframe loads content from trusted domains (aesthetic.computer)
- Content is served over HTTPS
- CSP restricts what can be loaded
- This is standard for embedded web content that needs both script execution and same-origin access

## Key Changes Made

### `/vscode-extension/extension.ts`

1. **Line ~1003**: Split URL construction from protocol
```typescript
let iframeUrl;
let iframeProtocol = "https://";
if (isCodespaces && codespaceName && codespacesDomain) {
  iframeUrl = `${codespaceName}-8888.${codespacesDomain}`;
} else {
  iframeUrl = local ? "localhost:8888" : "aesthetic.computer";
}
```

2. **Line ~1012**: Use wildcard CSP for Codespaces
```typescript
if (isCodespaces && codespacesDomain) {
  const codespaceWildcard = `https://*.${codespacesDomain}`;
  cspFrameSrc += ` ${codespaceWildcard}`;
  cspChildSrc += ` ${codespaceWildcard}`;
}
```

3. **Line ~940**: Add proper image URI generation
```typescript
const purplePalsUri = webview.asWebviewUri(
  vscode.Uri.joinPath(extContext.extensionUri, "resources", "purple-pals.svg"),
);
```

4. **Line ~1026**: Update CSP and add inline background image
```typescript
<meta http-equiv="Content-Security-Policy" content="default-src 'none'; ${cspFrameSrc}; ${cspChildSrc}; style-src ${webview.cspSource} 'unsafe-inline'; script-src 'nonce-${nonce}'; media-src *; img-src ${webview.cspSource} data:;">
...
<style>
  iframe#aesthetic {
    background-image: url('${purplePalsUri}');
  }
</style>
```

5. **Line ~1037**: Fix iframe src URL construction
```typescript
src="${iframeProtocol}${iframeUrl}/${param}${hashFragment}"
```

6. **Line ~238-246**: Apply same wildcard CSP fix to docs panel

### `/vscode-extension/main.css`

Removed problematic hardcoded vscode-resource URL:
```css
/* background-image: url('vscode-resource:/purple-pals.svg'); */
/* Background image is set via inline styles in extension.ts to use proper webview URIs */
```

## Testing

After these changes:
1. ✅ Codespaces URLs load correctly without double `https://`
2. ✅ CSP allows any Codespace subdomain via wildcard
3. ✅ Background images load without CSP violations
4. ✅ No deprecated feature warnings
5. ✅ Future Codespaces will work without code changes

## Future Considerations

- The sandbox warning is informational and can be ignored
- If absolute isolation is needed in the future, consider:
  - Loading content in a separate window
  - Using a proxy service
  - Splitting functionality between sandboxed and non-sandboxed contexts
