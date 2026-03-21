# VS Code Extension Development

## Quick Publish Workflow

```bash
npm run build    # Compile TypeScript + bundle views
npm run publish  # Bump version + package + publish to marketplace
```

That's it! The `publish` script handles version bumping automatically.

## Scripts Reference

| Script | Description |
|--------|-------------|
| `npm run build` | Compile extension (views + esbuild) |
| `npm run compile` | Alias for build |
| `npm run publish` | Bump patch version + package + publish |
| `npm run package` | Create .vsix without publishing |
| `npm run views` | Generate embedded JS from views/*.js |

## File Structure

- `extension.ts` - Main extension code
- `views/` - Embedded webview JS (process-tree, ast-tree)
- `generated-views.ts` - Auto-generated from views/ (don't edit)
- `out/` - Compiled output (gitignored)
- `themes/` - Color themes
- `syntaxes/` - KidLisp syntax highlighting

## Key Webview Functions

- `getWebViewContent()` - Main AC sidebar panel
- `getKidLispWebViewContent()` - KidLisp.com window
- `getNewsWebViewContent()` - News window
- `getWelcomePanelHtml()` - Welcome/architecture panel

## CSP Notes

Each webview needs proper Content-Security-Policy for:
- `frame-src` / `child-src` - Allow iframes (localhost:8888, aesthetic.computer)
- `script-src` - Use nonces for inline scripts
- `connect-src` - For WebSocket/fetch if needed

## Testing Locally

1. Press F5 in VS Code to launch Extension Development Host
2. Or install the .vsix: `code --install-extension aesthetic-computer-code-X.X.X.vsix`

## Troubleshooting

- **Marketplace shows old version**: Cache lag, wait 2-5 minutes or bump version again
- **TypeScript errors**: These are often strict-mode warnings, build still succeeds
- **Webview blank**: Check CSP frame-src includes the iframe URL
