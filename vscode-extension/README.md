# Aesthetic Computer - VS Code Extension

Live code, preview, and publish interactive pieces on [aesthetic.computer](https://aesthetic.computer) directly from VS Code.

## ✨ Features

### 🎨 Live Preview Panel
- Embedded `aesthetic.computer` preview right in your sidebar
- Real-time piece execution as you save files
- Seamless authentication with your Aesthetic Computer account

### 💻 Local Development Mode
- Connect to your local Netlify dev server (`localhost:8888`)
- Smart server detection - shows "Waiting for server..." until ready
- Auto-refreshes when local server becomes available
- Toggle with command: `Aesthetic Computer: Local Server`

### 🚀 One-Click Publishing
- Run a piece and type `publish` in the prompt to go live
- Your piece gets a unique URL on `aesthetic.computer`

### 🔗 Jump Integration
- Connect VSCode to your browser via WebSocket
- Use the `ac` command to jump between pieces seamlessly

### 🌈 KidLisp.com Window
- Open a dedicated KidLisp editor window
- Perfect for creative coding with Lisp-style syntax

### 📚 Documentation Hover
- Hover over API functions for inline documentation
- Quick access to the full docs via command palette

## 🛠️ Commands

| Command | Description |
|---------|-------------|
| `Aesthetic Computer: Run Piece` | Execute the current file as a piece |
| `Aesthetic Computer: Local Server` | Toggle local development mode |
| `Aesthetic Computer: Clear Slug` | Clear the stored piece URL |
| `Aesthetic Computer: Open Docs` | Browse system documentation |
| `Aesthetic Computer: Open Aesthetic Window` | Open piece in separate window |
| `Aesthetic Computer: Open KidLisp Window` | Open KidLisp.com editor |

## 🏁 Getting Started

1. **Install** the extension from the VS Code Marketplace
2. **Click** the Aesthetic Computer icon in the sidebar
3. **Save** any `.mjs` file to run it as a piece
4. **Authenticate** to publish and save your work

## 💡 Tips

- Use `channel` command in the prompt to sync multiple devices
- Open Developer Tools (`Help > Toggle Developer Tools`) for debugging
- In local mode, run `ac-site` to start your dev server

## 🔧 Local Development Setup

For local development with the full Aesthetic Computer stack:

```bash
# Start the local server
ac-site

# The extension will auto-detect when localhost:8888 is ready
```

## 📬 Support

Need help? Reach out to `mail@aesthetic.computer`

## 📜 History

This extension has evolved significantly since its inception:

- **January 2023** - Initial development began with VS Code integration exploration
- **June 2023** - First published version with live reload, code channels, and publish functionality
- **July 2023** - Added guest piece routing and publish button improvements
- **2024** - Major updates including Codespaces support, painting upload progress, ATProto federation
- **v1.190.0** - Full GitHub Codespaces support with SSL fixes
- **v1.191.0** - CSP violation fixes and improved URL construction
- **v1.194.0** - Game Boy ROM support and Option E implementation
- **v1.195.0** - KidLisp.com dedicated window, pointer lock support
- **v1.198.0** - Local server detection with "waiting for server" UI
- **v1.199.0** - This comprehensive documentation update

The extension is actively maintained as part of the [Aesthetic Computer](https://github.com/whistlegraph/aesthetic-computer) project.
