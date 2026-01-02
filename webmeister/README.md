# 🌐 Webmeister

Browser automation and development tools for Aesthetic Computer subsites.

## Structure

```
webmeister/
├── lib/                    # Shared utilities
│   ├── external-chrome.mjs # Chrome on host machine via SSH + CDP
│   └── web.mjs            # VS Code Simple Browser CDP wrapper
├── sites/                  # Site-specific dev tools
│   └── kidlisp.com/
│       └── dev.mjs        # KidLisp.com development server
└── *.mjs                  # General utilities
```

## External Chrome Development

For testing with browser extensions (Temple wallet, etc.), use external Chrome:

### Setup (One-time)

1. Create a Chrome profile called "AestheticDev" with your extensions:
   - Temple Wallet
   - Any other dev tools

2. The profile will be used automatically when starting Chrome via webmeister.

### Usage

```bash
# Open kidlisp.com in external Chrome and watch console
node webmeister/sites/kidlisp.com/dev.mjs watch

# Just open the page
node webmeister/sites/kidlisp.com/dev.mjs open

# Reload the page
node webmeister/sites/kidlisp.com/dev.mjs reload

# Check wallet state
node webmeister/sites/kidlisp.com/dev.mjs wallet

# Evaluate JS
node webmeister/sites/kidlisp.com/dev.mjs eval "window.beacon"
```

### How it works

1. SSH into the Mac host (`jas@host.docker.internal`)
2. Start Chrome with `--remote-debugging-port=9222` and the AestheticDev profile
3. Connect via CDP WebSocket through the SSH tunnel
4. Control and monitor the page

## VS Code Simple Browser

For quick testing without extensions, use VS Code's Simple Browser:

```bash
# Watch VS Code Simple Browser CDP
node webmeister/kidlisp-dev.mjs
```

## Adding New Sites

Create a new directory under `sites/` with a `dev.mjs`:

```bash
mkdir -p webmeister/sites/my-site.com
# Copy and modify from kidlisp.com/dev.mjs
```

## Machine Configuration

SSH host is read from `aesthetic-computer-vault/machines.json`:
- `jeffrey-macbook`: `jas@host.docker.internal`
- `mac-mini`: `jas@192.168.12.27`
