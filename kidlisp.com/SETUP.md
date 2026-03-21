# KidLisp.com Setup Notes

**Last Updated:** January 13, 2026

## Directory Structure

```
/workspaces/aesthetic-computer/
├── kidlisp.com/                    # Documentation & source tracking
│   ├── FEATURES.md                 # Database integration
│   ├── KIDLISP-COM-LLM.md         # LLM context (comprehensive)
│   ├── LAUNCH-STATUS.md           # Launch status & checklist
│   ├── LIVE-RELOAD-PLAN.md        # PostMessage architecture
│   ├── README.md                   # Basic overview
│   └── SETUP.md                    # This file
└── system/
    ├── netlify.toml                # Netlify configuration
    └── public/
        └── kidlisp.com/            # DEPLOYED SOURCE (edit here!)
            ├── index.html          # Main app (~15,000 lines)
            ├── css/main.css        # Modular CSS
            ├── js/                 # Modular JS modules
            └── README.md           # Dev notes
```

## How It Works

1. **Domain**: `kidlisp.com` is configured in Netlify dashboard
2. **Redirects**: Configured in `system/netlify.toml`:
   - `kidlisp.com` → `aesthetic.computer/kidlisp.com/`
   - `kidlisp.com/*` → `aesthetic.computer/kidlisp.com/:splat`
   - SPA fallback: `/kidlisp.com/*` → `/kidlisp.com/index.html`
3. **Content**: Served from `system/public/kidlisp.com/` directory

## Updating the Site

Edit files directly in `system/public/kidlisp.com/`:

```bash
# Make changes to the source
# Edit: system/public/kidlisp.com/index.html

# Commit and push
git add system/public/kidlisp.com/
git commit -m "kidlisp.com: description of changes"
git push
```

Netlify will automatically deploy on push to main.

## Local Development

```bash
# Start the aesthetic.computer dev server
npm run site

# Access at:
# https://localhost:8888/kidlisp.com/
```

## Netlify Configuration

The relevant section in `system/netlify.toml`:

```toml
[[redirects]]
from = "https://kidlisp.com"
to = "https://aesthetic.computer/kidlisp.com/"
status = 200
force = true

[[redirects]]
from = "https://kidlisp.com/*"
to = "https://aesthetic.computer/kidlisp.com/:splat"
status = 200
force = true

[[redirects]]
from = "https://www.kidlisp.com"
to = "https://aesthetic.computer/kidlisp.com/"
status = 200
force = true

[[redirects]]
from = "https://www.kidlisp.com/*"
to = "https://aesthetic.computer/kidlisp.com/:splat"
status = 200
force = true

# SPA fallback for client-side routing
[[redirects]]
from = "/kidlisp.com/*"
to = "/kidlisp.com/index.html"
status = 200
```

## DNS Configuration

In Netlify dashboard for `kidlisp.com`:
- Domain added in Site Settings → Domain Management
- DNS configured via Netlify DNS or external CNAME
- SSL automatically provisioned via Let's Encrypt
