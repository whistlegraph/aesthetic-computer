# KidLisp.com Setup Notes

## Directory Structure

```
/workspaces/aesthetic-computer/
├── kidlisp.com/              # Source directory for kidlisp.com
│   ├── index.html            # Main landing page
│   └── README.md             # Documentation
└── system/
    ├── netlify.toml          # Netlify configuration with redirects
    └── public/
        └── kidlisp-com.html  # Deployed version (copied from kidlisp.com/index.html)
```

## How It Works

1. **Domain**: `kidlisp.com` is configured in your Netlify dashboard
2. **Redirects**: Configured in `system/netlify.toml`:
   - `kidlisp.com` → `aesthetic.computer/kidlisp-com`
   - `www.kidlisp.com` → `aesthetic.computer/kidlisp-com`
   - `/kidlisp-com` → `/kidlisp-com.html`
3. **Content**: Served from `system/public/kidlisp-com.html`

## Updating the Site

When you make changes to `kidlisp.com/index.html`:

```bash
# Copy the updated file to the public directory
cp kidlisp.com/index.html system/public/kidlisp-com.html

# Commit and push
git add kidlisp.com/ system/public/kidlisp-com.html system/netlify.toml
git commit -m "Update kidlisp.com landing page"
git push
```

Netlify will automatically deploy the changes.

## Similar Domains

This follows the same pattern as:
- `aesthetic.direct` → `system/public/aesthetic-direct.html`

## Netlify Configuration

The relevant section in `system/netlify.toml`:

```toml
[[redirects]]
from = "/kidlisp-com"
to = "/kidlisp-com.html"
status = 200

[[redirects]]
from = "https://kidlisp.com"
to = "https://aesthetic.computer/kidlisp-com"
status = 200
force = true

[[redirects]]
from = "https://www.kidlisp.com"
to = "https://aesthetic.computer/kidlisp-com"
status = 200
force = true
```

## DNS Configuration

In your Netlify dashboard for `kidlisp.com`:
- Add the domain in Site Settings → Domain Management
- Configure DNS or add a CNAME record pointing to your Netlify site
- SSL will be automatically provisioned
