# 🌐 Webmeister

Browser automation tools for web development gigs using CDP in VS Code Simple Browser.

## Tools

- `web.mjs` - Core CDP wrapper
- `fill-godaddy.mjs` - GoDaddy login helper (console snippets)

## Usage

```bash
# Generate login snippets for GoDaddy
node webmeister/fill-godaddy.mjs thomaslawson.com
```

## Credentials

Stored in `aesthetic-computer-vault/gigs/<domain>/credentials.json`

⚠️ Never commit credentials to git - vault is gitignored.
