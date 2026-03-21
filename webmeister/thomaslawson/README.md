# Thomas Lawson Website Scripts

Browser automation scripts for thomaslawson.com WordPress site on GoDaddy hosting.

## Scripts

- check-logo.mjs - Check if logo/images are loading correctly
- click.mjs - Click on thomaslawson.com row in GoDaddy
- cpanel.mjs - cPanel automation helpers
- dns-txt.mjs - Add TXT records for DNS verification
- fill-godaddy.mjs - Generate GoDaddy login snippets
- go-admin.mjs - Navigate to wp-admin
- godaddy-dns.mjs - DNS record management
- godaddy-login.mjs - Automate GoDaddy login
- letsencrypt.mjs - Let's Encrypt SSL setup
- ssl-setup.mjs - Full SSL installation workflow

## Requirements

- Chrome running with --remote-debugging-port=9222
- CDP tunnel running (if in devcontainer)
- Credentials in aesthetic-computer-vault/gigs/thomaslawson.com/credentials.json

## Hosting

- Host: GoDaddy Web Hosting Deluxe
- cPanel: p3plzcpnl502855.prod.phx3.secureserver.net:2083
- PHP: 5.6.40 (needs upgrade)
- WordPress: 6.0.11 (needs upgrade)
