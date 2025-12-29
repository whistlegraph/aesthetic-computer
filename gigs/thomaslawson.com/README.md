# Thomas Lawson Website Gig

**Domain:** thomaslawson.com  
**Client:** Thomas Lawson  
**Started:** 2024-12-28  
**Status:** Active

## Resources

All credentials, SSH keys, and SSL certificates are stored in:  
`aesthetic-computer-vault/gigs/thomaslawson.com/`

See vault for:
- `credentials.json` - GoDaddy/cPanel login info
- `ssh/` - SSH keypair for server access
- `ssl/` - Let's Encrypt certificates (expires March 29, 2026)
- `SSL-RENEWAL.md` - Renewal process documentation

## Setup Complete

- ✅ Let's Encrypt SSL installed via acme.sh + cPanel automation
- ✅ HTTPS working on thomaslawson.com and www.thomaslawson.com
- ✅ SSH access configured

## Tools

- `webmeister/` - CDP-based Puppeteer automation for GoDaddy/cPanel
- `acme.sh` at `/home/me/.acme.sh/` (renewal email: mail@aesthetic.computer)
