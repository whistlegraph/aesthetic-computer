# Environment Setup

The `.env` file for this directory is managed through the **aesthetic-computer-vault** architecture.

## Structure

```
aesthetic-computer-vault/
  └── at/
      ├── .env          # Master credentials (DO NOT COMMIT)
      └── README.md     # Vault documentation

aesthetic-computer/
  └── at/
      └── .env          # Copied from vault (gitignored)
```

## Getting Credentials

The `.env` file is stored in `aesthetic-computer-vault/at/.env` and copied to this directory.

### Initial Setup

If you don't have the app password yet:

1. Go to https://bsky.app/settings/app-passwords
2. Log in as @aesthetic.computer  
3. Create new app password (name: "aesthetic-computer-cli")
4. Copy the password to `aesthetic-computer-vault/at/.env`
5. Run `devault.fish` or manually copy to this directory

### Updating Credentials

To update credentials:

1. Edit `aesthetic-computer-vault/at/.env`
2. Run from vault directory:
   ```bash
   fish devault.fish
   ```
   Or manually copy:
   ```bash
   cp aesthetic-computer-vault/at/.env at/.env
   ```

## Security

- ✅ `.env` is in `.gitignore` (already configured)
- ✅ Never commit credentials to public repos
- ✅ App passwords are safer than account passwords
- ✅ Vault repo should be private and access-controlled

## Current Configuration

```bash
BSKY_SERVICE=https://bsky.social
BSKY_IDENTIFIER=aesthetic.computer
BSKY_APP_PASSWORD=****-****-****-****
BSKY_RELAY=wss://bsky.network
```
