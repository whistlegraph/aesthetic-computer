# ATProto / Bluesky Vault

Credentials for the @aesthetic.computer Bluesky account.

## Files

- `.env` - ATProto/Bluesky credentials and configuration

## Setup

To get your app password:

1. Go to https://bsky.app/settings/app-passwords
2. Log in as @aesthetic.computer
3. Create new app password (name it "aesthetic-computer-cli")
4. Copy the generated password to `BSKY_APP_PASSWORD` in `.env`

## Usage

Run `devault.fish` from the aesthetic-computer-vault directory to copy
these credentials to `aesthetic-computer/at/.env`
