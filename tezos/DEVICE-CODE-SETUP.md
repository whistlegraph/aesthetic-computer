# Device Code Flow Setup for CLI Authentication

## Current Status

✅ Implemented Device Code Flow in `ac-login.mjs`  
❌ Auth0 client needs configuration update

## Required Auth0 Configuration

### Enable Device Code Grant Type

1. Visit Auth0 Dashboard: https://manage.auth0.com/dashboard/us/aesthetic/applications
2. Find application: `LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt`
3. Go to **Settings** → **Advanced Settings** → **Grant Types**
4. Check the box for **Device Code**
5. Click **Save Changes**

## Why Device Code Flow?

Device Code Flow is perfect for CLI tools because:

- ✅ **No localhost required** - works in containers, SSH sessions, etc.
- ✅ **No callback URL** - user authenticates on any device
- ✅ **Production-ready** - can ship to any user
- ✅ **Better UX** - shows simple code to enter in browser

## How It Works

1. CLI requests a device code from Auth0
2. Auth0 returns:
   - `user_code` (e.g., "ABCD-EFGH") 
   - `verification_uri` (e.g., "https://auth0.com/activate")
   - `device_code` (internal token for polling)
3. CLI displays code and URL to user
4. User visits URL on any device (phone, browser, etc.)
5. User enters the code and logs in
6. CLI polls Auth0 every 5 seconds
7. Once user completes auth, CLI receives tokens
8. Tokens saved to `~/.ac-token`

## Testing After Configuration

```bash
# Login
node ac-login.mjs

# Check status
node ac-login.mjs status

# View token
node ac-login.mjs token

# Logout
node ac-login.mjs logout
```

## Alternative: Keep Authorization Code Flow

If you can't enable Device Code Flow, we can use the Authorization Code Flow with the hosted callback endpoint we created:

- `/api/auth/cli-callback` endpoint exists
- User needs to add `https://aesthetic.computer/api/auth/cli-callback` to Auth0 allowed callbacks
- Requires `AUTH0_CLIENT_SECRET` environment variable in Netlify

Device Code Flow is cleaner for CLI usage, but both work!
