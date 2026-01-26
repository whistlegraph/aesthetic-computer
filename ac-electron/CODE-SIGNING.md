# Code Signing & Release Setup

## Required GitHub Secrets

To enable automated code signing and notarization, add these secrets to your GitHub repository:

### macOS Code Signing & Notarization

1. **MACOS_CERTIFICATE** - Base64-encoded `.p12` certificate file
2. **MACOS_CERTIFICATE_PASSWORD** - Password for the `.p12` certificate
3. **KEYCHAIN_PASSWORD** - Any secure password for the temporary keychain
4. **APPLE_ID** - Your Apple ID email (e.g., `hi@aesthetic.computer`)
5. **APPLE_NOTARIZE_PWD** - App-specific password from appleid.apple.com
6. **APPLE_TEAM_ID** - Your Apple Developer Team ID

### How to Get These Values

#### 1. Export Your Developer ID Certificate

```bash
# List available signing identities
security find-identity -v -p codesigning

# Export to .p12 (you'll be prompted for a password)
security export -t identities -f pkcs12 -o certificate.p12 -k ~/Library/Keychains/login.keychain-db
```

Then base64 encode it:
```bash
base64 -i certificate.p12 | pbcopy
```

Paste that into `MACOS_CERTIFICATE` secret.

#### 2. Find Your Team ID

Your Team ID can be found at:
- https://developer.apple.com/account → Membership Details
- Or in Xcode: Preferences → Accounts → Select team → View Details

#### 3. Create App-Specific Password

1. Go to https://appleid.apple.com/account/manage
2. Sign in with your Apple ID
3. Under "App-Specific Passwords", click "Generate Password"
4. Label it "Aesthetic Computer Electron"
5. Copy the generated password

## Local Development Signing

For local testing with signing (without notarization):

```bash
# Set your identity
export CSC_NAME="Developer ID Application: Your Name (TEAM_ID)"

# Build
npm run build:mac
```

To test notarization locally:

```bash
export APPLE_ID="your@email.com"
export APPLE_NOTARIZE_PWD="xxxx-xxxx-xxxx-xxxx"
export APPLE_TEAM_ID="YOUR_TEAM_ID"
export FORCE_NOTARIZE=1

npm run build:mac
```

## Releasing

### Automatic (via Git tags)

```fish
# Bump version and create tag
./release.fish patch  # or minor, major, or specific version

# Push to trigger release
git push origin main && git push origin electron-v<version>
```

### Manual (via GitHub Actions)

1. Go to Actions → "Electron Release"
2. Click "Run workflow"
3. Optionally specify a version
4. Click "Run workflow"

The release will be created as a **draft** so you can review before publishing.

## Verifying the Signed App

After building, verify the signature:

```bash
# Check code signature
codesign -dv --verbose=4 "dist/mac-universal/Aesthetic Computer.app"

# Verify notarization
spctl -a -t exec -vv "dist/mac-universal/Aesthetic Computer.app"

# Check stapled ticket
stapler validate "dist/mac-universal/Aesthetic Computer.app"
```

## Troubleshooting

### "Developer ID Application" identity not found

Make sure you have a valid Developer ID Application certificate installed:
1. Open Keychain Access
2. Look in "login" keychain → "My Certificates"
3. You should see "Developer ID Application: Your Name (TEAM_ID)"

If missing, download from https://developer.apple.com/account/resources/certificates/list

### Notarization fails with "invalid credentials"

1. Verify your Apple ID is correct
2. Regenerate the app-specific password
3. Make sure your Apple Developer account is in good standing

### "The signature of the binary is invalid"

The app wasn't signed correctly. Check:
1. Certificate is valid and not expired
2. Entitlements file exists and is valid
3. Hardened runtime is enabled

### Build succeeds but app won't open on other Macs

The app might not be notarized. Check the GitHub Actions logs for notarization errors.
