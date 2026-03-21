# Aesthetic Computer Electron Build Credentials

This directory contains signing certificates and credentials for building and distributing the Aesthetic Computer desktop app across all platforms.

## Contents

### macOS Code Signing
- `mac-developer-id.p12` - Apple Developer ID certificate (export from Xcode/Keychain)
- `mac-developer-id-password.txt` - Password for the .p12 file
- `mac-provisioning-profile.provisionprofile` - Provisioning profile (if needed)
- `apple-id.txt` - Apple ID email for notarization
- `apple-app-specific-password.txt` - App-specific password for notarization

### Windows Code Signing
- `windows-code-signing.pfx` - Windows Authenticode certificate
- `windows-code-signing-password.txt` - Password for the .pfx file

### Linux
No signing required for Linux builds (AppImage, deb, rpm are unsigned by default)

### Build Secrets
- `.env` - Environment variables for build process:
  ```bash
  # macOS
  CSC_LINK=./mac-developer-id.p12
  CSC_KEY_PASSWORD=<from mac-developer-id-password.txt>
  APPLE_ID=<from apple-id.txt>
  APPLE_APP_SPECIFIC_PASSWORD=<from apple-app-specific-password.txt>
  APPLE_TEAM_ID=<your Apple Team ID>

  # Windows
  WIN_CSC_LINK=./windows-code-signing.pfx
  WIN_CSC_KEY_PASSWORD=<from windows-code-signing-password.txt>

  # Publishing
  GH_TOKEN=<GitHub Personal Access Token with repo scope>
  ```

## Usage

The `build-all-platforms.fish` script in the parent `ac-electron/` directory will:
1. Source credentials from this vault
2. Build for all platforms using Docker containers
3. Sign apps with appropriate certificates
4. Publish to GitHub releases

## Getting Certificates

### macOS Developer ID Certificate
1. Log into https://developer.apple.com
2. Go to Certificates, Identifiers & Profiles
3. Create a Developer ID Application certificate
4. Download and install in Keychain Access
5. Export as .p12 with a strong password
6. Store .p12 and password in vault

### Windows Code Signing Certificate
1. Purchase from a CA (DigiCert, Sectigo, etc.)
2. Download the .pfx file
3. Store .pfx and password in vault

### Apple App-Specific Password
1. Log into https://appleid.apple.com
2. Go to Security > App-Specific Passwords
3. Generate a new password for "Aesthetic Computer Electron Builder"
4. Store in vault

## Security Notes

- **NEVER commit certificates to git**
- `.p12` and `.pfx` files should be mode 600 (owner read/write only)
- Passwords stored in separate files for easier rotation
- This directory is gitignored in the parent repo
