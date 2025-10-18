# Secrets Management - false.work UE5 Builder

## Overview

All secrets for the false.work UE5 builder are stored in **`aesthetic-computer-vault`** (private submodule), NOT in the main `aesthetic-computer` repo.

This ensures credentials are never committed publicly.

## Structure

```
aesthetic-computer-vault/
  └── false.work/
      └── ue5-builder.env       # All secrets stored here

aesthetic-computer/
  └── false.work/
      └── unreal-builder/
          ├── scripts/
          │   ├── load-secrets.sh     # Load secrets from vault
          │   └── load-secrets.fish   # Load secrets from vault (fish)
          └── .gitignore              # Ignores any local secret files
```

## Secret Storage

### File: `aesthetic-computer-vault/false.work/ue5-builder.env`

Contains all credentials:
- VM connection details
- Perforce credentials
- GCP project configuration
- Project settings

**This file is in the vault submodule and never committed to the public repo.**

## Usage

### Loading Secrets (Bash)
```bash
cd /workspaces/aesthetic-computer/false.work/unreal-builder
source scripts/load-secrets.sh
```

### Loading Secrets (Fish)
```fish
cd /workspaces/aesthetic-computer/false.work/unreal-builder
source scripts/load-secrets.fish
```

This sets environment variables:
- `VM_IP`, `VM_USER`, `VM_PASSWORD`
- `P4_SERVER`, `P4_USER`, `P4_PASSWORD`
- `GCP_PROJECT_ID`, `GCP_ZONE`, `GCP_VM_NAME`
- `PROJECT_NAME`, `PROJECT_PATH`

### Using in Scripts

Scripts can load secrets automatically:

```bash
#!/bin/bash
source "$(dirname "$0")/load-secrets.sh"

# Now use the variables
echo "Connecting to $VM_IP"
gcloud compute instances start $GCP_VM_NAME --zone=$GCP_ZONE
```

## GitHub Secrets

For GitHub Actions, secrets must be added manually to the repo:

1. Go to: https://github.com/whistlegraph/aesthetic-computer/settings/secrets/actions
2. Add each secret individually
3. Reference them in workflows with `${{ secrets.SECRET_NAME }}`

**Required GitHub Secrets:**
- `P4_SERVER`
- `P4_USER`
- `P4_PASSWORD`
- `P4_WORKSPACE`
- `P4_CLIENT_PATH`
- `PROJECT_NAME`

## Security Best Practices

### ✅ DO:
- Store all secrets in `aesthetic-computer-vault`
- Use `load-secrets.sh/fish` to load them
- Add sensitive files to `.gitignore`
- Rotate passwords regularly
- Use environment variables in scripts

### ❌ DON'T:
- Commit secrets to `aesthetic-computer` repo
- Hardcode credentials in scripts
- Share passwords in chat/email
- Use the same password for multiple services
- Commit `.env` files with real credentials

## Files Ignored by Git

The `.gitignore` prevents accidental commits:

```
secrets.env
*.secrets
*.credentials
gcp-vm-connection.txt
*.rdp
```

## Updating Secrets

### To Update VM Password
1. Edit `aesthetic-computer-vault/false.work/ue5-builder.env`
2. Update `VM_PASSWORD`
3. Commit to vault repo
4. Reload: `source scripts/load-secrets.sh`

### To Update Perforce Credentials
1. Edit `aesthetic-computer-vault/false.work/ue5-builder.env`
2. Update `P4_USER` and/or `P4_PASSWORD`
3. Commit to vault repo
4. Update GitHub Secrets (if changed)
5. Update password on VM: `p4 set P4PASSWD=newpassword`

## Sharing Secrets with Team

### For false.work team members:

**Option 1: Grant vault access**
- Add them to the `aesthetic-computer-vault` repo
- They clone both repos
- They run `load-secrets.sh`

**Option 2: Secure sharing (one-time)**
- Use 1Password, LastPass, or similar
- Share the `ue5-builder.env` file contents
- They create local copy in their vault

**Option 3: Different credentials**
- Create separate credentials per person
- Each person has their own `ue5-builder-{name}.env`
- Load the appropriate one

## Vault Submodule Setup

If someone new needs access:

```bash
# Clone main repo
git clone https://github.com/whistlegraph/aesthetic-computer.git
cd aesthetic-computer

# Initialize vault submodule (if they have access)
git submodule update --init aesthetic-computer-vault

# Load secrets
cd false.work/unreal-builder
source scripts/load-secrets.sh
```

## Emergency: Rotate All Credentials

If credentials are compromised:

1. **Change VM password:**
   ```bash
   gcloud compute reset-windows-password ue5-builder-falsework \
     --zone=us-central1-a --user=builduser
   ```

2. **Change Perforce password:**
   - Contact Perforce admin
   - Update on VM: `p4 passwd`

3. **Update vault:**
   - Edit `aesthetic-computer-vault/false.work/ue5-builder.env`
   - Commit and push

4. **Update GitHub Secrets:**
   - Go to repo settings
   - Update all affected secrets

5. **Notify team:**
   - Tell everyone to pull latest vault
   - Reload secrets: `source scripts/load-secrets.sh`

## Checklist: Before Committing

Always check before `git push`:

```bash
# Make sure you're not in the vault
pwd
# Should be: /workspaces/aesthetic-computer (NOT aesthetic-computer-vault)

# Check what you're committing
git status
git diff

# Ensure no secrets in diff
git diff | grep -i "password\|secret\|key" && echo "⚠️  WARNING: Possible secret!"

# If clear, commit
git add .
git commit -m "Your message"
git push
```

## Support

Questions about secrets management?
- Check this document
- Review `.gitignore`
- Inspect `load-secrets.sh`
- Ask before committing anything secret-related!
