# Dev Container Updates for PDS Deployment

The `.devcontainer/Dockerfile` has been updated to include tools needed for PDS deployment:

## Added Tools

- **doctl** (v1.109.0) - DigitalOcean CLI for infrastructure management
- **s3cmd** - S3-compatible storage management for DigitalOcean Spaces

## How to Apply Changes

### Option 1: Rebuild Dev Container (Recommended)

1. In VS Code, press `Cmd/Ctrl + Shift + P`
2. Search for "Dev Containers: Rebuild Container"
3. Select and wait for rebuild

### Option 2: Install Manually (Temporary)

If you need the tools immediately without rebuilding:

```bash
# Install doctl
cd /tmp
wget https://github.com/digitalocean/doctl/releases/download/v1.109.0/doctl-1.109.0-linux-amd64.tar.gz
tar xf doctl-*.tar.gz
sudo mv doctl /usr/local/bin
rm doctl-*.tar.gz

# Install s3cmd (already available via dnf)
sudo dnf install -y s3cmd

# Verify installations
doctl version
s3cmd --version
```

### Option 3: Use Next Dev Container Session

The tools will be automatically available next time you:
- Start a new dev container
- Rebuild the workspace
- Restart the codespace

## Testing

After installation/rebuild, verify:

```bash
# Check doctl
doctl version
# Should show: doctl version 1.109.0

# Check s3cmd  
s3cmd --version
# Should show: s3cmd version 2.x.x

# Initialize doctl
doctl auth init
# Enter your DO token from vault
```

## Using the PDS Deployment Scripts

Once tools are available:

```bash
cd /workspaces/aesthetic-computer/at/pds/deployment/digitalocean

# Generate config
fish generate-pds-env.fish

# Deploy
fish deploy.fish
```

---

**Note:** The Dockerfile changes are committed and will be available in all future dev container instances automatically.
