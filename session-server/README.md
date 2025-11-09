# Session Server Deployment

SSH keys and deployment scripts for session-server.aesthetic.computer (157.245.134.225)

## Initial Setup

1. **Add the public key to the server** (one-time setup):
   
   From the droplet console, run:
   ```bash
   echo 'ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIObYI8+UhYBK2EMjaqmOi4FNTDkYarumR7H6GHViTq/P session-server-deploy' >> ~/.ssh/authorized_keys
   ```

2. **Configure SSH access in your dev container**:
   ```bash
   cd /workspaces/aesthetic-computer/aesthetic-computer-vault/session-server
   ./setup-ssh.fish
   ```

## Deploy Updates

To deploy the latest session server code to production:

```bash
cd /workspaces/aesthetic-computer/aesthetic-computer-vault/session-server
./deploy.fish
```

This will:
1. ✅ Pull latest code from GitHub
2. ✅ Install npm dependencies
3. ✅ Restart the session server with pm2
4. ✅ Show logs to verify deployment

## Files

- `session_server` - Private SSH key (DO NOT COMMIT to main repo)
- `session_server.pub` - Public SSH key
- `deploy.fish` - Deployment script
- `setup-ssh.fish` - SSH configuration script
- `.env` - Environment variables for session server

## Server Info

- **Host**: 157.245.134.225
- **Domain**: session-server.aesthetic.computer
- **User**: root
- **Code Path**: /root/aesthetic-computer/session-server
- **Process Manager**: pm2
- **Service Name**: session-server
