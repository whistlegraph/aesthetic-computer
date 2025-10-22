# Secrets Configuration Guide

This guide explains how to set up the required secrets for the UE5 build automation.

## Required GitHub Secrets

Go to your repository → Settings → Secrets and variables → Actions → New repository secret

### Perforce Secrets

#### `P4_SERVER`
- **Description:** Perforce server address
- **Format:** `ssl:hostname:port` or `hostname:port`
- **Example:** `ssl:perforce.falsework.studio:1666`
- **Dummy value for testing:** `ssl:test-server.example.com:1666`

#### `P4_USER`
- **Description:** Perforce username for build account
- **Format:** Plain text username
- **Example:** `build_bot`
- **Dummy value for testing:** `test_user`

#### `P4_PASSWORD`
- **Description:** Perforce password for build account
- **Format:** Plain text password
- **Example:** `YourSecurePassword123!`
- **Dummy value for testing:** `test_password_123`
- **Security note:** Use a dedicated build account with read-only access

#### `P4_WORKSPACE`
- **Description:** Perforce workspace/client name
- **Format:** Plain text workspace name
- **Example:** `ue5_build_workspace`
- **Dummy value for testing:** `test_workspace`

#### `P4_CLIENT_PATH`
- **Description:** Perforce depot path to sync
- **Format:** `//depot/path/...`
- **Example:** `//depot/UnrealProjects/MyGame/...`
- **Dummy value for testing:** `//depot/test/...`

### Project Secrets

#### `PROJECT_NAME`
- **Description:** Name of the Unreal project
- **Format:** Plain text (no spaces, matches .uproject filename)
- **Example:** `MyAwesomeGame`
- **Dummy value for testing:** `TestProject`

### Azure Secrets (Only if using Azure VM approach)

#### `AZURE_CREDENTIALS`
- **Description:** Azure service principal credentials (JSON)
- **Format:** JSON object
- **Example:**
```json
{
  "clientId": "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
  "clientSecret": "your-client-secret",
  "subscriptionId": "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
  "tenantId": "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
}
```
- **How to create:**
  1. `az login`
  2. `az ad sp create-for-rbac --name ue5-builder --role contributor --scopes /subscriptions/{subscription-id} --sdk-auth`

#### `AZURE_UE5_IMAGE_ID`
- **Description:** Resource ID of pre-configured VM image with UE5
- **Format:** `/subscriptions/{sub}/resourceGroups/{rg}/providers/Microsoft.Compute/images/{name}`
- **Example:** `/subscriptions/.../images/ue5-build-image-v1`

#### `VM_ADMIN_PASSWORD`
- **Description:** Admin password for provisioned VMs
- **Format:** Complex password (12+ chars, upper, lower, numbers, symbols)
- **Example:** `Build$ecure2024!`

## Optional Secrets

### Notification Webhooks

#### `DISCORD_WEBHOOK_URL`
- **Description:** Discord webhook for build notifications
- **Format:** Full webhook URL
- **Example:** `https://discord.com/api/webhooks/123456789/abcdefgh`

#### `SLACK_WEBHOOK_URL`
- **Description:** Slack webhook for build notifications
- **Format:** Full webhook URL
- **Example:** `https://hooks.slack.com/services/T00000000/B00000000/XXXXXXXXXXXX`

## Testing with Dummy Values

For initial testing without real Perforce access:

1. Set up dummy secrets:
```bash
P4_SERVER=ssl:dummy-server.example.com:1666
P4_USER=test_user
P4_PASSWORD=test_pass_123
P4_WORKSPACE=test_workspace
P4_CLIENT_PATH=//depot/test/...
PROJECT_NAME=TestProject
```

2. The workflow will fail at the Perforce sync step, but you can verify:
   - Workflow triggers correctly
   - Runner connects
   - Secrets are accessible
   - Scripts execute

3. When ready for real testing, update with actual values

## Security Best Practices

1. **Use dedicated build accounts**
   - Create a separate Perforce user for builds
   - Limit permissions (read-only if possible)
   - Different password from personal accounts

2. **Rotate credentials regularly**
   - Change passwords every 90 days
   - Update GitHub secrets when changed
   - Audit access logs

3. **Monitor access**
   - Review GitHub Actions logs
   - Check Perforce login attempts
   - Set up alerts for failed authentications

4. **Workspace isolation**
   - Use dedicated workspace for builds
   - Don't mix with development workspaces
   - Clean workspace regularly

## Perforce Build Account Setup

Create a dedicated Perforce account for builds:

```bash
# As Perforce admin
p4 user -f build_bot

# Set up the user with these properties:
User: build_bot
Email: build-bot@falsework.studio
FullName: Build Bot
Password: [generate strong password]

# Create group with limited permissions
p4 group build_bots
# Add read access to depot
```

## Verification Steps

After setting up secrets:

1. **Test Perforce connection locally:**
```powershell
$env:P4PORT = "ssl:your-server:1666"
$env:P4USER = "build_bot"
$env:P4PASSWD = "your_password"
p4 info
p4 login -s
```

2. **Verify workspace:**
```bash
p4 client -o ue5_build_workspace
```

3. **Test sync:**
```bash
p4 sync //depot/YourProject/...#head -n
```

4. **Run test workflow:**
   - Go to Actions tab
   - Select "UE5 Build (Self-Hosted)"
   - Click "Run workflow"
   - Monitor for connection success

## When to Share Real Credentials

**Don't commit to repo!** When you're ready to test with real Perforce:

1. Verify the setup works with dummy values first
2. Share credentials securely (1Password, LastPass, etc.)
3. Add them to GitHub Secrets (only repo admins can see)
4. Test with a manual workflow run
5. Monitor first few builds closely

## Contact

When ready to set up real credentials, share:
- Perforce server address (P4PORT)
- Build account username (P4USER)  
- Build account password (P4PASSWORD)
- Workspace name (or we'll create one)
- Project depot path
- Project name

Use a secure channel (encrypted email, password manager, etc.)
