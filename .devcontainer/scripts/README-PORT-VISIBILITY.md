# Port Visibility Management

## Automatic Setup

Port 8888 is automatically set to public when the dev container starts via:
- `.devcontainer/scripts/set-port-public.fish` (runs during `entry.fish`)

## Manual Commands

If you need to manually set port 8888 to public:

```fish
# Using the helper script
fish /workspaces/aesthetic-computer/.devcontainer/scripts/set-port-public.fish

# Or using curl directly
curl -X PATCH \
  -H "Authorization: Bearer $GH_TOKEN" \
  -H "Accept: application/vnd.github+json" \
  "https://api.github.com/user/codespaces/$CODESPACE_NAME/ports/8888" \
  -d '{"visibility":"public"}'
```

## Configuration

The port is also configured in `.devcontainer/devcontainer.json`:
- Listed in `forwardPorts` array
- Has `"visibility": "public"` in `portsAttributes`

However, the devcontainer.json setting doesn't always persist across rebuilds, so the script ensures it's set programmatically.
