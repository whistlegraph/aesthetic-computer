// claude.mjs — Launch Claude Code, with device auth if needed
// If credentials exist on disk, jump straight to terminal:claude.
// Otherwise, redirect to login piece for device-code auth flow.
function boot({ system }) {
  // Check if Claude credentials exist (written by login.mjs)
  const creds = system.readFile?.("/mnt/claude-credentials.json");
  if (creds && creds.length > 10) {
    system.jump("terminal:claude");
  } else {
    system.jump("login");
  }
}
export { boot };
