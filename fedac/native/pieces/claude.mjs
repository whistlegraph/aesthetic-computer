// claude.mjs — "code" command: launches Claude Code in terminal
// Sets a global flag so terminal.mjs knows to spawn claude even if params don't propagate.

function boot({ system }) {
  console.log("[code] boot");
  globalThis.__terminalCmd = "claude";
  system.jump("terminal:claude");
}

export { boot };
