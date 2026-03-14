// cc.mjs — Alias for claude (goes through auth curtain)
function boot({ system }) { system.jump("claude"); }
export { boot };
