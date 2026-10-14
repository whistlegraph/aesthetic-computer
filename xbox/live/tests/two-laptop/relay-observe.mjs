const room = process.argv[2] || "dojo1";
const ws = new WebSocket(`wss://session-server.aesthetic.computer/oskiewar-live?match=ow-${room}&role=viewer&surface=web`);
let done = false;
ws.onmessage = (e) => { let m; try { m = JSON.parse(e.data); } catch { return; }
  if (m.type === "oskiewar:state" && !done) { done = true; const c = m.content; const { fighters, ...rest } = c;
    console.log("envelope keys", Object.keys(m).join(","), "surface", m.surface, m.content.surface);
    console.log("state age s", ((Date.now() - c.at) / 1000).toFixed(0), "top", JSON.stringify(rest).slice(0, 600));
    console.log("fighters", fighters.map(f => `${f.name} alive=${f.alive} x=${f.x}`).join(" | ")); }
  else if (m.type !== "oskiewar:state") console.log("msg", JSON.stringify(m).slice(0, 300));
};
setTimeout(() => { ws.close(); process.exit(0); }, 3000);
