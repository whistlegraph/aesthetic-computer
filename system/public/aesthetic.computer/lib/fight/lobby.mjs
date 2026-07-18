// Client-side state adapter for the session-server fight presence queue.

export function createFightLobby(handle = "guest") {
  const state = { handle, roster: [], players: [], count: 0, role: "spectator", seat: -1 };
  return {
    state,
    join(server) { server?.send("fight:join", { handle }); },
    leave(server) { server?.send("fight:leave", { handle }); },
    receive(type, content) {
      if (type !== "fight:joined" && type !== "fight:roster") return false;
      const data = typeof content === "string" ? JSON.parse(content) : content;
      state.roster = [...(data?.roster || [])];
      state.players = [...(data?.players || [])];
      state.count = Number(data?.count) || 0;
      state.role = data?.role || "spectator";
      state.seat = Number.isInteger(data?.seat) ? data.seat : -1;
      return true;
    },
  };
}
