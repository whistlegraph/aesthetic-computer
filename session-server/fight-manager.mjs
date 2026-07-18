// Fight Lobby, 2026.07.18
// Reliable presence/queue for the rollback-based fight piece.

export class FightManager {
  constructor() {
    this.members = new Map();
    this.roster = [];
    this.sendWS = null;
  }
  setSendFunction(sendWS) { this.sendWS = sendWS; }
  snapshot(forHandle) {
    const players = this.roster.slice(0, 2);
    return { roster: [...this.roster], players, count: this.roster.length,
      role: players.includes(forHandle) ? "player" : "spectator",
      seat: players.indexOf(forHandle) };
  }
  publish() {
    for (const [handle, member] of this.members)
      this.sendWS?.(member.wsId, "fight:roster", this.snapshot(handle));
  }
  join(handle, wsId) {
    if (!handle || !wsId) return;
    const old = this.members.get(handle);
    this.members.set(handle, { wsId, joinedAt: old?.joinedAt || Date.now() });
    if (!this.roster.includes(handle)) this.roster.push(handle);
    this.sendWS?.(wsId, "fight:joined", this.snapshot(handle));
    this.publish();
  }
  leave(handle, wsId) {
    const old = this.members.get(handle);
    if (!old || (wsId && old.wsId !== wsId)) return;
    this.members.delete(handle);
    this.roster = this.roster.filter((candidate) => candidate !== handle);
    this.publish();
  }
}
