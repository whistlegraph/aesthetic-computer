const ACTION_EVENTS = Object.freeze({
  live_started: "ac_oskiewar_live_started",
  live_viewed: "ac_oskiewar_live_viewed",
  match_completed: "ac_oskiewar_match_completed",
  match_started: "ac_oskiewar_match_started",
  replay_viewed: "ac_oskiewar_replay_viewed",
  round_followed: "ac_oskiewar_round_followed",
  round_stored: "ac_oskiewar_round_stored",
  spectator_joined: "ac_oskiewar_spectator_joined",
});

const PROPERTY_VALUES = Object.freeze({
  duration_bucket: new Set(["under_15s", "15_29s", "30_59s", "60s_or_more"]),
  input_family: new Set(["gamepad", "keyboard", "xbox", "unknown"]),
  opponent_type: new Set(["dummy", "local-player"]),
  phase: new Set(["fight", "intro", "match", "replay", "round", "select"]),
  result: new Set(["tie", "win"]),
  round_position: new Set(["first", "followup"]),
  source_system: new Set(["browser", "lith", "session-server"]),
  surface: new Set(["macos", "web", "xbox", "unknown"]),
  viewer_state: new Set(["live", "waiting"]),
});

export function oskiewarSurface(value) {
  const surface = String(value || "").toLowerCase();
  return PROPERTY_VALUES.surface.has(surface) ? surface : "unknown";
}

export function oskiewarEvent(action, properties = {}) {
  const event = ACTION_EVENTS[action];
  if (!event) return null;
  const safe = {};
  for (const [key, allowed] of Object.entries(PROPERTY_VALUES)) {
    const value = String(properties[key] || "").toLowerCase();
    if (allowed.has(value)) safe[key] = value;
  }
  return { event, properties: safe };
}

export function oskiewarDurationBucket(durationTicks) {
  const seconds = Math.max(0, Number(durationTicks) || 0) / 60;
  if (seconds < 15) return "under_15s";
  if (seconds < 30) return "15_29s";
  if (seconds < 60) return "30_59s";
  return "60s_or_more";
}

export function oskiewarReplayProperties(demo, surface) {
  return {
    source_system: "lith",
    surface: oskiewarSurface(surface),
    opponent_type: demo?.fighters?.some(
      (fighter) => String(fighter).toUpperCase() === "DUMMY",
    )
      ? "dummy"
      : "local-player",
    round_position: Number(demo?.roundIndex) > 0 ? "followup" : "first",
    duration_bucket: oskiewarDurationBucket(demo?.durationTicks),
    result: demo?.winner ? "win" : "tie",
  };
}

export function oskiewarMatchCompleted(demo) {
  return (
    Array.isArray(demo?.finalRoundWins) && Math.max(...demo.finalRoundWins) >= 5
  );
}
