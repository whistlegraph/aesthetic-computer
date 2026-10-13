// A MIME card owns one stored round. It never joins or publishes a live room.
export function createMimeReplay(name, { scope = globalThis, fetchImpl = globalThis.fetch.bind(globalThis) } = {}) {
  if (!/^[a-z0-9]+(?:-[a-z0-9]+){0,3}$/.test(name) || name.length > 77)
    throw new Error("Replay unavailable");
  let driver, stopped = false, drawn = 0, ready = false;
  let playing = new URLSearchParams(scope.location.search).get("warm") !== "1";
  const active = () => playing && !scope.document.hidden && !stopped;
  const settle = () => {
    scope.__mimeAudioPlaying = ready && active();
    scope.__oskiewarSfx?.mute(!scope.__mimeAudioPlaying);
    scope.__oskiewarVoice?.mute(!scope.__mimeAudioPlaying);
    scope.__mimeAudioSettle?.(scope.__mimeAudioPlaying);
    if (driver && ready) {
      driver.setTimeScale(active() ? 1 : 0);
      driver.setVisible(active());
    }
  };
  scope.__mimePlayerSettle = play => { playing = play === true; settle(); };
  scope.__mimeSettle ||= scope.__mimePlayerSettle;
  scope.addEventListener("message", event => {
    if (event.source !== scope.parent || scope.parent === scope) return;
    if (event.data?.type === "mime:playing") scope.__mimePlayerSettle(event.data.playing);
    if (event.data?.type === "ac:start-audio" && active()) settle();
  });
  scope.document.addEventListener("visibilitychange", settle);
  scope.addEventListener("pagehide", () => { stopped = true; settle(); });
  const announce = error => {
    ready = true; scope.__mimeReady = true; settle();
    scope.parent.postMessage({ type: "mime:ready", ...(error ? { error: true } : {}) }, "*");
  };
  settle();
  return {
    name,
    start(listener) {
      const controller = new AbortController();
      fetchImpl("/api/oskiewar-replays?id=" + encodeURIComponent("ow-" + name),
        { signal: controller.signal, credentials: "omit" })
        .then(async response => {
          if (!response.ok) throw new Error("Replay unavailable");
          const { replay } = await response.json();
          if (stopped) return;
          if (replay?.format !== "ac.oskiedemo" || replay.version !== 1 ||
              replay.game !== "oskiewar" || !replay.checkpoints?.length ||
              replay.checkpoints.some(row => ![26, 32].includes(row.length)))
            throw new Error("Replay unavailable");
          // The saved checkpoint stream supplies movement. Current physics is
          // never substituted for an old or incorrectly stamped simulation.
          scope.__oskiewarResim = false;
          listener({ type: "demo", roundName: name, content: replay, live: false });
        }).catch(error => {
          if (stopped || error.name === "AbortError") return;
          const note = scope.document.createElement("p");
          note.className = "mime-replay-error"; note.textContent = "Replay unavailable";
          scope.document.body.append(note); announce(true);
        });
      return () => { stopped = true; controller.abort(); settle(); };
    },
    bind(value) { driver = value; settle(); },
    painted() {
      if (!ready && scope.__oskiewarReplayReady && ++drawn >= 2) announce(false);
    },
  };
}
