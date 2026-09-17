// Guard screen-coordinate input against a switched or moved native window.
export function assertFrameTarget(before, after, observationId) {
  if (!before?.observation?.id || (observationId && observationId !== before.observation.id)) {
    throw new Error("Observation is missing or superseded; capture frame again in this session before acting");
  }
  if (before.capture_scope !== "window" || !before.observation.windowId) {
    throw new Error("Native input needs a window-scoped frame; capture frame without screen/crop first");
  }
  const rect = value => value && [value.x,value.y,value.w,value.h].join(",");
  if (after.capture !== "ok" || after.capture_scope !== "window" ||
      before.observation.windowId !== after.observation?.windowId ||
      before.meta?.frontmost?.pid !== after.meta?.frontmost?.pid ||
      rect(before.crop) !== rect(after.crop)) {
    throw new Error("Native target changed or moved; no action sent. Capture frame again");
  }
}
