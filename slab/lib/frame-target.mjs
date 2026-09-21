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

export function nativeInputRequest(before, observationId, click) {
  // Compact AX evidence has its own explicit kind; it is never labeled pixels.
  if (before?.capture === 'verified' && before.observation?.kind === 'ax-verification' &&
      before.nativeCapabilities?.includes('ax-verify-v1') && before.nativeInput?.verification?.ok === true) {
    assertFrameTarget(before, { ...before, capture: 'ok' }, observationId);
  } else {
    assertFrameTarget(before, before, observationId);
  }
  const request = { observationId: before.observation.id };
  if (click) {
    const { x, y, count, settleMs } = click;
    if (![x, y, settleMs].every(Number.isFinite) || !Number.isInteger(count) ||
        count < 1 || count > 3 || settleMs < 0 || settleMs > 1000) {
      throw new Error('Invalid native click; no action sent');
    }
    if (click.holdMs !== undefined && (!before.nativeCapabilities?.includes('click-hold-v1') ||
        !Number.isFinite(click.holdMs) || click.holdMs < 0 || click.holdMs > 1000)) {
      throw new Error('Invalid or unsupported native holdMs; no action sent');
    }
    if (click.drag && (!before.nativeCapabilities?.includes('guarded-drag-v1') || count !== 1 ||
        ![click.drag.x, click.drag.y, click.drag.durationMs].every(Number.isFinite) ||
        click.drag.durationMs < 0 || click.drag.durationMs > 2000 ||
        (click.drag.releaseMs !== undefined && (!Number.isFinite(click.drag.releaseMs) || click.drag.releaseMs < 0 || click.drag.releaseMs > 1000)))) {
      throw new Error('Invalid or unsupported native drag; no action sent');
    }
    if (click.verify) {
      const v = click.verify;
      if (!before.nativeCapabilities?.includes('ax-verify-v1') ||
          ![v.x, v.y, v.timeoutMs].every(Number.isFinite) ||
          typeof v.role !== 'string' || !v.role.startsWith('AX') || v.role.length > 80 ||
          !['AXValue', 'AXTitle', 'AXDescription'].includes(v.attribute) ||
          typeof v.equals !== 'string' || !v.equals || Buffer.byteLength(v.equals) > 512 ||
          v.timeoutMs < 1 || v.timeoutMs > 2000) throw new Error('Invalid or unsupported native verification; no action sent');
    }
    Object.assign(request, click);
  }
  return request;
}

export function assertNativeInputReceipt(env, request, status) {
  if (env.nativeInput?.observationId !== request.observationId || env.nativeInput?.status !== status ||
      (request.holdMs !== undefined && env.nativeInput?.holdMs !== request.holdMs) ||
      (request.drag && (env.nativeInput?.kind !== 'drag' || env.nativeInput?.releasePosted !== true || env.nativeInput?.durationMs !== request.drag.durationMs ||
        (request.drag.releaseMs !== undefined && env.nativeInput?.releaseMs !== request.drag.releaseMs)))) {
    throw new Error('Native input receipt missing or mismatched; outcome unknown, request not retried');
  }
}
