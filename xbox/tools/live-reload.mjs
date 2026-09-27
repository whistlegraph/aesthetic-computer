// Supervisor generations restart with the application; they are not global.
export function latestLiveReady(log) {
  const marker = [...String(log).matchAll(
    /AC_NATIVE_LIVE_READY bytes=(\d+) generation=(\d+)/g)].at(-1);
  return marker ? { bytes: Number(marker[1]), generation: Number(marker[2]) } : null;
}

export function freshLiveReady(before, current, publishedBytes) {
  const previous = latestLiveReady(before);
  const ready = latestLiveReady(current);
  if (!ready || (publishedBytes !== undefined && ready.bytes !== publishedBytes)) return null;
  if (previous && ready.generation === previous.generation && ready.bytes === previous.bytes)
    return null;
  return ready;
}
