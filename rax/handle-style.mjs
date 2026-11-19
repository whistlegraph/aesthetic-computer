// AC's saved palette includes the @ character. Keep its exact spelling and RGBs.
export async function handleColors(handle, { fetch = globalThis.fetch } = {}) {
  try {
    const response = await fetch(
      `https://aesthetic.computer/api/handle-colors?handle=${encodeURIComponent(handle)}`,
      { signal: AbortSignal.timeout(5000) },
    );
    if (!response.ok) return null;
    const { colors } = await response.json();
    if (!Array.isArray(colors) || colors.length !== handle.length + 1)
      return null;
    if (
      !colors.every(
        (c) =>
          c &&
          [c.r, c.g, c.b].every(
            (v) => Number.isInteger(v) && v >= 0 && v <= 255,
          ),
      )
    )
      return null;
    return colors.map(({ r, g, b }) => ({ r, g, b }));
  } catch {
    return null;
  }
}
