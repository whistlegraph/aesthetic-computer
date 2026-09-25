// Preserve sharp pixels when the browser limits refresh rather than draw time.
export function renderDensity(height, logicalHeight, devicePixelRatio = 1, scale = 1) {
  return height / logicalHeight * Math.max(1, Math.min(2, devicePixelRatio)) * scale;
}

export function nextResolutionScale(scale, { fps = 60, renderCpuMs = 0 } = {}) {
  // A 30 Hz display or Energy Saver can leave most of every frame idle.
  // Shrinking those frames cannot buy more refreshes and only blurs the image.
  if (fps < 52 && renderCpuMs > 12)
    return Math.max(.62, scale - .12);
  if (fps >= 58 || renderCpuMs < 8)
    return Math.min(1, scale + .06);
  return scale;
}
