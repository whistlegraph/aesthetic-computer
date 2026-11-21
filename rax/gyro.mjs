// DeviceOrientation uses intrinsic Z-X-Y rotations. Compare the rear-facing
// direction to the previous phone pose so any grip can be the starting aim.
const radians = Math.PI / 180;
const dot = (a, b) => a[0] * b[0] + a[1] * b[1] + a[2] * b[2];

function axes(alpha, beta, gamma) {
  const a = alpha * radians, b = beta * radians, g = gamma * radians;
  const ca = Math.cos(a), sa = Math.sin(a), cb = Math.cos(b), sb = Math.sin(b), cg = Math.cos(g), sg = Math.sin(g);
  return {
    x: [ca * cg - sa * sb * sg, sa * cg + ca * sb * sg, -cb * sg],
    y: [-sa * cb, ca * cb, sb],
    z: [ca * sg + sa * sb * cg, sa * sg - ca * sb * cg, cb * cg],
  };
}

export function gyroLook(host, active, turn) {
  let previous = null, previousAngle, previousTime, permission = 'idle';
  let samples = 0, valid = 0, turns = 0, disposed = false;
  const reset = () => { previous = null; };
  const status = () => ({ gyroState: permission, gyroSamples: samples, gyroValid: valid, gyroTurns: turns });
  function read({ alpha, beta, gamma, timeStamp }) {
    samples++;
    if (!Number.isFinite(beta) || !Number.isFinite(gamma)) return reset();
    valid++;
    if (!active()) return reset();
    const angle = host.screen.orientation?.angle ?? host.orientation ?? 0;
    const next = axes(Number.isFinite(alpha) ? alpha : 0, beta, gamma);
    if (previous && angle === previousAngle && timeStamp - previousTime < 500) {
      const x = dot(next.z, previous.x), y = dot(next.z, previous.y);
      const c = Math.cos(angle * radians), s = Math.sin(angle * radians);
      const yaw = Math.atan2(c * x - s * y, dot(next.z, previous.z)) / radians;
      const pitch = Math.asin(Math.max(-1, Math.min(1, -s * x - c * y))) / radians;
      if (Math.hypot(yaw, pitch) > 0.08) turns++;
      turn(yaw, pitch);
    }
    previous = next;
    previousAngle = angle;
    previousTime = timeStamp;
  }
  async function enable() {
    const sensor = host.DeviceOrientationEvent;
    if (disposed) return;
    if (!sensor || !host.isSecureContext) { permission = 'unavailable'; return; }
    if (permission !== 'idle' && permission !== 'activation') return;
    permission = 'pending';
    try {
      if (sensor.requestPermission && await sensor.requestPermission() !== 'granted') {
        permission = 'denied';
        return;
      }
      if (disposed) return;
      host.addEventListener('deviceorientation', read);
      permission = 'enabled';
    } catch (error) {
      // A transient activation failure can be retried on the next touch.
      permission = error.name === 'NotAllowedError' ? 'activation' : 'denied';
    }
  }
  function dispose() {
    disposed = true;
    host.removeEventListener('deviceorientation', read);
    reset();
  }
  return { enable, reset, status, dispose };
}
