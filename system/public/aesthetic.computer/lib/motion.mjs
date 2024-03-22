// Motion, 23.08.14.21.05
// Get the accelerometer data from the device.

export function startCapturingMotion() {
  if (window.DeviceMotionEvent) {
    console.log("🏃 Started motion capture.");
    window.addEventListener("devicemotion", handleDeviceMotion);
  }
}

export function stopCapturingMotion() {
  console.log("🏃 Stopped motion capture.");
  window.removeEventListener("devicemotion", handleDeviceMotion);
}

function handleDeviceMotion(event) {
  const x = event.accelerationIncludingGravity.x;
  const y = event.accelerationIncludingGravity.y;
  const z = event.accelerationIncludingGravity.z;

  const motion = {
    accel: { ...event.acceleration },
    accelWithGravity: { ...event.accelerationIncludingGravity },
    rotation: { ...event.rotationRate },
  };

  window.acSEND?.({ type: "motion:update", content: motion });
}
