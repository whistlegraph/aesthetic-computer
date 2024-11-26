// Motion, 23.08.14.21.05
// Get the accelerometer data from the device.

export async function startCapturingMotion() {
  if (!window.DeviceMotionEvent) {
    console.warn("❌ DeviceMotionEvent is not supported on this device.");
    return;
  }

  try {
    const permission = await DeviceMotionEvent.requestPermission();
    if (permission === "granted") {
      console.log("🏃 Started motion capture.");
      window.addEventListener("devicemotion", handleDeviceMotion);
    } else {
      console.error("🚫 Motion permission denied.");
    }
  } catch (err) {
    console.error("❌ Error requesting motion permission:", err);
  }
}

export function stopCapturingMotion() {
  console.log("🏃 Stopped motion capture.");
  window.removeEventListener("devicemotion", handleDeviceMotion);
}

function handleDeviceMotion(event) {
  const motion = {
    accel: { ...event.acceleration },
    accelWithGravity: { ...event.accelerationIncludingGravity },
    rotation: { ...event.rotationRate },
  };

  window.acSEND?.({ type: "motion:update", content: motion });
}
