// Motion, 23.08.14.21.05
// Get the accelerometer data from the device.

export async function startCapturingMotion() {
  if (!window.DeviceMotionEvent) {
    console.log("❌ DeviceMotionEvent is not supported on this device.");
    return;
  }

  // alert("pointer down");

  window.addEventListener(
    "pointerup",
    async () => {
      try {
        const permission = await DeviceMotionEvent.requestPermission();
        if (permission === "granted") {
          console.log("🏃 Started motion capture.");
          window.acSEND?.({ type: "motion:enabled" });
          window.addEventListener("devicemotion", handleDeviceMotion);
        } else {
          console.error("🚫 Motion permission denied.");
        }
      } catch (err) {
        alert(err);
        console.error("❌ Error requesting motion permission:", err);
      }
    },
    { once: true },
  );
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
