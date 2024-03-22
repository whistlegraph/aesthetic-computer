// 🖐️ Hand Processor
// Runs in a worker
// This seems pretty slow these days... 23.04.28.20.16
// ⚠️ So it's no longer in use (instead we run GPU in the main thread)

let handLandmarker;

async function init() {
  const { HandLandmarker, FilesetResolver } = await import(
    "../dep/@mediapipe/tasks-vision/vision_bundle.js"
  );

  const vision = await FilesetResolver.forVisionTasks(
    "../dep/@mediapipe/tasks-vision/wasm"
  );

  handLandmarker = await HandLandmarker.createFromOptions(vision, {
    baseOptions: {
      modelAssetPath: "../../models/hand_landmarker.task",
      delegate: "GPU",
    },
    canvas: new OffscreenCanvas(0, 0),
    runningMode: "VIDEO",
    minHandDetectionConfidence: 0.25,
    minHandPresenceConfidence: 0.25,
    minTrackingConfidence: 0.25,
    numHands: 1,
  });
}

onmessage = function ({ data: { pixels, width, height } }) {
  let imageData = new ImageData(new Uint8ClampedArray(pixels), width, height);
  const data = handLandmarker?.detectForVideo(imageData, performance.now());
  imageData = null; // Clear this data.
  postMessage(data?.landmarks[0] || []);
};

init();