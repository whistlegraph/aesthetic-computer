// 🖐️ Hand Processor
// Runs in a worker

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
    },
    runningMode: "VIDEO",
    minHandDetectionConfidence: 0.5,
    minHandPresenceConfidence: 0.5,
    minTrackingConfidence: 0.5,
    numHands: 1,
  });
}

let imageData;

onmessage = function ({ data: { pixels, width, height } }) {
// onmessage = function ({ data: { offscreenCanvas } }) {
  imageData = new ImageData(new Uint8ClampedArray(pixels), width, height);

  // const data = handLandmarker?.detectForVideo(offscreenCanvas, performance.now());
  const data = handLandmarker?.detectForVideo(imageData, performance.now());
  postMessage(data?.landmarks[0] || []);
};

init();
