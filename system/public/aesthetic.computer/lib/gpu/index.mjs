// 🎨 GPU Backends Index
// Registers all available renderer backends

import { registerBackend } from "./backends.mjs";

// Import backend factories
import { createCanvas2DBackend } from "./canvas2d-backend.mjs";
import { createWebGL2Backend } from "./webgl2-backend.mjs";
import { createThorVGBackend } from "./thorvg-backend.mjs";
import { createBlend2DBackend } from "./blend2d-backend.mjs";
import { createVelloBackend } from "./vello-backend.mjs";

// Register backends (order matters for fallback)
registerBackend("canvas2d", createCanvas2DBackend);
registerBackend("webgl2", createWebGL2Backend);
registerBackend("thorvg", createThorVGBackend);
registerBackend("blend2d", createBlend2DBackend);
registerBackend("vello", createVelloBackend);

// Re-export everything from backends
export * from "./backends.mjs";

// Export individual factories for direct use
export { createCanvas2DBackend } from "./canvas2d-backend.mjs";
export { createWebGL2Backend } from "./webgl2-backend.mjs";
export { createThorVGBackend } from "./thorvg-backend.mjs";
export { createBlend2DBackend } from "./blend2d-backend.mjs";
export { createVelloBackend } from "./vello-backend.mjs";
