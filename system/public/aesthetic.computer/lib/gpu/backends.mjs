// 🎨 GPU Backend Registry
// Manages available renderer backends and handles fallback

/* #region 📚 Notes
  - Registers available backends (WebGPU, WebGL2, ThorVG, Blend2D, Canvas2D)
  - Provides fallback chain when preferred backend unavailable
  - Allows runtime backend switching for testing
#endregion */

import { log } from "../logs.mjs";

const backends = new Map();
let activeRenderer = null;
let gpuCanvas = null;

/**
 * Register a renderer backend factory
 * @param {string} name - Backend name (e.g., "webgpu", "webgl2", "thorvg")
 * @param {function(): Object} factory - Factory function that creates the renderer
 */
export function registerBackend(name, factory) {
  backends.set(name.toLowerCase(), factory);
  log.gpu.debug?.(`Registered backend: ${name}`);
}

/**
 * Create and initialize a renderer by name
 * @param {string} name - Backend name
 * @param {HTMLCanvasElement} canvas - Target canvas
 * @returns {Promise<Object|null>} - Initialized renderer or null
 */
export async function createRenderer(name, canvas) {
  const factory = backends.get(name.toLowerCase());
  if (!factory) {
    log.gpu.warn?.(`Unknown backend: ${name}`);
    return null;
  }

  try {
    const renderer = factory();
    const success = await renderer.init(canvas);
    if (success) {
      log.gpu.log?.(`Initialized ${name} backend`);
      return renderer;
    } else {
      log.gpu.warn?.(`Failed to initialize ${name} backend`);
      return null;
    }
  } catch (err) {
    log.gpu.error?.(`Error creating ${name} backend:`, err);
    return null;
  }
}

/**
 * Initialize the GPU renderer system with fallback chain
 * @param {HTMLCanvasElement} canvas - Target canvas
 * @param {string} preferred - Preferred backend name
 * @returns {Promise<Object|null>} - Active renderer
 */
export async function initGPU(canvas, preferred = "webgpu") {
  gpuCanvas = canvas;

  // Try preferred backend first
  activeRenderer = await createRenderer(preferred, canvas);
  if (activeRenderer) {
    return activeRenderer;
  }

  // Fallback chain
  const fallbacks = ["webgl2", "canvas2d"];
  for (const backend of fallbacks) {
    if (backend === preferred) continue;
    activeRenderer = await createRenderer(backend, canvas);
    if (activeRenderer) {
      log.gpu.log?.(`Fell back to ${backend} backend`);
      return activeRenderer;
    }
  }

  log.gpu.error?.("No GPU backend available!");
  return null;
}

/**
 * Switch to a different backend at runtime
 * @param {string} name - Backend name
 * @returns {Promise<boolean>} - Success
 */
export async function switchBackend(name) {
  if (!gpuCanvas) {
    log.gpu.error?.("No canvas available for backend switch");
    return false;
  }

  // Destroy current renderer
  if (activeRenderer) {
    activeRenderer.destroy();
    activeRenderer = null;
  }

  // Create new renderer
  activeRenderer = await createRenderer(name, gpuCanvas);
  return activeRenderer !== null;
}

/**
 * Get the currently active renderer
 * @returns {Object|null}
 */
export function getActiveRenderer() {
  return activeRenderer;
}

/**
 * List all registered backend names
 * @returns {string[]}
 */
export function listBackends() {
  return [...backends.keys()];
}

/**
 * Check if a specific backend is available
 * @param {string} name - Backend name
 * @returns {boolean}
 */
export function hasBackend(name) {
  return backends.has(name.toLowerCase());
}

/**
 * Handle a render command (dispatches to active renderer)
 * @param {Object} command - Render command
 */
export function handleCommand(command) {
  if (!activeRenderer) {
    console.warn("No active GPU renderer");
    return;
  }
  activeRenderer.handleCommand(command);
}

export default {
  registerBackend,
  createRenderer,
  initGPU,
  switchBackend,
  getActiveRenderer,
  listBackends,
  hasBackend,
  handleCommand,
};
