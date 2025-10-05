import path from "path";
import { pathToFileURL } from "url";

let kidlispModulePromise = null;
let kidlispTelemetryModulePromise = null;

async function loadKidLispModule(enableTelemetry = true) {
  const moduleUrl = pathToFileURL(
    path.resolve(
      process.cwd(),
      "system/public/aesthetic.computer/lib/kidlisp.mjs"
    )
  ).href;

  if (enableTelemetry) {
    if (!kidlispTelemetryModulePromise) {
      const previousTelemetryFlag = globalThis.__DEBUG_KIDLISP_BAKE__;
      globalThis.__DEBUG_KIDLISP_BAKE__ = true;
      const telemetryUrl = `${moduleUrl}?bake-telemetry`;
      kidlispTelemetryModulePromise = import(telemetryUrl).finally(() => {
        if (previousTelemetryFlag === undefined) {
          delete globalThis.__DEBUG_KIDLISP_BAKE__;
        } else {
          globalThis.__DEBUG_KIDLISP_BAKE__ = previousTelemetryFlag;
        }
      });
    }
    return kidlispTelemetryModulePromise;
  }

  if (!kidlispModulePromise) {
    kidlispModulePromise = import(moduleUrl);
  }
  return kidlispModulePromise;
}

function createStub() {
  const fn = (...args) => {
    fn.calls.push(args);
  };
  fn.calls = [];
  return fn;
}

function createMockPaintApi({ width = 256, height = 256 } = {}) {
  const pixels = new Uint8ClampedArray(width * height * 4);
  return {
    screen: {
      width,
      height,
      pixels
    },
    page: createStub(),
    ink: createStub(),
    line: createStub(),
    box: createStub(),
    write: createStub(),
    wipe: createStub(),
    suck: createStub(),
    zoom: createStub(),
    blur: createStub(),
    contrast: createStub()
  };
}

export async function createBakeHarness(options = {}) {
  const {
    passThroughTelemetry = false,
    enableTelemetry = true,
    dimensions = { width: 256, height: 256 }
  } = options;

  const module = await loadKidLispModule(enableTelemetry);
  const KidLispClass = module.KidLisp ?? module.default;
  if (!KidLispClass) {
    throw new Error("KidLisp class export not found");
  }

  const kid = new KidLispClass();
  const paintEvents = [];
  const routingEvents = [];
  const contextEvents = [];

  const originalTracePaint = kid.tracePaintCommand?.bind(kid);
  const originalTraceRouting = kid.traceRoutingTransition?.bind(kid);
  const originalTraceContext = kid.traceFrameRoutingContext?.bind(kid);

  kid.tracePaintCommand = (commandName, args, meta = {}) => {
    paintEvents.push({ commandName, args, meta });
    if (passThroughTelemetry && typeof originalTracePaint === "function") {
      return originalTracePaint(commandName, args, meta);
    }
    return null;
  };

  kid.traceRoutingTransition = (eventName, meta = {}) => {
    routingEvents.push({ eventName, meta });
    if (passThroughTelemetry && typeof originalTraceRouting === "function") {
      return originalTraceRouting(eventName, meta);
    }
    return null;
  };

  if (typeof kid.traceFrameRoutingContext === "function") {
    kid.traceFrameRoutingContext = (label, meta = {}) => {
      contextEvents.push({ label, meta });
      if (passThroughTelemetry && typeof originalTraceContext === "function") {
        return originalTraceContext(label, meta);
      }
      return null;
    };
  }

  const paintApi = createMockPaintApi(dimensions);

  const restore = () => {
    if (typeof originalTracePaint === "function") {
      kid.tracePaintCommand = originalTracePaint;
    }
    if (typeof originalTraceRouting === "function") {
      kid.traceRoutingTransition = originalTraceRouting;
    }
    if (typeof originalTraceContext === "function") {
      kid.traceFrameRoutingContext = originalTraceContext;
    }
  };

  const reset = () => {
    paintEvents.length = 0;
    routingEvents.length = 0;
    contextEvents.length = 0;
  };

  const resolveLayerTarget = (layer) => {
    if (!layer) {
      return null;
    }
    return layer.buffer ?? layer;
  };

  const getOverlayTarget = () => {
    if (!kid.layerManager || typeof kid.layerManager.overlayLayer !== "function") {
      return resolveLayerTarget(kid.postBakeLayer);
    }
    return resolveLayerTarget(kid.layerManager.overlayLayer());
  };

  const getBakedTargets = () => {
    if (!kid.layerManager || typeof kid.layerManager.bakedLayers !== "function") {
      return Array.isArray(kid.bakedLayers) ? kid.bakedLayers.map(resolveLayerTarget) : [];
    }
    return (kid.layerManager.bakedLayers() ?? []).map(resolveLayerTarget);
  };

  return {
    kid,
    paintApi,
    paintEvents,
    routingEvents,
    contextEvents,
    reset,
    restore,
    getOverlayTarget,
    getBakedTargets
  };
}
