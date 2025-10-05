const previousBakeTelemetryFlag = typeof globalThis !== "undefined"
  ? globalThis.__DEBUG_KIDLISP_BAKE__
  : undefined;

if (typeof globalThis !== "undefined") {
  globalThis.__DEBUG_KIDLISP_BAKE__ = true;
}

try {
  const { createBakeHarness } = await import("../kidlisp-bake-harness.js");
  const warmHarness = await createBakeHarness({ enableTelemetry: true, passThroughTelemetry: false });
  warmHarness.restore();
} catch (error) {
  console.warn("⚠️  KidLisp telemetry warm-up failed", error);
}

if (typeof jasmine !== "undefined" && jasmine.getEnv) {
  jasmine.getEnv().addReporter({
    jasmineDone() {
      if (typeof globalThis === "undefined") {
        return;
      }

      if (previousBakeTelemetryFlag === undefined) {
        delete globalThis.__DEBUG_KIDLISP_BAKE__;
      } else {
        globalThis.__DEBUG_KIDLISP_BAKE__ = previousBakeTelemetryFlag;
      }
    }
  });
}
