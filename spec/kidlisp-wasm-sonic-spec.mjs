import { createSonicFrameEngine } from "../kidlisp-wasm/sonic-frame.mjs";
import { getSonicFixture, listSonicFixtures } from "../kidlisp-wasm/sonic-fixtures.mjs";

function mean(values) {
  return values.reduce((sum, value) => sum + value, 0) / Math.max(1, values.length);
}

function range(values) {
  if (values.length === 0) return 0;
  return Math.max(...values) - Math.min(...values);
}

function averageL1Change(series) {
  if (series.length < 2) return 0;
  let total = 0;
  for (let i = 1; i < series.length; i += 1) {
    const previous = series[i - 1];
    const current = series[i];
    let step = 0;
    for (let j = 0; j < current.length; j += 1) {
      step += Math.abs(current[j] - previous[j]);
    }
    total += step / current.length;
  }
  return total / (series.length - 1);
}

function activeExpertCount(series, threshold = 0.12) {
  const sums = new Array(series[0]?.length || 0).fill(0);
  for (const weights of series) {
    for (let i = 0; i < weights.length; i += 1) {
      sums[i] += weights[i];
    }
  }
  return sums.filter((sum) => sum / Math.max(1, series.length) >= threshold).length;
}

function analyzeFixture(name, options = {}) {
  const fixture = getSonicFixture(name);
  if (!fixture) {
    throw new Error(`Unknown fixture ${name}. Available: ${listSonicFixtures().join(", ")}`);
  }

  const width = options.width || 64;
  const height = options.height || 64;
  const frames = options.frames || 24;
  const fps = options.fps || 24;
  const sampleRate = options.sampleRate || 24000;
  const style = options.style || "default";
  const engine = createSonicFrameEngine({
    source: `fixture:${name}`,
    width,
    height,
    fps,
    sampleRate,
    style,
  });

  const frameStats = [];
  for (let frame = 0; frame < frames; frame += 1) {
    const rgba = fixture.render({ width, height, frame, frames });
    const sonicFrame = engine.synthesizeFrame(rgba, frame);
    frameStats.push(sonicFrame.analysis);
  }

  const rms = frameStats.map((entry) => (entry.rmsLeft + entry.rmsRight) * 0.5);
  const stereo = frameStats.map((entry) => entry.stereoSpread);
  const latentFlux = frameStats.map((entry) => entry.latentFlux);
  const expertSeries = frameStats.map((entry) => entry.expertMix);
  const motion = frameStats.map((entry) => entry.motionSpread);

  return {
    meanRms: mean(rms),
    rmsRange: range(rms),
    meanStereoSpread: mean(stereo),
    meanLatentFlux: mean(latentFlux),
    meanMotionSpread: mean(motion),
    expertDrift: averageL1Change(expertSeries),
    activeExperts: activeExpertCount(expertSeries),
  };
}

describe("kidlisp-wasm hybrid latent sonic fixtures", () => {
  it("keeps a pulsing square sonically active", () => {
    const metrics = analyzeFixture("pulse-square");
    expect(metrics.meanRms).toBeGreaterThan(0.05);
    expect(metrics.rmsRange).toBeGreaterThan(0.01);
    expect(metrics.meanStereoSpread).toBeGreaterThan(0.015);
    expect(metrics.meanLatentFlux).toBeGreaterThan(0.02);
    expect(metrics.meanMotionSpread).toBeGreaterThan(0.2);
    expect(metrics.activeExperts).toBeGreaterThanOrEqual(3);
  });

  it("lets a gradient sweep span range without freezing into one decoder", () => {
    const metrics = analyzeFixture("gradient-sweep");
    expect(metrics.meanRms).toBeGreaterThan(0.05);
    expect(metrics.rmsRange).toBeGreaterThan(0.008);
    expect(metrics.meanStereoSpread).toBeGreaterThan(0.01);
    expect(metrics.meanLatentFlux).toBeGreaterThan(0.018);
    expect(metrics.meanMotionSpread).toBeGreaterThan(0.18);
    expect(metrics.activeExperts).toBeGreaterThanOrEqual(3);
  });

  it("keeps orbiting blobs moving through the latent field", () => {
    const metrics = analyzeFixture("orbit-blobs");
    expect(metrics.meanRms).toBeGreaterThan(0.05);
    expect(metrics.meanMotionSpread).toBeGreaterThan(0.2);
    expect(metrics.meanStereoSpread).toBeGreaterThan(0.015);
    expect(metrics.meanLatentFlux).toBeGreaterThan(0.018);
    expect(metrics.activeExperts).toBeGreaterThanOrEqual(3);
  });
});
