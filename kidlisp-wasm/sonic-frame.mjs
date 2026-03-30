import { createSeededRandom, hashString } from "./runtime.mjs";

const GLOBAL_GRID_X = 4;
const GLOBAL_GRID_Y = 4;
const GLOBAL_FEATURE_COUNT = 13 + GLOBAL_GRID_X * GLOBAL_GRID_Y;
const STATE_LATENT_SIZE = 8;
const CELL_COUNT = 64;
const TILE_GRID_X = 8;
const TILE_GRID_Y = 8;
const TILE_FEATURE_COUNT = 12;
const LATENT_FIELD_CHANNELS = 12;
const OUTPUT_SIZE = 40;
const ADDITIVE_PARTIALS = 6;
const FORMANT_COUNT = 3;
const EXPERT_NAMES = ["tonal", "vocal", "table", "living"];
const TAU = Math.PI * 2;

export const DECODER_NAMES = [...EXPERT_NAMES];

const SOUND_STYLES = {
  default: {
    latentBlend: 0.24,
    fieldBlend: 0.68,
    fieldMemory: 0.64,
    byteMixScale: 0.9,
    petriMixScale: 0.82,
    tonalMixScale: 0.98,
    vocalMixScale: 0.96,
    tableMixScale: 1.08,
    livingMixScale: 0.78,
    byteSoftness: 0.18,
    byteLowpass: 0.66,
    outputSmoothing: 0.58,
    gainScale: 0.92,
    panScale: 1.0,
    exciteScale: 1.0,
    couplingScale: 1.0,
    growScale: 1.0,
    diffuseScale: 1.0,
    sparkleScale: 1.0,
    byteHarmonicsScale: 1.0,
    formantWarmth: 1.0,
    tableWarpScale: 1.0,
  },
  soft: {
    latentBlend: 0.18,
    fieldBlend: 0.6,
    fieldMemory: 0.74,
    byteMixScale: 0.34,
    petriMixScale: 0.74,
    tonalMixScale: 0.88,
    vocalMixScale: 1.12,
    tableMixScale: 0.82,
    livingMixScale: 0.28,
    byteSoftness: 0.78,
    byteLowpass: 0.9,
    outputSmoothing: 0.88,
    gainScale: 0.82,
    panScale: 0.68,
    exciteScale: 0.54,
    couplingScale: 0.68,
    growScale: 0.76,
    diffuseScale: 1.18,
    sparkleScale: 0.72,
    byteHarmonicsScale: 0.58,
    formantWarmth: 1.18,
    tableWarpScale: 0.74,
  },
};

function clamp(value, low, high) {
  return Math.max(low, Math.min(high, value));
}

function lerp(a, b, t) {
  return a + (b - a) * t;
}

function mapSigned(value, low, high) {
  return low + ((value + 1) * 0.5) * (high - low);
}

function tanh(value) {
  return Math.tanh(value);
}

function normalize01(value) {
  return clamp(value, 0, 1) * 2 - 1;
}

function wrap01(value) {
  const wrapped = value % 1;
  return wrapped < 0 ? wrapped + 1 : wrapped;
}

function buildNetwork(seed, inputSize, hiddenSizes, outputSize) {
  const prng = createSeededRandom(seed);
  const sizes = [inputSize, ...hiddenSizes, outputSize];
  const layers = [];

  for (let layerIndex = 0; layerIndex < sizes.length - 1; layerIndex += 1) {
    const inSize = sizes[layerIndex];
    const outSize = sizes[layerIndex + 1];
    const scale = 1 / Math.sqrt(inSize);
    const weights = new Float32Array(inSize * outSize);
    const biases = new Float32Array(outSize);

    for (let i = 0; i < weights.length; i += 1) {
      weights[i] = (prng() * 2 - 1) * scale;
    }

    for (let i = 0; i < biases.length; i += 1) {
      biases[i] = (prng() * 2 - 1) * scale;
    }

    layers.push({ inSize, outSize, weights, biases });
  }

  return { layers };
}

function forwardNetwork(network, input) {
  let activations = input;

  for (let layerIndex = 0; layerIndex < network.layers.length; layerIndex += 1) {
    const layer = network.layers[layerIndex];
    const next = new Float32Array(layer.outSize);
    for (let out = 0; out < layer.outSize; out += 1) {
      let sum = layer.biases[out];
      const weightOffset = out * layer.inSize;
      for (let i = 0; i < layer.inSize; i += 1) {
        sum += layer.weights[weightOffset + i] * activations[i];
      }
      next[out] = tanh(sum);
    }
    activations = next;
  }

  return activations;
}

function colorPolar(r, g, b) {
  const hueAngle = Math.atan2(Math.sqrt(3) * (g - b), 2 * r - g - b);
  const max = Math.max(r, g, b);
  const min = Math.min(r, g, b);
  return {
    hueSin: Math.sin(hueAngle),
    hueCos: Math.cos(hueAngle),
    saturation: max - min,
    value: max,
  };
}

function extractFramebufferFeatures(rgba, width, height) {
  const totalPixels = Math.max(1, width * height);
  const invW = width > 1 ? 1 / (width - 1) : 0;
  const invH = height > 1 ? 1 / (height - 1) : 0;
  const prevRow = new Float32Array(width);
  const gridSums = new Float32Array(GLOBAL_GRID_X * GLOBAL_GRID_Y);
  const gridCounts = new Float32Array(GLOBAL_GRID_X * GLOBAL_GRID_Y);

  let rSum = 0;
  let gSum = 0;
  let bSum = 0;
  let lumSum = 0;
  let lumSqSum = 0;
  let edgeX = 0;
  let edgeY = 0;
  let centroidX = 0;
  let centroidY = 0;
  let diagMain = 0;
  let diagCross = 0;

  for (let y = 0; y < height; y += 1) {
    let leftLum = 0;
    for (let x = 0; x < width; x += 1) {
      const offset = (y * width + x) * 4;
      const r = rgba[offset] / 255;
      const g = rgba[offset + 1] / 255;
      const b = rgba[offset + 2] / 255;
      const lum = r * 0.299 + g * 0.587 + b * 0.114;

      rSum += r;
      gSum += g;
      bSum += b;
      lumSum += lum;
      lumSqSum += lum * lum;
      centroidX += x * invW * lum;
      centroidY += y * invH * lum;

      if (x > 0) edgeX += Math.abs(lum - leftLum);
      if (y > 0) edgeY += Math.abs(lum - prevRow[x]);
      leftLum = lum;
      prevRow[x] = lum;

      if (x <= y * (width / Math.max(1, height))) diagMain += lum;
      else diagCross += lum;

      const cellX = Math.min(GLOBAL_GRID_X - 1, Math.floor((x / Math.max(1, width)) * GLOBAL_GRID_X));
      const cellY = Math.min(GLOBAL_GRID_Y - 1, Math.floor((y / Math.max(1, height)) * GLOBAL_GRID_Y));
      const cellIndex = cellY * GLOBAL_GRID_X + cellX;
      gridSums[cellIndex] += lum;
      gridCounts[cellIndex] += 1;
    }
  }

  const meanR = rSum / totalPixels;
  const meanG = gSum / totalPixels;
  const meanB = bSum / totalPixels;
  const meanLum = lumSum / totalPixels;
  const varianceLum = Math.max(0, lumSqSum / totalPixels - meanLum * meanLum);
  const edgeNormX = edgeX / totalPixels;
  const edgeNormY = edgeY / totalPixels;
  const centroidNormX = lumSum > 1e-6 ? centroidX / lumSum : 0.5;
  const centroidNormY = lumSum > 1e-6 ? centroidY / lumSum : 0.5;
  const colorSpread = (Math.abs(meanR - meanG) + Math.abs(meanG - meanB) + Math.abs(meanB - meanR)) / 3;
  const diagonalBias = (diagMain - diagCross) / Math.max(1e-6, diagMain + diagCross);

  const features = new Float32Array(GLOBAL_FEATURE_COUNT);
  features[0] = normalize01(meanLum);
  features[1] = normalize01(meanR);
  features[2] = normalize01(meanG);
  features[3] = normalize01(meanB);
  features[4] = clamp(varianceLum * 10 - 1, -1, 1);
  features[5] = clamp(edgeNormX * 5 - 1, -1, 1);
  features[6] = clamp(edgeNormY * 5 - 1, -1, 1);
  features[7] = centroidNormX * 2 - 1;
  features[8] = centroidNormY * 2 - 1;
  features[9] = clamp(meanR - meanG, -1, 1);
  features[10] = clamp(meanG - meanB, -1, 1);
  features[11] = clamp(colorSpread * 4 - 1, -1, 1);
  features[12] = clamp(diagonalBias, -1, 1);

  for (let i = 0; i < gridSums.length; i += 1) {
    const average = gridSums[i] / Math.max(1, gridCounts[i]);
    features[13 + i] = average * 2 - 1;
  }

  return features;
}

function extractTileFeatures(rgba, width, height, tileX, tileY) {
  const startX = Math.floor((tileX * width) / TILE_GRID_X);
  const endX = Math.max(startX + 1, Math.floor(((tileX + 1) * width) / TILE_GRID_X));
  const startY = Math.floor((tileY * height) / TILE_GRID_Y);
  const endY = Math.max(startY + 1, Math.floor(((tileY + 1) * height) / TILE_GRID_Y));
  const tileWidth = Math.max(1, endX - startX);
  const tileHeight = Math.max(1, endY - startY);
  const totalPixels = tileWidth * tileHeight;
  const prevRow = new Float32Array(tileWidth);

  let rSum = 0;
  let gSum = 0;
  let bSum = 0;
  let lumSum = 0;
  let lumSqSum = 0;
  let edgeX = 0;
  let edgeY = 0;
  let hueSinSum = 0;
  let hueCosSum = 0;
  let satSum = 0;

  for (let y = startY; y < endY; y += 1) {
    let leftLum = 0;
    for (let x = startX; x < endX; x += 1) {
      const localX = x - startX;
      const offset = (y * width + x) * 4;
      const r = rgba[offset] / 255;
      const g = rgba[offset + 1] / 255;
      const b = rgba[offset + 2] / 255;
      const lum = r * 0.299 + g * 0.587 + b * 0.114;
      const polar = colorPolar(r, g, b);

      rSum += r;
      gSum += g;
      bSum += b;
      lumSum += lum;
      lumSqSum += lum * lum;
      hueSinSum += polar.hueSin;
      hueCosSum += polar.hueCos;
      satSum += polar.saturation;

      if (x > startX) edgeX += Math.abs(lum - leftLum);
      if (y > startY) edgeY += Math.abs(lum - prevRow[localX]);
      leftLum = lum;
      prevRow[localX] = lum;
    }
  }

  const meanR = rSum / totalPixels;
  const meanG = gSum / totalPixels;
  const meanB = bSum / totalPixels;
  const meanLum = lumSum / totalPixels;
  const varianceLum = Math.max(0, lumSqSum / totalPixels - meanLum * meanLum);
  const features = new Float32Array(TILE_FEATURE_COUNT);

  features[0] = normalize01(meanLum);
  features[1] = normalize01(meanR);
  features[2] = normalize01(meanG);
  features[3] = normalize01(meanB);
  features[4] = clamp(varianceLum * 10 - 1, -1, 1);
  features[5] = clamp(edgeX / totalPixels * 6 - 1, -1, 1);
  features[6] = clamp(edgeY / totalPixels * 6 - 1, -1, 1);
  features[7] = clamp(hueSinSum / totalPixels, -1, 1);
  features[8] = clamp(hueCosSum / totalPixels, -1, 1);
  features[9] = clamp(satSum / totalPixels * 2 - 1, -1, 1);
  features[10] = tileX / Math.max(1, TILE_GRID_X - 1) * 2 - 1;
  features[11] = tileY / Math.max(1, TILE_GRID_Y - 1) * 2 - 1;

  return features;
}

function buildPcmField(rgba, width, height) {
  const totalPixels = Math.max(1, width * height);
  const field = new Float32Array(totalPixels * 3);
  let energy = 0;

  for (let pixelIndex = 0; pixelIndex < totalPixels; pixelIndex += 1) {
    const rgbaOffset = pixelIndex * 4;
    const writeOffset = pixelIndex * 3;
    const r = rgba[rgbaOffset] / 127.5 - 1;
    const g = rgba[rgbaOffset + 1] / 127.5 - 1;
    const b = rgba[rgbaOffset + 2] / 127.5 - 1;
    field[writeOffset] = r;
    field[writeOffset + 1] = g;
    field[writeOffset + 2] = b;
    energy += Math.abs(r) + Math.abs(g) + Math.abs(b);
  }

  return {
    field,
    energy: energy / field.length,
  };
}

function samplePcmField(field, phase) {
  const scaled = wrap01(phase) * field.length;
  const index = Math.floor(scaled);
  const nextIndex = (index + 1) % field.length;
  const frac = scaled - index;
  return lerp(field[index], field[nextIndex], frac);
}

function encodeLatentField(rgba, width, height, previousField, globalLatent, encoderNetwork, style) {
  const input = new Float32Array(TILE_FEATURE_COUNT + STATE_LATENT_SIZE + LATENT_FIELD_CHANNELS);
  const nextField = new Float32Array(previousField.length);
  const summary = new Float32Array(LATENT_FIELD_CHANNELS);
  let flux = 0;
  let energy = 0;

  for (let tileY = 0; tileY < TILE_GRID_Y; tileY += 1) {
    for (let tileX = 0; tileX < TILE_GRID_X; tileX += 1) {
      const tileFeatures = extractTileFeatures(rgba, width, height, tileX, tileY);
      const tileIndex = tileY * TILE_GRID_X + tileX;
      const latentOffset = tileIndex * LATENT_FIELD_CHANNELS;
      const previousLatent = previousField.subarray(latentOffset, latentOffset + LATENT_FIELD_CHANNELS);
      input.set(tileFeatures, 0);
      input.set(globalLatent, TILE_FEATURE_COUNT);
      input.set(previousLatent, TILE_FEATURE_COUNT + STATE_LATENT_SIZE);
      const encoded = forwardNetwork(encoderNetwork, input);

      for (let channel = 0; channel < LATENT_FIELD_CHANNELS; channel += 1) {
        const directFeature = tileFeatures[channel % TILE_FEATURE_COUNT];
        const encodedValue = lerp(directFeature, encoded[channel], style.fieldBlend);
        const nextValue = clamp(previousLatent[channel] * style.fieldMemory + encodedValue * (1 - style.fieldMemory), -1, 1);
        nextField[latentOffset + channel] = nextValue;
        summary[channel] += nextValue;
        flux += Math.abs(nextValue - previousLatent[channel]);
        energy += Math.abs(nextValue);
      }
    }
  }

  const divisor = TILE_GRID_X * TILE_GRID_Y;
  for (let channel = 0; channel < summary.length; channel += 1) {
    summary[channel] /= divisor;
  }

  return {
    field: nextField,
    summary,
    flux: flux / nextField.length,
    energy: energy / nextField.length,
  };
}

function sampleLatentField(field, x, y, out) {
  const fx = wrap01(x) * TILE_GRID_X;
  const fy = wrap01(y) * TILE_GRID_Y;
  const x0 = Math.floor(fx) % TILE_GRID_X;
  const y0 = Math.floor(fy) % TILE_GRID_Y;
  const x1 = (x0 + 1) % TILE_GRID_X;
  const y1 = (y0 + 1) % TILE_GRID_Y;
  const tx = fx - Math.floor(fx);
  const ty = fy - Math.floor(fy);

  const index00 = (y0 * TILE_GRID_X + x0) * LATENT_FIELD_CHANNELS;
  const index10 = (y0 * TILE_GRID_X + x1) * LATENT_FIELD_CHANNELS;
  const index01 = (y1 * TILE_GRID_X + x0) * LATENT_FIELD_CHANNELS;
  const index11 = (y1 * TILE_GRID_X + x1) * LATENT_FIELD_CHANNELS;

  for (let channel = 0; channel < LATENT_FIELD_CHANNELS; channel += 1) {
    const a = lerp(field[index00 + channel], field[index10 + channel], tx);
    const b = lerp(field[index01 + channel], field[index11 + channel], tx);
    out[channel] = lerp(a, b, ty);
  }

  return out;
}

function interpretRules(output, features, fieldSummary) {
  const brightness = (features[0] + 1) * 0.5;
  const edge = ((features[5] + 1) * 0.5 + (features[6] + 1) * 0.5) * 0.5;
  const spread = (features[11] + 1) * 0.5;
  const fieldColor = (Math.abs(fieldSummary[1]) + Math.abs(fieldSummary[2]) + Math.abs(fieldSummary[3])) / 3;
  const fieldMotion = (Math.abs(fieldSummary[4]) + Math.abs(fieldSummary[5])) * 0.5;

  return {
    grow: mapSigned(output[8], 0.02, 0.2) * (0.7 + brightness * 0.6),
    diffuse: mapSigned(output[9], 0.01, 0.26) * (0.7 + edge * 0.5),
    decay: mapSigned(output[10], 0.004, 0.07),
    excite: mapSigned(output[11], 0.04, 0.92),
    coupling: mapSigned(output[12], 0.08, 0.88),
    strideA: Math.max(1, Math.round(mapSigned(output[13], 1, 19))),
    strideB: Math.max(1, Math.round(mapSigned(output[14], 3, 29))),
    shiftA: Math.max(1, Math.round(mapSigned(output[15], 2, 9))),
    shiftB: Math.max(1, Math.round(mapSigned(output[16], 3, 13))),
    shiftC: Math.max(1, Math.round(mapSigned(output[17], 4, 17))),
    mulA: Math.max(1, Math.round(mapSigned(output[18], 3, 61))),
    mulB: Math.max(1, Math.round(mapSigned(output[19], 5, 83))),
    mask: Math.max(31, Math.round(mapSigned(output[20], 31, 255))),
    byteMix: mapSigned(output[21], 0.08, 0.8),
    petriMix: mapSigned(output[22], 0.06, 0.76),
    panSkew: clamp(output[23] + features[7] * 0.25, -1, 1),
    sparkle: clamp(spread * 0.55 + brightness * 0.25 + fieldColor * 0.2, 0, 1),
    tonalMix: mapSigned(output[24], 0.16, 0.96),
    vocalMix: mapSigned(output[25], 0.1, 0.92),
    tableMix: mapSigned(output[26], 0.22, 1.05),
    livingMix: mapSigned(output[27], 0.08, 0.88),
    scanRateX: mapSigned(output[28], -0.42, 0.42) * (0.45 + edge * 0.5),
    scanRateY: mapSigned(output[29], -0.42, 0.42) * (0.45 + spread * 0.5),
    scanWarp: mapSigned(output[30], 0.08, 2.4),
    scanOrbit: mapSigned(output[31], 0.02, 0.34),
    basePitch: mapSigned(output[32], 42, 420) * (0.72 + brightness * 0.42 + fieldColor * 0.12),
    pitchSpread: mapSigned(output[33], 0.2, 2.8),
    breath: mapSigned(output[34], 0.04, 0.88),
    formantShift: mapSigned(output[35], 0.78, 1.44),
    tableRate: mapSigned(output[36], 0.24, 2.8),
    tableWarp: mapSigned(output[37], 0.08, 3.2),
    latentDrift: mapSigned(output[38], 0.02, 0.28) * (0.6 + fieldMotion * 0.5),
    stereoDrift: clamp(output[39], -1, 1),
  };
}

function seedPetriDish(cells, features, latent) {
  for (let i = 0; i < cells.length; i += 1) {
    const feature = features[i % features.length];
    const memory = latent[i % latent.length];
    cells[i] = clamp(cells[i] * 0.7 + feature * 0.2 + memory * 0.1, -1, 1);
  }
}

function evolvePetriDish(state, features, rules, sampleIndex) {
  const current = state.cells;
  const next = state.nextCells;
  const latent = state.latent;
  const featureOffset = sampleIndex % features.length;

  for (let i = 0; i < current.length; i += 1) {
    const left = current[(i + current.length - 1) % current.length];
    const center = current[i];
    const right = current[(i + 1) % current.length];
    const feature = features[(featureOffset + i * 3) % features.length];
    const memory = latent[i % latent.length];
    const reagent = feature * rules.excite + memory * rules.coupling;
    const growth = tanh(left * 0.9 + center * (0.4 + rules.sparkle) + right * 0.9 + reagent);
    const diffusion = (left + right - 2 * center) * rules.diffuse;
    next[i] = clamp(center * (1 - rules.decay) + growth * rules.grow + diffusion, -1, 1);
  }

  state.cells = next;
  state.nextCells = current;
}

function bytebeatSample(t, rules, petriByteA, petriByteB) {
  return (
    (((t * rules.mulA) & ((t >> rules.shiftA) | petriByteA)) ^
      ((t * rules.mulB) & (t >> rules.shiftB)) ^
      ((t + petriByteB) >> rules.shiftC)) & rules.mask
  ) & 255;
}

function blendRules(a, b, mix, style) {
  return {
    grow: lerp(a.grow, b.grow, mix) * style.growScale,
    diffuse: lerp(a.diffuse, b.diffuse, mix) * style.diffuseScale,
    decay: lerp(a.decay, b.decay, mix),
    excite: lerp(a.excite, b.excite, mix) * style.exciteScale,
    coupling: lerp(a.coupling, b.coupling, mix) * style.couplingScale,
    strideA: Math.round(lerp(a.strideA, b.strideA, mix)),
    strideB: Math.round(lerp(a.strideB, b.strideB, mix)),
    shiftA: Math.round(lerp(a.shiftA, b.shiftA, mix)),
    shiftB: Math.round(lerp(a.shiftB, b.shiftB, mix)),
    shiftC: Math.round(lerp(a.shiftC, b.shiftC, mix)),
    mulA: Math.round(lerp(a.mulA, b.mulA, mix)),
    mulB: Math.round(lerp(a.mulB, b.mulB, mix)),
    mask: Math.round(lerp(a.mask, b.mask, mix)),
    byteMix: clamp(lerp(a.byteMix, b.byteMix, mix), 0.05, 1.2),
    petriMix: clamp(lerp(a.petriMix, b.petriMix, mix), 0.05, 1.25),
    panSkew: lerp(a.panSkew, b.panSkew, mix),
    sparkle: lerp(a.sparkle, b.sparkle, mix),
    tonalMix: clamp(lerp(a.tonalMix, b.tonalMix, mix), 0.02, 1.2),
    vocalMix: clamp(lerp(a.vocalMix, b.vocalMix, mix), 0.02, 1.2),
    tableMix: clamp(lerp(a.tableMix, b.tableMix, mix), 0.02, 1.2),
    livingMix: clamp(lerp(a.livingMix, b.livingMix, mix), 0.02, 1.2),
    scanRateX: lerp(a.scanRateX, b.scanRateX, mix),
    scanRateY: lerp(a.scanRateY, b.scanRateY, mix),
    scanWarp: lerp(a.scanWarp, b.scanWarp, mix),
    scanOrbit: lerp(a.scanOrbit, b.scanOrbit, mix),
    basePitch: lerp(a.basePitch, b.basePitch, mix),
    pitchSpread: lerp(a.pitchSpread, b.pitchSpread, mix),
    breath: lerp(a.breath, b.breath, mix),
    formantShift: lerp(a.formantShift, b.formantShift, mix),
    tableRate: lerp(a.tableRate, b.tableRate, mix),
    tableWarp: lerp(a.tableWarp, b.tableWarp, mix),
    latentDrift: lerp(a.latentDrift, b.latentDrift, mix),
    stereoDrift: lerp(a.stereoDrift, b.stereoDrift, mix),
  };
}

function normalizeWeights(values) {
  const output = new Float32Array(values.length);
  let sum = 0;

  for (let i = 0; i < values.length; i += 1) {
    const value = Math.max(0.0001, values[i]);
    output[i] = value;
    sum += value;
  }

  for (let i = 0; i < output.length; i += 1) {
    output[i] /= sum;
  }

  return output;
}

function deriveExpertWeights(rules, latentVec, style) {
  return normalizeWeights([
    rules.tonalMix * style.tonalMixScale * (0.52 + (latentVec[0] + 1) * 0.2 + Math.abs(latentVec[6]) * 0.12),
    rules.vocalMix * style.vocalMixScale * (0.48 + (latentVec[1] + 1) * 0.18 + rules.breath * 0.2),
    rules.tableMix * style.tableMixScale * (0.55 + (latentVec[2] + 1) * 0.18 + Math.abs(latentVec[7]) * 0.14),
    rules.livingMix * style.livingMixScale * (0.42 + (latentVec[3] + 1) * 0.18 + rules.sparkle * 0.2),
  ]);
}

function stepAdditive(channelState, latentVec, rules, sampleRate, stereoOffset) {
  const basePitch = clamp(
    rules.basePitch * Math.pow(2, latentVec[0] * rules.pitchSpread * 0.3) * (1 + stereoOffset * 0.015),
    24,
    sampleRate * 0.45,
  );

  let sum = 0;
  let ampSum = 0;

  for (let partial = 0; partial < ADDITIVE_PARTIALS; partial += 1) {
    const ratio = 1 + partial * (0.78 + (latentVec[(partial + 2) % latentVec.length] + 1) * 0.22);
    const detune = 1 + stereoOffset * 0.012 * (partial + 1) + latentVec[(partial + 5) % latentVec.length] * 0.004;
    const frequency = clamp(basePitch * ratio * detune, 24, sampleRate * 0.45);
    channelState.phases[partial] = (channelState.phases[partial] + TAU * frequency / sampleRate) % TAU;
    const amplitude = (0.28 + (latentVec[(partial + 7) % latentVec.length] + 1) * 0.18) / (partial + 1);
    sum += Math.sin(channelState.phases[partial]) * amplitude;
    ampSum += amplitude;
  }

  return ampSum > 0 ? sum / ampSum : 0;
}

function resonatorStep(frequency, bandwidth, input, state, offset, sampleRate) {
  const clampedFrequency = clamp(frequency, 40, sampleRate * 0.45);
  const radius = clamp(Math.exp(-Math.PI * bandwidth / sampleRate), 0.7, 0.9995);
  const coefficient = 2 * radius * Math.cos(TAU * clampedFrequency / sampleRate);
  const output = input + coefficient * state[offset] - radius * radius * state[offset + 1];
  state[offset + 1] = state[offset];
  state[offset] = output;
  return output;
}

function stepVocal(channelState, latentVec, rules, sampleRate, noiseValue, style, stereoOffset) {
  const basePitch = clamp(
    rules.basePitch * (0.45 + (latentVec[4] + 1) * 0.18) * (1 + stereoOffset * 0.02),
    55,
    720,
  );
  channelState.phase = (channelState.phase + TAU * basePitch / sampleRate) % TAU;

  const voiced =
    Math.sin(channelState.phase) * 0.78 +
    Math.sin(channelState.phase * 2 + latentVec[5] * 0.8) * 0.26 +
    Math.sin(channelState.phase * 3 + latentVec[6] * 0.4) * 0.12;
  const aspiration = noiseValue * (0.12 + rules.breath * 0.42) + voiced * (0.86 - rules.breath * 0.34);
  const formantShift = rules.formantShift * style.formantWarmth * (1 + latentVec[7] * 0.08);
  const bandwidthTilt = 1 + Math.abs(latentVec[8]) * 0.5 + rules.breath * 0.35;
  const formants = [
    mapSigned(latentVec[1], 260, 880) * formantShift,
    mapSigned(latentVec[2], 900, 2400) * formantShift,
    mapSigned(latentVec[3], 1800, 3600) * formantShift,
  ];
  const bandwidths = [90, 140, 200].map((value) => value * bandwidthTilt);
  let output = 0;

  for (let index = 0; index < FORMANT_COUNT; index += 1) {
    output += resonatorStep(
      formants[index],
      bandwidths[index],
      aspiration * (0.45 - index * 0.08),
      channelState.resonators,
      index * 2,
      sampleRate,
    );
  }

  return clamp(output * 0.08, -1, 1);
}

function stepTable(channelState, pcmField, latentVec, rules, sampleRate, headX, headY, style, stereoOffset) {
  const playbackHz = clamp(
    rules.basePitch * rules.tableRate * (0.3 + (latentVec[0] + 1) * 0.24) * (1 + stereoOffset * 0.02),
    18,
    sampleRate * 0.45,
  );
  channelState.phase = wrap01(channelState.phase + playbackHz / sampleRate);
  const warpAmount = rules.tableWarp * style.tableWarpScale;
  const warpedPhase = wrap01(
    channelState.phase +
      Math.sin(channelState.phase * TAU * (1.1 + Math.abs(latentVec[3]) * 1.8) + headY * TAU) * 0.025 * warpAmount +
      headX * 0.17 +
      headY * 0.09 +
      latentVec[4] * 0.04,
  );
  const primary = samplePcmField(pcmField, warpedPhase);
  const secondary = samplePcmField(
    pcmField,
    wrap01(warpedPhase * (1.01 + latentVec[5] * 0.03) + latentVec[6] * 0.05 + stereoOffset * 0.01),
  );
  return clamp(primary * 0.72 + secondary * 0.28, -1, 1);
}

function writeAscii(view, offset, text) {
  for (let i = 0; i < text.length; i += 1) {
    view.setUint8(offset + i, text.charCodeAt(i));
  }
}

export function createSonicFrameEngine(options = {}) {
  const source = options.source || "";
  const fps = options.fps || 30;
  const sampleRate = options.sampleRate || 48000;
  const width = options.width || 128;
  const height = options.height || 128;
  const seed = options.seed ?? hashString(source || "kidlisp-wasm-sonic-frame");
  const style = SOUND_STYLES[options.style] || SOUND_STYLES.default;
  const controlNetwork = buildNetwork(seed ^ 0x9e3779b9, GLOBAL_FEATURE_COUNT + STATE_LATENT_SIZE, [32, 32], OUTPUT_SIZE);
  const encoderNetwork = buildNetwork(
    seed ^ 0x85ebca6b,
    TILE_FEATURE_COUNT + STATE_LATENT_SIZE + LATENT_FIELD_CHANNELS,
    [24, 24],
    LATENT_FIELD_CHANNELS,
  );
  const jitter = createSeededRandom(seed ^ 0xc2b2ae35);
  const noise = createSeededRandom(seed ^ 0x27d4eb2f);

  let cells = new Float32Array(CELL_COUNT);
  let nextCells = new Float32Array(CELL_COUNT);
  let globalLatent = new Float32Array(STATE_LATENT_SIZE);
  let latentField = new Float32Array(TILE_GRID_X * TILE_GRID_Y * LATENT_FIELD_CHANNELS);
  let sampleClock = 0;
  let byteLeftState = 0;
  let byteRightState = 0;
  let smoothLeft = 0;
  let smoothRight = 0;
  let previousRules = null;
  const tonalLeftState = { phases: new Float32Array(ADDITIVE_PARTIALS) };
  const tonalRightState = { phases: new Float32Array(ADDITIVE_PARTIALS) };
  const vocalLeftState = { phase: 0, resonators: new Float32Array(FORMANT_COUNT * 2) };
  const vocalRightState = { phase: 0, resonators: new Float32Array(FORMANT_COUNT * 2) };
  const tableLeftState = { phase: jitter() };
  const tableRightState = { phase: jitter() };

  for (let i = 0; i < cells.length; i += 1) {
    cells[i] = jitter() * 2 - 1;
  }

  for (let i = 0; i < globalLatent.length; i += 1) {
    globalLatent[i] = jitter() * 2 - 1;
  }

  for (let i = 0; i < latentField.length; i += 1) {
    latentField[i] = jitter() * 2 - 1;
  }

  return {
    synthesizeFrame(rgba, frameIndex) {
      const features = extractFramebufferFeatures(rgba, width, height);
      const controlInput = new Float32Array(GLOBAL_FEATURE_COUNT + STATE_LATENT_SIZE);
      controlInput.set(features, 0);
      controlInput.set(globalLatent, GLOBAL_FEATURE_COUNT);

      const controlOutput = forwardNetwork(controlNetwork, controlInput);
      const nextGlobalLatent = new Float32Array(STATE_LATENT_SIZE);
      for (let i = 0; i < STATE_LATENT_SIZE; i += 1) {
        nextGlobalLatent[i] = clamp(lerp(globalLatent[i], controlOutput[i], style.latentBlend), -1, 1);
      }
      globalLatent = nextGlobalLatent;

      const { field: nextField, summary: fieldSummary, flux: fieldFlux, energy: fieldEnergy } = encodeLatentField(
        rgba,
        width,
        height,
        latentField,
        globalLatent,
        encoderNetwork,
        style,
      );
      latentField = nextField;

      const rules = interpretRules(controlOutput, features, fieldSummary);
      seedPetriDish(cells, features, globalLatent);
      const pcm = buildPcmField(rgba, width, height);

      const frameStart = Math.round(frameIndex * sampleRate / fps);
      const frameEnd = Math.round((frameIndex + 1) * sampleRate / fps);
      const sampleCount = Math.max(1, frameEnd - frameStart);
      const left = new Float32Array(sampleCount);
      const right = new Float32Array(sampleCount);
      const lastRules = previousRules || rules;
      const state = { cells, nextCells, latent: globalLatent };
      const latentLeft = new Float32Array(LATENT_FIELD_CHANNELS);
      const latentRight = new Float32Array(LATENT_FIELD_CHANNELS);
      const latentMix = new Float32Array(LATENT_FIELD_CHANNELS);
      const expertSums = new Float32Array(EXPERT_NAMES.length);
      let leftPower = 0;
      let rightPower = 0;
      let stereoDiff = 0;
      let motionAccumulator = 0;

      for (let sampleIndex = 0; sampleIndex < sampleCount; sampleIndex += 1) {
        const mix = sampleCount === 1 ? 1 : sampleIndex / (sampleCount - 1);
        const blendedRules = blendRules(lastRules, rules, mix, style);
        evolvePetriDish(state, features, blendedRules, sampleIndex);
        cells = state.cells;
        nextCells = state.nextCells;

        const absoluteTime = sampleClock / sampleRate;
        const scanDrift = frameIndex / Math.max(1, fps) * blendedRules.latentDrift;
        const orbitPhase = absoluteTime * (0.35 + blendedRules.scanWarp * 0.3) + globalLatent[0];
        const orbitX = Math.sin(orbitPhase + globalLatent[1] * 0.7) * blendedRules.scanOrbit;
        const orbitY = Math.cos(orbitPhase * 1.17 + globalLatent[2] * 0.6) * blendedRules.scanOrbit;
        const headX = wrap01((features[7] * 0.5 + 0.5) + scanDrift + absoluteTime * blendedRules.scanRateX + orbitX);
        const headY = wrap01((features[8] * 0.5 + 0.5) - scanDrift + absoluteTime * blendedRules.scanRateY + orbitY);
        const stereoSpread = 0.04 + Math.abs(blendedRules.stereoDrift) * 0.1;
        const leftX = wrap01(headX - stereoSpread + globalLatent[3] * 0.03);
        const leftY = wrap01(headY + stereoSpread * 0.5 + globalLatent[4] * 0.03);
        const rightX = wrap01(headX + stereoSpread + globalLatent[5] * 0.03);
        const rightY = wrap01(headY - stereoSpread * 0.5 + globalLatent[6] * 0.03);

        sampleLatentField(latentField, leftX, leftY, latentLeft);
        sampleLatentField(latentField, rightX, rightY, latentRight);
        for (let i = 0; i < LATENT_FIELD_CHANNELS; i += 1) {
          latentMix[i] = (latentLeft[i] + latentRight[i]) * 0.5;
        }

        const expertWeights = deriveExpertWeights(blendedRules, latentMix, style);
        for (let i = 0; i < expertWeights.length; i += 1) {
          expertSums[i] += expertWeights[i];
        }

        const tonalLeft = stepAdditive(tonalLeftState, latentLeft, blendedRules, sampleRate, -1);
        const tonalRight = stepAdditive(tonalRightState, latentRight, blendedRules, sampleRate, 1);
        const noiseLeft = noise() * 2 - 1;
        const noiseRight = noise() * 2 - 1;
        const vocalLeft = stepVocal(vocalLeftState, latentLeft, blendedRules, sampleRate, noiseLeft, style, -1);
        const vocalRight = stepVocal(vocalRightState, latentRight, blendedRules, sampleRate, noiseRight, style, 1);
        const tableLeft = stepTable(tableLeftState, pcm.field, latentLeft, blendedRules, sampleRate, leftX, leftY, style, -1);
        const tableRight = stepTable(tableRightState, pcm.field, latentRight, blendedRules, sampleRate, rightX, rightY, style, 1);

        const t = sampleClock;
        const petriIndexA = (t * blendedRules.strideA + sampleIndex) % cells.length;
        const petriIndexB = (t * blendedRules.strideB + sampleIndex * 3) % cells.length;
        const petriA = cells[petriIndexA];
        const petriB = cells[petriIndexB];
        const petriByteA = Math.floor((petriA * 0.5 + 0.5) * 255) & 255;
        const petriByteB = Math.floor((petriB * 0.5 + 0.5) * 255) & 255;
        const byteLeftRaw = bytebeatSample(t, blendedRules, petriByteA, petriByteB) / 127.5 - 1;
        const byteRightRaw = bytebeatSample(t + 17, blendedRules, petriByteB, petriByteA) / 127.5 - 1;
        const byteLeftShaped = lerp(byteLeftRaw, Math.sin(byteLeftRaw * Math.PI * 0.5), style.byteSoftness);
        const byteRightShaped = lerp(byteRightRaw, Math.sin(byteRightRaw * Math.PI * 0.5), style.byteSoftness);

        byteLeftState = byteLeftState * style.byteLowpass + byteLeftShaped * (1 - style.byteLowpass);
        byteRightState = byteRightState * style.byteLowpass + byteRightShaped * (1 - style.byteLowpass);

        const livingLeft = clamp(
          byteLeftState * blendedRules.byteMix * style.byteMixScale * style.byteHarmonicsScale +
            petriA * blendedRules.petriMix * style.petriMixScale,
          -1,
          1,
        );
        const livingRight = clamp(
          byteRightState * blendedRules.byteMix * style.byteMixScale * style.byteHarmonicsScale +
            petriB * blendedRules.petriMix * style.petriMixScale,
          -1,
          1,
        );

        const rawLeft =
          tonalLeft * expertWeights[0] * 0.86 +
          vocalLeft * expertWeights[1] * 0.96 +
          tableLeft * expertWeights[2] * 0.92 +
          livingLeft * expertWeights[3] * 0.84;
        const rawRight =
          tonalRight * expertWeights[0] * 0.86 +
          vocalRight * expertWeights[1] * 0.96 +
          tableRight * expertWeights[2] * 0.92 +
          livingRight * expertWeights[3] * 0.84;

        const pan = clamp(0.5 + blendedRules.panSkew * 0.32 * style.panScale, 0.12, 0.88);
        const mixedLeft = rawLeft * (1 - pan * 0.18) + rawRight * pan * 0.08 + petriB * 0.04;
        const mixedRight = rawRight * (0.82 + pan * 0.18) + rawLeft * (1 - pan) * 0.08 + petriA * 0.04;
        const gain = (0.34 + blendedRules.sparkle * 0.06 * style.sparkleScale) * style.gainScale;

        smoothLeft = smoothLeft * style.outputSmoothing + mixedLeft * (1 - style.outputSmoothing);
        smoothRight = smoothRight * style.outputSmoothing + mixedRight * (1 - style.outputSmoothing);

        left[sampleIndex] = clamp(tanh(smoothLeft * gain), -1, 1);
        right[sampleIndex] = clamp(tanh(smoothRight * gain), -1, 1);
        leftPower += left[sampleIndex] * left[sampleIndex];
        rightPower += right[sampleIndex] * right[sampleIndex];
        stereoDiff += Math.abs(left[sampleIndex] - right[sampleIndex]);
        motionAccumulator += Math.abs(orbitX) + Math.abs(orbitY);
        sampleClock += 1;
      }

      previousRules = rules;
      return {
        left,
        right,
        rules,
        features,
        analysis: {
          expertNames: DECODER_NAMES,
          expertMix: Array.from(expertSums, (sum) => sum / sampleCount),
          rmsLeft: Math.sqrt(leftPower / sampleCount),
          rmsRight: Math.sqrt(rightPower / sampleCount),
          stereoSpread: stereoDiff / sampleCount,
          latentFlux: fieldFlux,
          latentEnergy: fieldEnergy,
          pcmEnergy: pcm.energy,
          motionSpread: motionAccumulator / sampleCount,
        },
      };
    },
  };
}

export function encodeStereoWav(leftChunks, rightChunks, sampleRate = 48000) {
  const totalSamples = leftChunks.reduce((sum, chunk) => sum + chunk.length, 0);
  const bytesPerSample = 2;
  const numChannels = 2;
  const dataSize = totalSamples * numChannels * bytesPerSample;
  const buffer = new ArrayBuffer(44 + dataSize);
  const view = new DataView(buffer);

  writeAscii(view, 0, "RIFF");
  view.setUint32(4, 36 + dataSize, true);
  writeAscii(view, 8, "WAVE");
  writeAscii(view, 12, "fmt ");
  view.setUint32(16, 16, true);
  view.setUint16(20, 1, true);
  view.setUint16(22, numChannels, true);
  view.setUint32(24, sampleRate, true);
  view.setUint32(28, sampleRate * numChannels * bytesPerSample, true);
  view.setUint16(32, numChannels * bytesPerSample, true);
  view.setUint16(34, 16, true);
  writeAscii(view, 36, "data");
  view.setUint32(40, dataSize, true);

  let offset = 44;
  for (let i = 0; i < leftChunks.length; i += 1) {
    const left = leftChunks[i];
    const right = rightChunks[i];
    for (let sample = 0; sample < left.length; sample += 1) {
      view.setInt16(offset, clamp(left[sample], -1, 1) * 0x7fff, true);
      offset += 2;
      view.setInt16(offset, clamp(right[sample], -1, 1) * 0x7fff, true);
      offset += 2;
    }
  }

  return Buffer.from(buffer);
}
