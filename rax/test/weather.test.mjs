// 🌧️ Open-Meteo's answer as what the room draws.
import test from 'node:test';
import assert from 'node:assert/strict';
import { read, CLEAR } from '../weather.mjs';

const answer = (precipitation, weather_code, snowfall = precipitation.map(() => 0)) => ({
  current: { cloud_cover: 100, wind_speed_10m: 18, wind_gusts_10m: 36, wind_direction_10m: 200 },
  minutely_15: { time: precipitation.map((_, i) => `t${i}`), precipitation, snowfall, weather_code },
});

test('a dry sky has no rain, and its cloud and wind are told', () => {
  const w = read(answer([0, 0, 0], [3, 3, 3]), 5);
  assert.deepEqual(w, { rain: 0, snow: 0, cloud: 1, wind: 5, gust: 10, from: 200, at: 5 });
});

test('a shower in the last half hour still falls, harder the more came down', () => {
  const light = read(answer([0.1, 0, 0], [51, 3, 3])).rain, heavy = read(answer([4.2, 2.9, 0], [65, 65, 3])).rain;
  assert.ok(light > 0 && light < 0.4 && heavy === 1, `${light}, ${heavy}`);
});

test('snow falls as snow, not rain, and nothing heard is a clear sky', () => {
  const w = read(answer([1, 1, 1], [73, 73, 73], [0.6, 0.4, 0]));
  assert.ok(w.snow > 0 && w.rain === 0);
  assert.deepEqual({ ...read(null, 0) }, { ...CLEAR });
});
