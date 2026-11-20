// 🌧️ The weather
// rax has Los Angeles's sky (sky.mjs) and its weather too: what falls over
// 90026 falls here. The server asks Open-Meteo every ten minutes (no key, and
// nothing about anyone is sent: only the place) and the page asks the server.
export const PLACE = { zip: '90026', latitude: 34.078, longitude: -118.265 };
export const WEATHER_URL = `https://api.open-meteo.com/v1/forecast?latitude=${PLACE.latitude}&longitude=${PLACE.longitude}` +
  '&current=cloud_cover,wind_speed_10m,wind_direction_10m,wind_gusts_10m,temperature_2m&minutely_15=precipitation,snowfall,weather_code&past_minutely_15=2&forecast_minutely_15=1&daily=sunrise,sunset&forecast_days=1&timezone=America%2FLos_Angeles';
export const CLEAR = { rain: 0, snow: 0, cloud: 0, wind: 0, gust: 0, from: 270, rise: 0.25, set: 0.75, at: 0 };
// "2026-11-19T06:29" (the place's own clock) as a share of its day.
const share = (stamp, or) => { const m = /T(\d\d):(\d\d)/.exec(stamp || ''); return m ? +((Number(m[1]) * 60 + Number(m[2])) / 1440).toFixed(4) : or; };

// Open-Meteo's answer as what the room draws: rain and snow from 0 (none) to
// 1 (pouring), the cloud from 0 to 1, the wind and its gusts in metres a
// second and the bearing it comes from (air.mjs makes the air of them). The last
// half hour counts, so a shower does not stop and start with each reading.
export function read(json, now = Date.now()) {
  const q = json?.minutely_15, n = (v) => (Number.isFinite(v) ? v : 0);
  let wet = 0, white = 0, drizzle = false;
  for (let i = 0; i < (q?.time?.length || 0); i++) {
    white = Math.max(white, n(q.snowfall?.[i]));
    wet = Math.max(wet, n(q.precipitation?.[i]));
    const code = q.weather_code?.[i];
    if ((code >= 51 && code <= 67) || (code >= 80 && code <= 82) || code >= 95) drizzle = true;
  }
  const snow = white > 0 ? Math.min(1, 0.25 + white / 1.5) : 0;
  // (mm in a quarter hour: 0.1 is a drizzle, 2.5 and over is pouring)
  const rain = snow ? 0 : wet > 0 || drizzle ? Math.min(1, 0.2 + wet / 2.5) : 0;
  return { rain: +rain.toFixed(2), snow: +snow.toFixed(2), cloud: +(n(json?.current?.cloud_cover) / 100).toFixed(2),
    wind: +(n(json?.current?.wind_speed_10m) / 3.6).toFixed(1), gust: +(Math.max(n(json?.current?.wind_gusts_10m), n(json?.current?.wind_speed_10m)) / 3.6).toFixed(1),
    from: Number.isFinite(json?.current?.wind_direction_10m) ? Math.round(json.current.wind_direction_10m) : 270,
    // 🌅 When the sun really comes up and goes down there today (sky.mjs keeps the day by them).
    rise: share(json?.daily?.sunrise?.[0], 0.25), set: share(json?.daily?.sunset?.[0], 0.75), at: now };
}
