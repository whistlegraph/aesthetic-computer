import { promises as fs } from "node:fs";
import path from "node:path";

// Persist across the short-lived wallpaper CLI processes and subject changes.
export function wallpaperBackoff(directory, now = Date.now) {
  const file = (provider) => path.join(directory, `.retry-${provider}.json`);
  return {
    async active(provider) {
      try {
        const { until } = JSON.parse(await fs.readFile(file(provider), "utf8"));
        return Number.isFinite(until) && until > now();
      } catch {
        return false;
      }
    },
    async defer(provider, retryAfter) {
      const seconds = Number(retryAfter);
      const delay =
        retryAfter && Number.isFinite(seconds)
          ? seconds * 1000
          : Date.parse(retryAfter) - now();
      const until =
        now() + (Number.isFinite(delay) && delay > 0 ? delay : 60000);
      await fs.mkdir(directory, { recursive: true });
      const temp = `${file(provider)}.${process.pid}.tmp`;
      await fs.writeFile(temp, JSON.stringify({ until }));
      await fs.rename(temp, file(provider));
    },
  };
}
