// geo.mjs — IP-based geolocation piece
// Shows current city, region, country, coordinates, timezone, and ISP
// Uses ip-api.com (free, no key needed, 45 req/min)

let geoData = null;
let fetchStarted = false;
let error = null;
let animFrame = 0;

function boot({ system, wifi }) {
  // Start fetch immediately if we have internet
  if (wifi?.connected) {
    system.fetch("http://ip-api.com/json/?fields=status,message,country,regionName,city,zip,lat,lon,timezone,isp,query");
    fetchStarted = true;
  }
}

function paint({ wipe, ink, write, screen, system, wifi, frame }) {
  animFrame = frame;
  const W = screen.width;
  const H = screen.height;
  const font = "6x10";
  const lh = 12;

  wipe(15, 12, 25);

  // Title
  ink(180, 220, 255);
  write("geo", { x: 4, y: 4, size: 2, font });

  // Try to fetch if we haven't yet and wifi just connected
  if (!fetchStarted && wifi?.connected) {
    system.fetch("http://ip-api.com/json/?fields=status,message,country,regionName,city,zip,lat,lon,timezone,isp,query");
    fetchStarted = true;
  }

  // Check for fetch result
  if (system.fetchResult && !geoData) {
    try {
      const data = JSON.parse(system.fetchResult);
      if (data.status === "success") {
        geoData = data;
      } else {
        error = data.message || "lookup failed";
      }
    } catch (e) {
      error = "parse error";
    }
  }

  let y = 30;

  if (geoData) {
    const lines = [
      { label: "city", value: `${geoData.city}, ${geoData.regionName}` },
      { label: "country", value: geoData.country },
      { label: "zip", value: geoData.zip },
      { label: "coords", value: `${geoData.lat.toFixed(4)}, ${geoData.lon.toFixed(4)}` },
      { label: "timezone", value: geoData.timezone },
      { label: "isp", value: geoData.isp },
      { label: "ip", value: geoData.query },
    ];

    for (const { label, value } of lines) {
      ink(100, 140, 180);
      write(label, { x: 4, y, size: 1, font });
      ink(240, 240, 220);
      write(value, { x: 4 + (label.length + 1) * 6, y, size: 1, font });
      y += lh;
    }

    // Visual: pulsing dot representing location
    y += 10;
    const pulse = Math.sin(frame * 0.06) * 0.3 + 0.7;
    const dotR = Math.floor(8 * pulse);
    const cx = W / 2;
    const cy = y + 30;
    ink(80, 180, 255, Math.floor(60 * pulse));
    // Draw concentric rings
    for (let r = dotR + 20; r > dotR; r -= 5) {
      for (let a = 0; a < 360; a += 3) {
        const rad = a * Math.PI / 180;
        const px = Math.floor(cx + r * Math.cos(rad));
        const py = Math.floor(cy + r * Math.sin(rad));
        write(".", { x: px, y: py, size: 1, font });
      }
    }
    ink(120, 220, 255);
    write("*", { x: Math.floor(cx) - 3, y: Math.floor(cy) - 5, size: 1, font });
  } else if (error) {
    ink(255, 100, 100);
    write(error, { x: 4, y, size: 1, font });
  } else if (!wifi?.connected) {
    ink(180, 140, 100);
    const dots = ".".repeat((Math.floor(frame / 20) % 3) + 1);
    write("waiting for wifi" + dots, { x: 4, y, size: 1, font });
  } else {
    ink(140, 180, 220);
    const dots = ".".repeat((Math.floor(frame / 20) % 3) + 1);
    write("locating" + dots, { x: 4, y, size: 1, font });
  }

  // Footer: ESC to go back
  ink(80, 80, 100);
  write("ESC: back", { x: 4, y: H - 14, size: 1, font });
}

function act({ event: e, system }) {
  if (e.is("keyboard:down:escape")) {
    system.jump("prompt");
  }
}

export { boot, paint, act };
