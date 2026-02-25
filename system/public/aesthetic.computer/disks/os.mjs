// os, 2025.02.24
// Download a bootable FedAC OS image for any piece.
// Usage: os:notepat or os:$code
//
// Generates a ~3GB bootable disk image that runs the piece fullscreen
// in Firefox kiosk mode on bare metal (flash to USB with Balena Etcher).

function boot({ api, params, jump }) {
  const { wipe, ink, write } = api;
  let target = params[0];

  if (!target) {
    wipe("red");
    ink("white");
    write("Usage: os:piece-name", { center: "xy" });
    return { needsPaint: false };
  }

  const isKidlisp = target.startsWith("$");
  if (isKidlisp) target = target.slice(1);

  const host = api.net.host || "";
  const dev = host.startsWith("localhost") || host.includes("local.");
  const apiUrl = dev ? `https://${host}` : "https://aesthetic.computer";
  const param = isKidlisp
    ? `code=${encodeURIComponent(target)}`
    : `piece=${encodeURIComponent(target)}`;
  const url = `${apiUrl}/api/os?${param}`;

  jump("out:" + url);

  wipe(32);
  ink("cyan");
  write("Building OS image...", { center: "xy", size: 2 });

  return { needsPaint: false };
}

export const desc = "Download a bootable OS image for any piece.";
export { boot };
