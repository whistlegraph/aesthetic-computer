// os, 2025.02.24
// Download a bootable FedAC OS image for any piece.
// Usage: os:notepat or os:$code
//
// Redirects to prompt where the os command runs inline with SSE
// progress UI. Direct URL visitors get bounced to prompt~os piece~!autorun.

function boot({ params, jump }) {
  const target = params[0];
  if (!target) {
    jump("prompt");
    return;
  }
  jump("prompt~os " + target + "~!autorun");
}

export const desc = "Download a bootable OS image for any piece.";
export { boot };
