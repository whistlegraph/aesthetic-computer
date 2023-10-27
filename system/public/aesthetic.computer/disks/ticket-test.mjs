// Ticket-Test, 2023.10.26.21.00.12.436
// A pay-walled ticket test implementation using `ticket` from the disk API.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [-] Make sure to clear the ticket screen whenever piece changes.
  - [] Set up expiring links on the email / in the database.
    - [] Send an email.
    - [] Would I need to respond to Stripe webhooks here?
#endregion */

let needsWipe = true;

// 🥾 Boot
function boot({ ticket, query }) {
  if (query?.notice !== "PAID!") ticket({ from: "sotce", item: "botce" });
}

// 🎨 Paint
function paint({ wipe, ink, help: { choose } }) {
  if (needsWipe) {
    wipe("orange");
    needsWipe = false;
  }
  ink().write(choose("sotce", "botce"));
}

// 🎪 Act
function act({ event: e }) {
  if (e.is("reframed")) needsWipe = true;
}

// 🧮 Sim
// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

// 📰 Meta
function meta() {
  return {
    title: "Ticket-test",
    desc: "A pay-walled ticket test implementation using `ticket` from the disk API.",
  };
}

// 🖼️ Preview
// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// 🪷 Icon
// function icon() {
// Render an application icon, aka favicon.
// }

export { boot, paint, act, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
