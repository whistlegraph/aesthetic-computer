// Botce, 2023.10.26.21.00.12.436
// A paywall for botce, and...
// a paywalled ticket test implementation using `ticket` from the disk API.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO
  + Done
  - [🟠] Add botce.ac and aesthetic.computer domain to payment methods: https://dashboard.stripe.com/login?redirect=%2Fsettings%2Fpayment_method_domains
  - [-] Test Google Pay / Apple Pay
  - [x] Move protected code to a separate pieces server.
  - [x] Add "Powered by Stripe"
  - [x] Replace all curly quotes with single quote. 
  - [x] Question marks end of sentence spacing.
#endregion */

let needsWipe = true;

// 🥾 Boot
async function boot({ ticket, query, notice, store, jump }) {
  // Check for a local stub and use that to validate with the API.
  let storedTicket = await store.retrieve("ticket:botce");
  let noMessage = "botce";

  if (storedTicket) {
    // Expire if over a day.
    const hoursPassed =
      (new Date() - new Date(storedTicket.time)) / (1000 * 60 * 60);
    console.log("🎟️ Ticket found! Hours passed:", hoursPassed, storedTicket);
    if (hoursPassed > 24) {
      await store.delete("ticket:botce");
      storedTicket = null;
      console.warn("🎟️ Ticket stub expired.");
      noMessage = "old ticket :/";
    }
  }

  const ticketToCheck = query?.ticket || storedTicket?.key;
  if (!ticketToCheck)
    notice(noMessage, [
      "pink",
      "black",
      // [48, 49, 61],
      // [250, 146, 146],
    ]);

  // Check for a ticket stub using the API.
  if (ticketToCheck) {
    let slug = `/api/ticket/${ticketToCheck}`;
    if (storedTicket) slug += "?found=true";

    fetch(slug)
      .then((res) => {
        if (!res.ok) {
          if (res.status === 401) console.warn("No ticket found. 😢");
          if (res.status === 403) console.warn("Ticket expired. 🎟️");
          throw new Error(`Access denied: ${res.status}`);
        }
        return res.json();
      })
      .then((data) => {
        console.log("✅ 🎟️ Ticket accepted:", data);
        notice(`ticket ${3 - data.ticket.uses}`);
        store["ticket:botce"] = { key: ticketToCheck, time: new Date() };
        store.persist("ticket:botce"); // Store stub with current time.
        setTimeout(() => jump(data.botce.piece, true, true), 500); // Actually
      })
      .catch((err) => {
        console.log("Error:", err);
        notice("nothing left.", ["yellow", "red"]);
        setTimeout(() => ticket({ from: "sotce", item: "botce" }), 1500);
      });
  } else {
    // setTimeout(() => {
    // notice("has left us", ["pink", "black"]);
    // }, 1000);
    ticket({ from: "sotce", item: "botce" });
  }
}

// 🎨 Paint
function paint({ wipe, ink, help: { choose }, screen }) {
  if (needsWipe) {
    wipe(20, 30, 100);
    needsWipe = false;
  }
  ink(choose("pink", "gray", "blue"))
    .write(choose("hey sotce :)", "sup botce :D"))
    .ink(10, 30, 80, 6)
    .box(0, 0, screen.width, screen.height);
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
    title: "Botce",
    desc: "Ask questions and botce will answer you.",
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

export const nohud = true;

// 📚 Library
//   (Useful functions used throughout the piece)
