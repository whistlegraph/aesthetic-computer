// Botce, 2023.10.26.21.00.12.436
// A paywall for botce, and...
// a paywalled ticket test implementation using `ticket` from the disk API.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 

  - [] Three remaining.
  - [] Lowercase tickets. 
  - [] Add gutter bar 2 the right.
  - [] Test a centered layout that's low resolution.
  - [] More distant line spacing. 

    Under pay button:
  - [] Do some security checks / put the `botce` prompt text behind a private
       store... or in the database?
  ------------
  + Done
  - [x] Fix mobile offset not being centered.
  - [x] Add to ticket prompt:
    - [x] Get botce.ac working again.
    - [x] Add image of happy Amelia.
    - [x] ask and botce will answer you
  - [x] When a user clicks the link... 
  `/?ticket=botce_{key}`
  - [x] It should check for the localStorage token and
       validate on the server.
      - [x] If the local token exists and is less than 24 hours old, then do not
           decrement the `uses`.
      - [x] Otherwise, decrement and then store the key in localStorage.
  - [x] If validated, it should return the botce prompt
       source code content / piece name from the server to unlock it.
    - [x] Use the ticket api endpoint to validate,
         and returns the prompt.
  - [x] And load the piece.
  - [x] Format email.
  - [x] Make sure to clear the ticket screen whenever piece changes.
  - [x] Set up expiring links.
    - [x] Add link to the db via `tickets` collection.
    - [x] Get links to function via a getter endpoint of
         `api/ticket/{key}`
  - [x] After each successful sale, produce a special hash link
        tied to a database record that has only a certain number of clicks.
#endregion */

let needsWipe = true;

// 🥾 Boot
async function boot({ ticket, query, notice, store, jump }) {
  // Check for a local stub and use that to validate with the API.
  let storedTicket = await store.retrieve("ticket:botce");
  let noMessage = "botce";

  if (storedTicket) {
    // Expire if over a day.
    const hoursPassed = (new Date() - storedTicket.time) / (1000 * 60 * 60);
    console.log("🎟️ Ticket found! Hours passed:", hoursPassed, storedTicket);
    if (hoursPassed > 24) {
      await store.delete("ticket:botce");
      storedTicket = null;
      console.warn("🎟️ Ticket stub expired.");
      noMessage = "old ticket :/";
    }
  }

  const ticketToCheck = query?.ticket || storedTicket?.key;
  if (!ticketToCheck) notice(noMessage);

  // Check for a ticket stub using the API.
  if (ticketToCheck) {
    // notice("checking ticket :(");
    let slug = `https://aesthetic.computer/api/ticket/${ticketToCheck}`;
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
        //                                                            go to botce.
      })
      .catch((err) => {
        console.log("Error:", err);
        notice("nothing left.", ["yellow", "red"]);
        setTimeout(() => ticket({ from: "sotce", item: "botce" }), 1500);
      });
  } else {
    ticket({ from: "sotce", item: "botce" });
  }
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
