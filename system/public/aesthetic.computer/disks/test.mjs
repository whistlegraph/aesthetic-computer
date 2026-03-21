// ap (Autopilot), 22.10.13.01.27
// A test that imports any brush from the nopaint system,
// and runs it on autopilot through $api interception / automation.

// TODO: Pick a random brush from a whitelist if none are chosen?
// TODO: Sim needs to generate pen gestures over time.
//       (Start with linear ones)
// TODO: Pick a seed for randomness and use it in num.

let brush;
let painting;
let paintCountOffset;

async function boot($api) {
  const name = $api.params[0] || "smear";

  brush = await import(`${$api.net.pieces}/${name}.mjs`);
  painting = $api.painting(256, 256, (p) => p.noise16DIGITPAIN());
  $api.system = { painting };
  $api.params.shift(); // Remove the first element from params.
  brush?.boot?.($api);
}

function paint($api) {
  $api.system = { painting };
  $api.params.shift(); // Remove the first element from params.

  $api.pen = {
    drawing: true,
    x: $api.num.randInt(256),
    y: $api.num.randInt(256),
    px: $api.num.randInt(256),
    py: $api.num.randInt(256),
    dragBox: [0, 0, 10, 10],
  };

  brush?.paint?.($api);
}

function act($api) {
  // TODO: Automate a lift event for rect?
  brush?.act?.($api);
}

export { boot, paint, act };
