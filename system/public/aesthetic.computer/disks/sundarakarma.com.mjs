// SundaraKarma.com, 2025.07.01
// A simple test piece for the sundarakarma.com domain redirect.

function boot({ help }) {
  help("Welcome to SundaraKarma.com");
}

function paint({ wipe, ink, write, screen }) {
  wipe("white");
  ink("blue");
  write("Welcome to SundaraKarma.com!", { center: "xy" });
  ink("gray");
  write("This is served from aesthetic.computer", { 
    center: "x", 
    y: screen.height / 2 + 30 
  });
}

function meta() {
  return {
    title: "SundaraKarma.com",
    desc: "Welcome to SundaraKarma.com - powered by aesthetic.computer"
  };
}

export { boot, paint, meta };
