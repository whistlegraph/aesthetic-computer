// Mime Reel, 2026.11.02
// The slot the mime reel boots into: mime's dark media colour while the feed
// arrives, after which the reel (lib/mime-reel.mjs) jumps to the first card.
// It only runs as a reel inside a noauth MIME runtime:
//   /mime-reel?noauth=true&mime=1&reel=1&nogap=true&nolabel=true

function paint({ wipe, ink, write, screen }) {
  wipe(23, 21, 38);
  ink(245, 238, 250, 140).write("mime", { center: "xy" });
  return false;
}

export { paint };
