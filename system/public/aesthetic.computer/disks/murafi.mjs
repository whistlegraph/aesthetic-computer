// murafi, 26.09.11
// Random lines, drawn forever.

function boot({ wipe }) {
  wipe(70, 50, 100);
}

function paint({ ink, screen, num }) {
  const { randInt } = num;

  for (let i = 0; i < 4; i += 1) {
    ink(randInt(255), randInt(255), randInt(255), 128).line(
      randInt(screen.width),
      randInt(screen.height),
      randInt(screen.width),
      randInt(screen.height),
    );
  }
}

export { boot, paint };
