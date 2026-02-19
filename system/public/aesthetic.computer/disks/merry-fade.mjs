// merry-fade — Host piece for crossfading between KidLisp $code pieces
// Stays loaded while the merry pipeline runs. Renders $code pieces via
// paintApi.kidlisp() and alpha-blends them during transitions.

export const nohud = true;

function boot({ system, needsPaint }) {
  if (!system.merry) return;
  needsPaint();
}

function paint({ wipe, screen, kidlisp, paste, pasteWithAlpha, system, needsPaint }) {
  const merry = system.merry;
  if (!merry || !merry.running) {
    wipe(0);
    return;
  }

  const w = screen.width;
  const h = screen.height;
  const pipeline = merry.pipeline;
  const fadeDuration = merry.fadeDuration || 0.3;

  // Determine what to render based on UTC-synced timing.
  const now = merry.getUTCTime();
  const totalMs = merry.totalDuration * 1000;
  const cyclePos = now % totalMs;

  // Find current piece index and time within it.
  let accumulated = 0;
  let currentIndex = 0;
  let pieceElapsed = 0;
  for (let i = 0; i < pipeline.length; i++) {
    const durMs = pipeline[i].duration * 1000;
    if (cyclePos < accumulated + durMs) {
      currentIndex = i;
      pieceElapsed = cyclePos - accumulated;
      break;
    }
    accumulated += durMs;
  }

  const pieceDurMs = pipeline[currentIndex].duration * 1000;
  const fadeMs = fadeDuration * 1000;
  const timeLeft = pieceDurMs - pieceElapsed;

  // Update merry state so the progress bar stays accurate.
  merry.currentIndex = currentIndex;
  merry.pieceProgress = pieceElapsed / pieceDurMs;
  merry.progress = cyclePos / totalMs;

  // Piece names may already include $ (e.g., "$xom") or not (e.g., "xom").
  const pieceName = pipeline[currentIndex].piece;
  const currentCode = pieceName.startsWith("$") ? pieceName : "$" + pieceName;

  // Are we in a crossfade window? (last fadeMs of the current piece)
  const nextIndex = (currentIndex + 1) % pipeline.length;
  const isFading = timeLeft <= fadeMs && pipeline.length > 1;

  if (isFading) {
    const nextName = pipeline[nextIndex].piece;
    const nextCode = nextName.startsWith("$") ? nextName : "$" + nextName;
    const fadeProgress = 1 - (timeLeft / fadeMs); // 0→1

    // Render incoming piece (will be underneath).
    const incoming = kidlisp(0, 0, w, h, nextCode, { noPaste: true });

    // Render outgoing piece to a buffer.
    const outgoing = kidlisp(0, 0, w, h, currentCode, { noPaste: true });

    // Composite: incoming first (full opacity), outgoing on top fading out.
    wipe(0);
    if (incoming) paste(incoming, 0, 0);
    if (outgoing) {
      const outAlpha = Math.round(255 * (1 - fadeProgress));
      if (outAlpha > 0) pasteWithAlpha(outgoing, 0, 0, outAlpha);
    }
  } else {
    // No crossfade — render current piece directly.
    wipe(0);
    kidlisp(0, 0, w, h, currentCode);
  }

  needsPaint(); // Keep rendering every frame.
}

export { boot, paint };
