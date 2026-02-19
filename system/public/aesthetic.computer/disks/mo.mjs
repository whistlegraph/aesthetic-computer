// mo, 26.01.10
// 🎄 Mo - Shorthand for merryo with uniform timing
// URL examples:
//   /mo.1:a:b:c -> merryo 0.1-a 0.1-b 0.1-c (100ms each, loops)
//   /mo.05:tone:clock -> merryo 0.05-tone 0.05-clock (50ms each)
//   /mo:a:b:c -> merryo a b c (default 5s each)
// Prompt examples:
//   mo.1 a b c -> same as merryo 0.1-a 0.1-b 0.1-c (handled in prompt.mjs)

export function boot({ colon, params, jump }) {
  // Parse timing from first param if it starts with "." (fractional seconds)
  // or is an integer (whole seconds).
  // URL routing converts /mo.1:a:b:c to /mo~.1:a:b:c
  // parse.mjs also converts /mo1~a~b~c to /mo~1~a~b~c
  let uniformDuration = null;
  let pieces = [];
  let fadeOption = ""; // e.g., ":fade" or ":fade.5"

  // Extract :fade option from colon params before processing pieces
  const filteredColon = [];
  if (colon && colon.length > 0) {
    for (const part of colon) {
      const fadeMatch = part.match(/^fade(\.\d+)?$/);
      if (fadeMatch) {
        fadeOption = ":" + part; // Preserve as colon param for prompt
      } else {
        filteredColon.push(part);
      }
    }
  }

  const firstParam = params && params.length > 0 ? params[0] : null;
  const hasTimingParam =
    typeof firstParam === "string" &&
    (firstParam.startsWith(".") || /^\d+$/.test(firstParam));

  if (hasTimingParam) {
    if (firstParam.startsWith(".")) {
      const parsed = parseFloat("0" + firstParam); // ".1" -> 0.1
      if (!isNaN(parsed) && parsed > 0) {
        uniformDuration = parsed;
      }
    } else {
      const parsed = parseInt(firstParam, 10); // "1" -> 1
      if (Number.isFinite(parsed) && parsed > 0) {
        uniformDuration = parsed;
      }
    }

    pieces.push(...filteredColon);
    if (params.length > 1) {
      pieces.push(...params.slice(1));
    }
  } else {
    // No timing specified, use colon and params directly as pieces
    pieces.push(...filteredColon);
    if (params && params.length > 0) {
      pieces.push(...params);
    }
  }

  if (pieces.length === 0) {
    jump("prompt~merryo");
    return;
  }

  // Build args with uniform duration if specified
  let args;
  if (uniformDuration !== null) {
    args = pieces.map(piece => `${uniformDuration}-${piece}`);
  } else {
    args = pieces;
  }

  // Jump to prompt with merryo command, forwarding :fade if present
  jump("prompt~merryo" + fadeOption + " " + args.join(" "));
}

export const nohud = true;

export function meta() {
  return {
    title: "Mo",
    desc: "🎄 Quick looping merry with uniform timing",
  };
}
