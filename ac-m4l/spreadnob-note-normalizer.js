// spreadnob-note-normalizer.js
// Normalizes octave-shifted QWERTY notes back into the spreadnob rack range.

autowatch = 1;
inlets = 3;
outlets = 2;

var low = 48;
var high = 62;
var lockedShift = 0;
var shiftKnown = false;
var lastRaw = null;
var lastNormalized = null;

function clamp(n, lo, hi) {
    return Math.max(lo, Math.min(hi, n));
}

function rounded(n) {
    return Math.round(Number(n) || 0);
}

function duplicateWidth() {
    return Math.max(0, (high - low) - 11);
}

function isAmbiguousNormalized(note) {
    var off = rounded(note) - low;
    var dup = duplicateWidth();
    if (dup <= 0) return false;
    return off < dup || off >= 12;
}

function getCandidates(raw) {
    var candidates = [];
    for (var shift = -8; shift <= 8; shift++) {
        var normalized = raw - shift * 12;
        if (normalized >= low && normalized <= high) {
            candidates.push({
                shift: shift,
                normalized: normalized,
                ambiguous: isAmbiguousNormalized(normalized),
            });
        }
    }
    return candidates;
}

function chooseClosestShift(candidates, preferredShift) {
    var best = candidates[0];
    var bestShiftDistance = Math.abs(best.shift - preferredShift);
    var bestNormalizedDistance = lastNormalized === null
        ? Math.abs(best.normalized - low)
        : Math.abs(best.normalized - lastNormalized);

    for (var i = 1; i < candidates.length; i++) {
        var candidate = candidates[i];
        var shiftDistance = Math.abs(candidate.shift - preferredShift);
        var normalizedDistance = lastNormalized === null
            ? Math.abs(candidate.normalized - low)
            : Math.abs(candidate.normalized - lastNormalized);

        if (shiftDistance < bestShiftDistance) {
            best = candidate;
            bestShiftDistance = shiftDistance;
            bestNormalizedDistance = normalizedDistance;
            continue;
        }

        if (shiftDistance === bestShiftDistance && normalizedDistance < bestNormalizedDistance) {
            best = candidate;
            bestShiftDistance = shiftDistance;
            bestNormalizedDistance = normalizedDistance;
        }
    }

    return best;
}

function resolve(raw) {
    var candidates = getCandidates(raw);
    if (!candidates.length) {
        var fallbackShift = shiftKnown ? lockedShift : rounded((raw - low) / 12);
        return {
            raw: raw,
            normalized: clamp(raw - fallbackShift * 12, low, high),
            shift: fallbackShift,
            locked: shiftKnown,
            ambiguous: 1,
        };
    }

    var selected = null;

    if (shiftKnown) {
        for (var i = 0; i < candidates.length; i++) {
            if (candidates[i].shift === lockedShift) {
                selected = candidates[i];
                break;
            }
        }
    }

    if (!selected) {
        var unambiguous = [];
        for (var j = 0; j < candidates.length; j++) {
            if (!candidates[j].ambiguous) unambiguous.push(candidates[j]);
        }

        if (unambiguous.length === 1) {
            selected = unambiguous[0];
            lockedShift = selected.shift;
            shiftKnown = true;
        } else if (candidates.length === 1) {
            selected = candidates[0];
        } else {
            selected = chooseClosestShift(candidates, shiftKnown ? lockedShift : 0);
        }
    }

    if (!selected.ambiguous) {
        lockedShift = selected.shift;
        shiftKnown = true;
    }

    return {
        raw: raw,
        normalized: selected.normalized,
        shift: selected.shift,
        locked: shiftKnown ? 1 : 0,
        ambiguous: candidates.length > 1 && selected.ambiguous ? 1 : 0,
    };
}

function emitState(state) {
    outlet(0, state.normalized);
    outlet(1, state.raw, state.normalized, state.shift, state.locked, state.ambiguous);
}

function note(value) {
    var raw = rounded(value);
    var state = resolve(raw);
    lastRaw = raw;
    lastNormalized = state.normalized;
    emitState(state);
}

function setlow(value) {
    low = rounded(value);
    if (high < low) high = low;
}

function sethigh(value) {
    high = rounded(value);
    if (high < low) low = high;
}

function clearlock() {
    lockedShift = 0;
    shiftKnown = false;
}

function loadbang() {
    clearlock();
}

function msg_int(value) {
    if (inlet === 0) note(value);
    if (inlet === 1) setlow(value);
    if (inlet === 2) sethigh(value);
}

function list() {
    var args = arrayfromargs(arguments);
    if (!args.length) return;
    msg_int(args[0]);
}

function anything() {
    var args = arrayfromargs(arguments);
    if (messagename === "note" && args.length) note(args[0]);
    if (messagename === "low" && args.length) setlow(args[0]);
    if (messagename === "high" && args.length) sethigh(args[0]);
    if (messagename === "reset" || messagename === "clearlock") clearlock();
}
