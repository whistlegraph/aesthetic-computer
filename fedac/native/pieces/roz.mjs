// roz.mjs — Redirect to lisp.mjs with $roz source
// For backward compatibility with system.jump("roz")

const ROZ_SOURCE = `fade:red-blue-black-blue-red
ink (? rainbow white 0) (1s... 24 64)
line w/2 0 w/2 h
(spin (2s... -1.125 1.125)) (zoom 1.1)
(0.5s (contrast 1.05))
(scroll (? -0.1 0 0.1) (? -0.1 0 0.1))
ink (? cyan yellow magenta) 8
circle w/2 h/2 (? 2 4 8)`;

function boot({ system }) {
  globalThis.__kidlispSource = ROZ_SOURCE;
  globalThis.__kidlispLabel = "$roz";
  system?.jump?.("lisp");
}

function act() {}
function sim() {}
function paint() {}

export { boot, paint, act, sim };
