// spreadnob-ui.js — jsui for the spreadnob dial
autowatch = 1;
mgraphics.init();
mgraphics.relative_coords = 0;
mgraphics.autofill = 0;

var KEYS = ["A","S","D","F","G","H","J","K","L"];
var N = 9;
var SWEEP = 270;
var START = 225; // degrees, lower-left

var val = 0;
var noteIdx = -1;
var name = "click a knob";
var pmin = 0;
var pmax = 1;

function note(n) {
  // Convert MIDI note to key index (0-8)
  var WHITE = [0,2,4,5,7,9,11,12,14];
  var off = n - 48;
  noteIdx = -1;
  for (var i = 0; i < WHITE.length; i++) {
    if (WHITE[i] === off) { noteIdx = i; break; }
  }
  mgraphics.redraw();
}

function value(v) {
  val = v;
  mgraphics.redraw();
}

function target() {
  var a = arrayfromargs(arguments);
  name = a.join(" ");
  mgraphics.redraw();
}

function setmin(v) { pmin = v; mgraphics.redraw(); }
function setmax(v) { pmax = v; mgraphics.redraw(); }

function d2r(d) { return d * Math.PI / 180; }

function paint() {
  with (mgraphics) {
    var w = mgraphics.size[0];
    var h = mgraphics.size[1];
    var cx = w / 2;
    var cy = h / 2 + 2;
    var R = Math.min(w, h) * 0.32;
    var tickR = R + 4;
    var labelR = R + 14;

    // Background
    set_source_rgba(0.12, 0.12, 0.14, 1);
    rectangle(0, 0, w, h);
    fill();

    // Track arc (background)
    set_source_rgba(0.25, 0.23, 0.28, 1);
    set_line_width(5);
    // mgraphics angles: clockwise from right. Negate math angles.
    arc(cx, cy, R, d2r(-START), d2r(-(START - SWEEP)));
    stroke();

    // Value arc
    var t = 0;
    if (pmax !== pmin) t = Math.max(0, Math.min(1, (val - pmin) / (pmax - pmin)));
    var valDeg = START - t * SWEEP;
    if (t > 0.001) {
      set_source_rgba(0.42, 1.0, 0.58, 0.9);
      set_line_width(5);
      arc(cx, cy, R, d2r(-START), d2r(-valDeg));
      stroke();
    }

    // Value dot
    var dx = cx + R * Math.cos(d2r(-valDeg));
    var dy = cy + R * Math.sin(d2r(-valDeg));
    set_source_rgba(1, 1, 1, 1);
    arc(dx, dy, 4, 0, Math.PI * 2);
    fill();

    // Ticks + key labels
    for (var i = 0; i < N; i++) {
      var deg = START - (i / (N - 1)) * SWEEP;
      var rad = d2r(-deg);
      var ox = cx + tickR * Math.cos(rad);
      var oy = cy + tickR * Math.sin(rad);
      var ix = cx + (R - 2) * Math.cos(rad);
      var iy = cy + (R - 2) * Math.sin(rad);
      var lx = cx + labelR * Math.cos(rad);
      var ly = cy + labelR * Math.sin(rad);

      var hit = (noteIdx === i);

      // Tick line
      set_line_width(hit ? 2.5 : 1);
      set_source_rgba(hit ? 1 : 0.5, hit ? 1 : 0.48, hit ? 1 : 0.55, 1);
      move_to(ox, oy);
      line_to(ix, iy);
      stroke();

      // Label
      set_source_rgba(hit ? 1 : 0.5, hit ? 1 : 0.48, hit ? 1 : 0.55, 1);
      select_font_face("Arial Bold");
      set_font_size(hit ? 11 : 9);
      var te = text_measure(KEYS[i]);
      move_to(lx - te[0] / 2, ly + te[1] / 3);
      show_text(KEYS[i]);
    }

    // Center — target name
    set_source_rgba(0.85, 0.7, 0.8, 1);
    select_font_face("Arial");
    set_font_size(9);
    var tn = name.length > 18 ? name.substring(0, 18) + "…" : name;
    var te = text_measure(tn);
    move_to(cx - te[0] / 2, cy - 4);
    show_text(tn);

    // Center — value
    var vt = val.toFixed(3);
    set_source_rgba(0.42, 1.0, 0.58, 1);
    select_font_face("Arial Bold");
    set_font_size(12);
    te = text_measure(vt);
    move_to(cx - te[0] / 2, cy + 12);
    show_text(vt);

    // Title
    set_source_rgba(1.0, 0.47, 0.72, 0.6);
    select_font_face("Arial Bold");
    set_font_size(8);
    move_to(4, 11);
    show_text("spreadnob");
  }
}
