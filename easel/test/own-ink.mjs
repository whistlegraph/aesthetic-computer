// Tests run in whatever window they were started from. This one might be a
// Slab-dressed Terminal, where the renderer follows the window's theme; the
// tests want the interface's own ink, so they say so before render.mjs loads.
process.env.EASEL_THEME = "own";
