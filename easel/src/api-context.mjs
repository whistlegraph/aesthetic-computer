// Small corrections to the generated map, checked against the runtime source.
// Keep these separate so rebuilding api.json cannot silently erase them.
export const API_WORKFLOW = `AC JavaScript workflow: lifecycle functions receive the AC API object; it is not CanvasRenderingContext2D. Before using an unfamiliar symbol, call ac_api with its exact name or a task such as "text fonts". Use the returned signatures and examples; do not invent browser-style overloads. If still unclear, use ac_examples for that symbol, then ac_outline/ac_symbol on one relevant file instead of repeatedly scanning the repository. After editing, call ac_preview and inspect the current revision before claiming it works. Missing feedback is not a successful test.`;

export const API_CORRECTIONS = [
 {
  name:'write',path:'write',aliases:['text','label','typography','font','fonts','lettering'],
  signature:'write(text, x, y, {typeface, bg, bounds, wordWrap, rotation}) | write(text, {x, y, size, center, rotation}, bg, bounds, wordWrap, customTypeface)',
  doc:'Draw text using the current ink; returns the paint API for chaining. Numeric x/y form takes font in options.typeface. Position-object form takes customTypeface as the SIXTH argument, not pos.typeface. size is a scale multiplier in the position object. center accepts "x", "y", or "xy"; set x/y explicitly when not centering, because omitted positions can be randomized.',
  source:'lib/disk.mjs: write; lib/type.mjs: Typeface.print',
  examples:['export function paint({wipe,ink}) { wipe("black"); ink("white").write("Hello", 8, 8, {typeface:"MatrixChunky8"}); }','ink("white").write("Hello", {center:"xy",size:2}, undefined, undefined, false, "microtype");'],
  related:['typeface','text.width','fonts'],
 },
 {
  name:'typeface',path:'typeface',aliases:['text','font','fonts','typography','metrics'],
  signature:'typeface.name; typeface.blockWidth; typeface.blockHeight',
  doc:'The lifecycle API exposes the current preloaded Typeface INSTANCE, not a typeface(name) function. Read its metrics; do not call it or assume assigning it selects a global font. Choose a font per write call. Proportional fonts require text.width(text,fontName), not text.length * blockWidth.',
  source:'lib/disk.mjs: $commonApi.typeface = tf; lib/type.mjs: class Typeface',
  examples:['export function paint({ink,typeface}) { ink("white").write(typeface.name, 8, 8); }'],related:['write','text.width','fonts'],
 },
 {
  name:'width',path:'text.width',aliases:['text','font','fonts','measure','metrics','typography'],
  signature:'text.width(stringOrWordArray, fontName?)',
  doc:'Returns unscaled pixel width using the selected font advances (or current typeface). Multiply by your write position.size scale for layout. Word arrays join with spaces. Do not assume every character has equal width in MatrixChunky8.',
  source:'lib/disk.mjs: $commonApi.text.width',
  examples:['const x = Math.floor((screen.width - text.width("Hello", "MatrixChunky8")) / 2);'],related:['write','typeface'],
 },
 {
  name:'fonts',path:'fonts',aliases:['text','font','typeface','typography','lettering'],
  signature:'Font names for write(): "font_1", "microtype", "unifont", "MatrixChunky8"',
  doc:'This is a font-name reference, NOT a callable API or a fonts property on the lifecycle object. font_1 is 6×10; microtype is 4×5; unifont is 8×16; MatrixChunky8 is proportional and 8 pixels high. Fonts are runtime assets; custom fonts may load asynchronously. CSS font-family strings are not this API.',
  source:'disks/common/fonts.mjs; lib/disk.mjs: resolveTypefaceInstance',related:['write','typeface','text.width'],
 },
];
export function apiEntries(map) {
 const overrides=new Map(API_CORRECTIONS.map(entry=>[entry.path,entry]));
 return [...(map.entries||[]).filter(entry=>!overrides.has(entry.path)),...API_CORRECTIONS];
}
