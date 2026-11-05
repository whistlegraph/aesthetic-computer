// The prompt's hidden keyboard bridge is not a text editor in Notepat.
export function notepatHasEditor(active, bridgeInput) {
  const editor=active?.closest?.('input, textarea, select, [contenteditable]:not([contenteditable="false"])');
  return !!editor && editor!==bridgeInput;
}
export function focusNotepatSurface(surface, bridgeInput, document=globalThis.document) {
  if(!surface || document.hidden || notepatHasEditor(document.activeElement,bridgeInput))return false;
  if(document.activeElement===bridgeInput)bridgeInput.blur();
  surface.tabIndex=-1;
  surface.focus({preventScroll:true});
  return document.activeElement===surface;
}
