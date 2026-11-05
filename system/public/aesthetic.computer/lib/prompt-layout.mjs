// Prompt input and the corner HUD share one text origin, so jumping does not
// move the word. Keep the HUD's internal room for shadows and status marks.
export const PROMPT_TEXT_ORIGIN = Object.freeze({x:6,y:6});
export const HUD_TEXT_INSET_Y = 4;
export const HUD_DEFAULT_OFFSET = Object.freeze({x:PROMPT_TEXT_ORIGIN.x,y:PROMPT_TEXT_ORIGIN.y-HUD_TEXT_INSET_Y});
