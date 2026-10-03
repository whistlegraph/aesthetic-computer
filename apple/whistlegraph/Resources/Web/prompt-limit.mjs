export const PROMPT_LIMIT = 96;
const characters = text => [...new Intl.Segmenter(undefined, {granularity:'grapheme'}).segment(text)];
export function checkedPrompt(text) {
  if (typeof text !== 'string' || !text.trim()) throw Error('A request is required');
  const prompt = text.replace(/[\r\n\u2028\u2029]+/g, ' ').trim();
  if (characters(prompt).length > PROMPT_LIMIT) throw Error(`Requests must be ${PROMPT_LIMIT} characters or fewer`);
  return prompt;
}
