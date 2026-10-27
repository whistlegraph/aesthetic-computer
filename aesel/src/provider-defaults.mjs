import {HOSTED_MODELS} from './open-models.mjs';
export const DEFAULT_CLAUDE_MODEL='claude-opus-5';
export const AC_MODELS={...HOSTED_MODELS,luna:'openai/gpt-5.6-luna',gpt:'openai/gpt-5.4'};
export const DEFAULT_AC_MODEL=AC_MODELS.flash;
