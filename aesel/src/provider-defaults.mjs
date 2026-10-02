import {OPEN_MODELS} from './open-models.mjs';
export const DEFAULT_CLAUDE_MODEL='claude-opus-5';
export const AC_MODELS={...OPEN_MODELS,luna:'openai/gpt-5.6-luna',opus:'anthropic/claude-opus-5',sonnet:'anthropic/claude-sonnet-4.6',gpt:'openai/gpt-5.4'};
export const DEFAULT_AC_MODEL=AC_MODELS.flash;
