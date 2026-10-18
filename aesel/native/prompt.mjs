import {bundledContext} from '../src/piece-context.mjs';
import {PIECE_INSTRUCTIONS} from '../src/harness-contract.mjs';
import {PIECE_VISUAL,PIECE_RESPONSIVE,PIECE_CLOCK,PIECE_SOUND,PIECE_REPLY} from '../src/piece-prompt.mjs';
import {API_WORKFLOW} from '../src/api-context.mjs';
import {harnessText} from '../src/harness-text.mjs';

export function nativeInstructions(context='') {
  return [
    bundledContext(),
    harnessText('native-identity'),
    PIECE_INSTRUCTIONS,PIECE_VISUAL,PIECE_RESPONSIVE,PIECE_CLOCK,PIECE_SOUND,PIECE_REPLY,API_WORKFLOW,
    harnessText('native-embedded-preview'),
    harnessText('native-capture'),
    harnessText('native-publication'),
    context ? harnessText('prior-conversation', {context}) : '',
  ].filter(Boolean).join('\n\n');
}
