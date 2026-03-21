import test from 'node:test';
import assert from 'node:assert/strict';

import { KidLisp } from '../system/public/aesthetic.computer/lib/kidlisp.mjs';

const COLOR_CODE_MATCH_REGEX = /\\([^\\]+)\\/g;
const COLOR_CODE_TEST_REGEX = /\\[^\\]+\\/;

test('KidLisp HUD highlight strips color codes cleanly', () => {
  const source = `$roz : (fade:red-blue:vertical) (line 50 50 100 100) (ink "cyan")`;
  const kid = new KidLisp();
  kid.initializeSyntaxHighlighting(source);

  const colored = kid.buildColoredKidlispString();
  assert.ok(colored.length > 0, 'colored string should not be empty');
  assert.ok(
    COLOR_CODE_TEST_REGEX.test(colored),
    'colored string should contain inline color escapes',
  );

  COLOR_CODE_MATCH_REGEX.lastIndex = 0;
  const clean = colored.replace(COLOR_CODE_MATCH_REGEX, '');
  assert.ok(!clean.includes('\\'), 'stripped text should not display backslashes');
  assert.ok(clean.includes('line'), 'stripped text should retain Lisp tokens');
  assert.ok(clean.includes('fade'), 'stripped text should preserve fade directives');
});
