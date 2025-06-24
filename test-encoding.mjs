// Test the new KidLisp URL encoding
import { encodeKidlispForUrl, decodeKidlispFromUrl } from './system/public/aesthetic.computer/lib/kidlisp.mjs';

const testCode = '(wipe orange) (ink black) (write "abcdefghijklmnopqrstuvwxyz" 2 2 white) (scroll (% frame width))';

const longTestCode = `(wipe blue)
(ink white)
(line 10 10 100 100)
(circle 50 50 25)
(box 20 20 60 60 red)
(write "This is a very long kidlisp program that includes multiple functions and should benefit from compression encoding when it gets converted to a URL slug for sharing with other users around the world" 10 200)
(def counter 0)
(later increment () (set counter (+ counter 1)))
(repeat 100 (increment) (circle (+ 50 counter) (+ 50 counter) 5))`;

function testEncoding(code, label) {
  console.log(`\n=== ${label} ===`);
  console.log('Original code:');
  console.log(code);
  console.log('Length:', code.length);

  // Test old encoding (character substitution)
  const oldEncoded = code
    .replace(/ /g, "_")
    .replace(/\n/g, "~")
    .replace(/%/g, "÷")
    .replace(/"/g, "`");

  console.log('\nOld encoding:');
  console.log(oldEncoded);
  console.log('Length:', oldEncoded.length);

  // Test new encoding
  const newEncoded = encodeKidlispForUrl(code);

  console.log('\nNew encoding:');
  console.log(newEncoded);
  console.log('Length:', newEncoded.length);

  // Test decoding
  const decoded = decodeKidlispFromUrl(newEncoded);
  console.log('\nDecoded matches original:', decoded === code);

  // Show URL lengths
  console.log('\nURL comparison:');
  console.log('Old URL length:', `https://localhost:8888/${oldEncoded}`.length);
  console.log('New URL length:', `https://localhost:8888/${newEncoded}`.length);
  const reduction = ((oldEncoded.length - newEncoded.length) / oldEncoded.length * 100);
  console.log('Size change:', reduction.toFixed(1) + '%' + (reduction > 0 ? ' smaller' : ' larger'));
}

testEncoding(testCode, 'Short KidLisp Code');
testEncoding(longTestCode, 'Long KidLisp Code');
