import { isKidlispSource } from './system/public/aesthetic.computer/lib/kidlisp.mjs';

const testText = 'prutti~3';
console.log('Testing text:', JSON.stringify(testText));
console.log('isKidlispSource result:', isKidlispSource(testText));

// Test the specific conditions
console.log('Starts with ( or ;:', testText.startsWith('(') || testText.startsWith(';'));
console.log('Contains §:', testText.includes('§'));
console.log('Contains _:', testText.includes('_'));
console.log('Contains \\n:', testText.includes('\n'));

// Check the _ condition more carefully
if (testText.includes('_') && testText.match(/[a-zA-Z_]\w*_[a-zA-Z]/)) {
  console.log('Matches underscore pattern');
  const decoded = testText.replace(/_/g, ' ').replace(/§/g, '\n').replace(/~/g, '\n');
  console.log('Decoded text:', JSON.stringify(decoded));
  console.log('Decoded starts with ( or ;:', decoded.startsWith('(') || decoded.startsWith(';'));
  
  if (decoded.includes('\n')) {
    console.log('Decoded contains newlines');
    const lines = decoded.split('\n');
    console.log('Lines:', lines);
    const hasKidlispLines = lines.some((line) => {
      const trimmed = line.trim();
      console.log('Checking line:', JSON.stringify(trimmed));
      const result = trimmed && (trimmed.startsWith('(') || /^[a-zA-Z_]\w*(\s|$)/.test(trimmed));
      console.log('Line result:', result);
      return result;
    });
    console.log('Has kidlisp lines:', hasKidlispLines);
  } else {
    const trimmed = decoded.trim();
    console.log('Checking trimmed decoded:', JSON.stringify(trimmed));
    const result = /^[a-zA-Z_]\w*(\s|$)/.test(trimmed);
    console.log('Trimmed result:', result);
  }
}
