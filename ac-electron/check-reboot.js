// Debug script to check reboot marker
const path = require('path');
const fs = require('fs');

console.log('__dirname:', __dirname);
console.log('process.cwd():', process.cwd());
console.log('Checking for marker at:', path.join(__dirname, '.reboot-requested'));
console.log('Exists?', fs.existsSync(path.join(__dirname, '.reboot-requested')));

if (fs.existsSync(path.join(__dirname, '.reboot-requested'))) {
  const content = fs.readFileSync(path.join(__dirname, '.reboot-requested'), 'utf-8');
  console.log('Marker content:', content);
}
