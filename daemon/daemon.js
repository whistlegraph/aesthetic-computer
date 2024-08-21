import { watch, existsSync, unlink } from 'fs';
import { exec, execSync } from 'child_process';
import { join } from 'path';
import { homedir } from 'os';

// Resolve the file path to handle the '~' character
const dirPath = join(homedir(), 'aesthetic-computer/ssl-dev');
const filePath = join(dirPath, '.ssl');

// Get the path to fish dynamically
const fishPath = 'fish';

const command = `${fishPath} ${join(homedir(), 'aesthetic-computer/ssl-dev/fedora-install.fish')} --install-only`;

watch(dirPath, (eventType, filename) => {
  if (eventType === 'rename' && filename === '.ssl') {
    // Check if the .ssl file exists
    if (existsSync(filePath)) {
      exec(command, { shell: true }, (err, stdout, stderr) => {
        if (err) {
          console.error(`Error executing command: ${err.message}`);
          console.error(`stderr: ${stderr}`);
          return;
        }
        console.log(`Command output: ${stdout}`);
        
        // Remove the .ssl file
        unlink(filePath, (err) => {
          if (err) {
            console.error(`Error removing file: ${err.message}`);
            return;
          }
          console.log(`File ${filePath} removed successfully.`);
        });
      });
    }
  }
});

console.log(`Watching for .ssl file creation in ${dirPath}`);
