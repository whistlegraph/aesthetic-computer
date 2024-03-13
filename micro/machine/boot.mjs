import chokidar from 'chokidar';
import fs from 'fs';
import { fileURLToPath } from 'url';
import { exec } from 'child_process';
import path from 'path';

// Function to rebuild and run aesthetic
function rebuildAesthetic() {
  exec(`npm run reset && npm run build`, (error, stdout, stderr) => {
    if (error) {
      console.error(`Execution error: ${error}`);
      return;
    }
    console.log(`stdout: ${stdout}`);
    console.error(`stderr: ${stderr}`);

    // Check for specific exit code (70) and possibly rerun
    if (error && error.code === 70) {
      console.log('Reloading...');
      rebuildAesthetic();
    }
  });
}

// Directory to watch
const directoryToWatch = '.';

console.log(`Watching for the creation of trigger.txt in ${directoryToWatch}`);

// Initialize watcher
const watcher = chokidar.watch(directoryToWatch, {
  ignored: /(^|[\/\\])\../, // ignore dotfiles
  persistent: true
});

// Event listener for added files
watcher.on('add', path => {
  if (path === 'trigger.txt') {
    console.log('trigger.txt was created. Executing code.');
    
    // Execute rebuildAesthetic function when trigger.txt is created
    rebuildAesthetic();

    // Delete trigger.txt after processing
    fs.unlink(path, (err) => {
      if (err) {
        console.error(`Error deleting trigger.txt: ${err}`);
        return;
      }
      console.log('trigger.txt has been deleted');
    });

    // Optionally, you might want to stop watching after this event
    watcher.close();
  }
});

rebuildAesthetic();
