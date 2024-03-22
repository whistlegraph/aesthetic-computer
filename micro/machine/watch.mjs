import chokidar from "chokidar";
import fs from "fs";
import open from "open";
import { spawn } from "child_process";

// Function to rebuild and run aesthetic
/*
function rebuildAesthetic() {
  console.log("Rebuilding...");

  const rebuildProcess = spawn("npm", ["run", "reset-and-build"]);

  rebuildProcess.stdout.on("data", (data) => {
    console.log(`stdout: ${data}`);
  });

  rebuildProcess.stderr.on("data", (data) => {
    console.error(`stderr: ${data}`);
  });

  rebuildProcess.on("error", (error) => {
    console.error(`Execution error: ${error}`);
  });

  rebuildProcess.on("close", (code) => {
    if (code === 70) {
      console.log("Build error, attempting rebuild...");
      // rebuildAesthetic();
    } else if (code !== 0) {
      console.error(`rebuildAesthetic process exited with code ${code}`);
    } else {
      console.log("Build completed successfully.");
    }
  });
}
*/

// Directory to watch
const directoryToWatch = ".";

console.log(`Watching for changes...`);

// Initialize watcher
const watcher = chokidar.watch(directoryToWatch, {
  ignored: /(^|[\/\\])\../, // ignore dotfiles
  persistent: true,
});

// Event listener for added files
watcher.on("add", (path) => {
  if (path === "trigger.txt") {
    console.log("Rebuilding aesthetic...");
    // rebuildAesthetic();
    deleteFile(path); // Delete trigger.txt after processing
    watcher.close(); // Optionally stop watching after this event
    return;
  }
  if (path === "url.txt") {
    // Read the contents of url.txt
    fs.readFile(path, "utf8", (err, url) => {
      if (err) {
        console.error("Error reading the file:", err);
        return;
      }
      // Open the URL in the default system browser
      open(url).catch((err) => {
        console.error("Error opening the URL:", err);
      });
      deleteFile(path); // Delete url.txt after processing
    });
    return;
  }
});

// rebuildAesthetic();

function deleteFile(path) {
  fs.unlink(path, (err) => {
    if (err) {
      console.error(`Error deleting ${path}: ${err}`);
      return;
    }
    console.log(`${path} has been deleted`);
  });
}
