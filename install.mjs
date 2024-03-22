import fs from "fs";
import path from "path";
import { spawn } from "child_process";
import { fileURLToPath } from "url";
import { dirname } from "path";

// Helper function to get __dirname in ES module
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Function to execute npm install using spawn for real-time output
const runNpmInstall = (dir) => {
  return new Promise((resolve, reject) => {
    const childProcess = spawn("npm", ["install"], {
      cwd: dir,
      stdio: "inherit",
    });

    childProcess.on("close", (code) => {
      if (code === 0) {
        console.log(`npm install completed in ${dir}`);
        resolve();
      } else {
        console.error(`npm install failed in ${dir} with exit code ${code}`);
        reject(new Error(`Process exited with code ${code}`));
      }
    });
  });
};

// Main function to loop through each top-level directory
const installDependenciesInDirectories = async () => {
  const directories = fs
    .readdirSync(__dirname, { withFileTypes: true })
    .filter((dirent) => dirent.isDirectory())
    .map((dirent) => dirent.name);

  for (const dir of directories) {
    const packageJsonPath = path.join(__dirname, dir, "package.json");
    if (fs.existsSync(packageJsonPath)) {
      console.log(`Running npm install in ${dir}`);
      try {
        await runNpmInstall(path.join(__dirname, dir));
      } catch (error) {
        console.error(`Failed to run npm install in ${dir}: ${error}`);
      }
    }
  }
};

// Run the main function
installDependenciesInDirectories();
