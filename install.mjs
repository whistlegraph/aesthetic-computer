import fs from "fs";
import path from "path";
import { spawn } from "child_process";
import { fileURLToPath } from "url";
import { dirname } from "path";

// Helper function to get __dirname in ES module
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Function to check if directory has node_modules
const hasNodeModules = (dir) => {
  const nodeModulesPath = path.join(dir, "node_modules");
  try {
    const stat = fs.statSync(nodeModulesPath);
    if (!stat.isDirectory()) return false;
    const contents = fs.readdirSync(nodeModulesPath);
    return contents.length > 0;
  } catch (err) {
    return false;
  }
};

// Function to execute npm install using spawn for real-time output
const runNpmInstall = (dir) => {
  return new Promise((resolve, reject) => {
    // Check if already has node_modules
    if (hasNodeModules(dir)) {
      console.log(`✅ ${path.basename(dir)} already has node_modules, skipping`);
      resolve();
      return;
    }

    console.log(`📦 Installing dependencies in ${path.basename(dir)}...`);
    
    // Try npm ci first (faster for clean installs)
    let childProcess = spawn("npm", ["ci", "--no-fund", "--no-audit", "--silent"], {
      cwd: dir,
      stdio: ["ignore", "pipe", "pipe"],
    });

    let output = "";
    let errorOutput = "";
    
    childProcess.stdout?.on("data", (data) => {
      output += data.toString();
    });
    
    childProcess.stderr?.on("data", (data) => {
      errorOutput += data.toString();
    });

    childProcess.on("close", (code) => {
      if (code === 0) {
        console.log(`✅ npm install completed in ${path.basename(dir)}`);
        resolve();
      } else {
        console.log(`⚠️  npm ci failed in ${path.basename(dir)}, trying npm install...`);
        
        // Fallback to npm install
        const fallbackProcess = spawn("npm", ["install", "--no-fund", "--no-audit", "--silent"], {
          cwd: dir,
          stdio: ["ignore", "pipe", "pipe"],
        });

        let fallbackOutput = "";
        let fallbackErrorOutput = "";
        
        fallbackProcess.stdout?.on("data", (data) => {
          fallbackOutput += data.toString();
        });
        
        fallbackProcess.stderr?.on("data", (data) => {
          fallbackErrorOutput += data.toString();
        });

        fallbackProcess.on("close", (fallbackCode) => {
          if (fallbackCode === 0) {
            console.log(`✅ npm install completed in ${path.basename(dir)}`);
            resolve();
          } else {
            console.error(`❌ npm install failed in ${path.basename(dir)} with exit code ${fallbackCode}`);
            if (fallbackErrorOutput) {
              console.error("Error output:", fallbackErrorOutput);
            }
            reject(new Error(`Process exited with code ${fallbackCode}`));
          }
        });
      }
    });
  });
};

// Function to recursively find all directories with package.json
const findPackageJsonDirectories = (basePath, maxDepth = 2) => {
  const results = [];
  
  const scanDirectory = (currentPath, depth) => {
    if (depth > maxDepth) return;
    
    try {
      const entries = fs.readdirSync(currentPath, { withFileTypes: true });
      
      // Check if current directory has package.json
      if (entries.some(entry => entry.name === "package.json")) {
        results.push(currentPath);
      }
      
      // Recursively scan subdirectories (avoiding node_modules and other common exclusions)
      for (const entry of entries) {
        if (entry.isDirectory() && 
            !entry.name.startsWith('.') && 
            entry.name !== 'node_modules' &&
            entry.name !== 'target' &&
            entry.name !== 'dist' &&
            entry.name !== 'build' &&
            entry.name !== 'archive') {  // Skip archive directory
          scanDirectory(path.join(currentPath, entry.name), depth + 1);
        }
      }
    } catch (err) {
      // Skip directories we can't read
      console.log(`⚠️  Skipping ${currentPath}: ${err.message}`);
    }
  };
  
  scanDirectory(basePath, 0);
  return results;
};

// Main function to loop through directories with package.json
const installDependenciesInDirectories = async () => {
  console.log("🔍 Finding all directories with package.json files...");
  const packageJsonDirs = findPackageJsonDirectories(__dirname);
  
  console.log(`📋 Found ${packageJsonDirs.length} directories with package.json:`);
  packageJsonDirs.forEach(dir => {
    const relativePath = path.relative(__dirname, dir);
    console.log(`   ${relativePath || '.'}`);
  });
  
  let successCount = 0;
  let skippedCount = 0;
  let failureCount = 0;

  for (const dir of packageJsonDirs) {
    const relativePath = path.relative(__dirname, dir) || '.';
    try {
      if (hasNodeModules(dir)) {
        skippedCount++;
        continue;
      }
      await runNpmInstall(dir);
      successCount++;
    } catch (error) {
      console.error(`❌ Failed to run npm install in ${relativePath}: ${error.message}`);
      failureCount++;
    }
  }
  
  console.log("\n📊 Installation Summary:");
  console.log(`   ✅ Successful installations: ${successCount}`);
  console.log(`   ⏭️  Skipped (already installed): ${skippedCount}`);
  console.log(`   ❌ Failed installations: ${failureCount}`);
};

// Run the main function
installDependenciesInDirectories();
