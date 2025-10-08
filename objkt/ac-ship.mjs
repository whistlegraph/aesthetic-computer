#!/usr/bin/env node

// ac-ship.mjs - Package OBJKT packages as Electron desktop apps
// Usage: node ac-ship.mjs [zip-file] [--platforms mac,windows,linux]

import fs from 'fs/promises';
import path from 'path';
import { spawn } from 'child_process';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

class ElectronShipper {
  constructor() {
    this.outputDir = path.join(__dirname, 'output');
    this.tempDir = null; // Will be set based on zip file name
    this.electronDir = null; // Electron project directory
    this.zipBaseName = null;
  }

  async findLatestZip() {
    try {
      // Look in current working directory first, then fallback to output directory
      const searchDirs = [process.cwd(), this.outputDir];
      let allZipFiles = [];
      
      for (const searchDir of searchDirs) {
        try {
          const files = await fs.readdir(searchDir);
          const zipFiles = files
            .filter(file => file.endsWith('.zip'))
            .map(file => ({
              name: file,
              path: path.join(searchDir, file),
              stats: null,
              source: searchDir === process.cwd() ? 'current' : 'output'
            }));
          allZipFiles.push(...zipFiles);
        } catch (error) {
          console.log(`📁 Could not read directory ${searchDir}: ${error.message}`);
        }
      }

      // Get file stats to sort by modification time
      for (const file of allZipFiles) {
        try {
          file.stats = await fs.stat(file.path);
        } catch (error) {
          console.warn(`⚠️ Could not stat ${file.name}:`, error.message);
        }
      }

      // Sort by modification time (newest first), preferring current directory
      const validFiles = allZipFiles.filter(f => f.stats);
      if (validFiles.length === 0) {
        throw new Error('No valid zip files found in current directory or output directory');
      }

      validFiles.sort((a, b) => {
        // Prefer files from current directory
        if (a.source === 'current' && b.source !== 'current') return -1;
        if (b.source === 'current' && a.source !== 'current') return 1;
        // Then sort by modification time (newest first)
        return b.stats.mtime - a.stats.mtime;
      });

      const selectedFile = validFiles[0];
      console.log(`📍 Found in ${selectedFile.source} directory: ${selectedFile.name}`);
      return selectedFile.path;
    } catch (error) {
      throw new Error(`Failed to find zip files: ${error.message}`);
    }
  }

  async extractZip(zipPath) {
    console.log(`📦 Extracting ${path.basename(zipPath)}...`);
    
    // Set directories based on zip file name
    this.zipBaseName = path.basename(zipPath, '.zip');
    this.tempDir = path.join(this.outputDir, `${this.zipBaseName}-temp`);
    this.electronDir = path.join(this.outputDir, `${this.zipBaseName}-electron`);
    
    console.log(`📁 Extract directory: ${this.tempDir}`);
    
    // Clean up existing directories
    try {
      await fs.rm(this.tempDir, { recursive: true, force: true });
      await fs.rm(this.electronDir, { recursive: true, force: true });
    } catch (error) {
      // Directories might not exist, that's okay
    }

    // Create temp directory
    await fs.mkdir(this.tempDir, { recursive: true });

    // Extract using unzip command
    return new Promise((resolve, reject) => {
      const unzip = spawn('unzip', ['-q', zipPath, '-d', this.tempDir], {
        stdio: 'pipe'
      });

      unzip.on('close', (code) => {
        if (code === 0) {
          console.log(`✅ Extracted to ${this.tempDir}`);
          resolve();
        } else {
          reject(new Error(`Unzip failed with code ${code}`));
        }
      });

      unzip.on('error', (error) => {
        reject(new Error(`Unzip command failed: ${error.message}`));
      });
    });
  }

  async createElectronProject() {
    console.log(`🛠️ Creating Electron project structure...`);
    
    // Create electron project directory
    await fs.mkdir(this.electronDir, { recursive: true });
    
    // Create app subdirectory and copy extracted contents
    const appDir = path.join(this.electronDir, 'app');
    await fs.mkdir(appDir, { recursive: true });
    
    // Copy all extracted files to app directory
    const files = await fs.readdir(this.tempDir);
    for (const file of files) {
      const srcPath = path.join(this.tempDir, file);
      const destPath = path.join(appDir, file);
      await this.copyRecursive(srcPath, destPath);
    }
    
    console.log(`📋 Copied package contents to app directory`);

    // Extract piece name from zip file name (format: @author-piecename-timestamp.zip)
    const matches = this.zipBaseName.match(/^(.+?)-([^-]+)-\d{4}\.\d{2}\.\d{2}\.\d{2}\.\d{2}\.\d{2}\.\d{3}$/);
    const author = matches ? matches[1] : '@jeffrey';
    const rawPieceName = matches ? matches[2] : this.zipBaseName.split('-')[0] || 'aesthetic-piece';
    const pieceName = rawPieceName.replace(/[^a-zA-Z0-9-]/g, ''); // Sanitize for package name
    
    // Create package.json
    const packageJson = {
      name: `aesthetic-${pieceName}`,
      version: '1.0.0',
      description: `${pieceName} - An interactive piece from aesthetic.computer`,
      main: 'main.js',
      author: author,
      license: 'MIT',
      repository: {
        type: 'git',
        url: 'https://github.com/digitpain/aesthetic.computer.git'
      },
      scripts: {
        start: 'electron .',
        build: 'electron-builder',
        'build:mac': 'electron-builder --mac',
        'build:win': 'electron-builder --win',
        'build:linux': 'electron-builder --linux'
      },
      devDependencies: {
        electron: 'latest',
        'electron-builder': 'latest'
      },
      build: {
        appId: `computer.aesthetic.${pieceName}`,
        productName: `${rawPieceName}`,
        directories: {
          output: 'dist'
        },
        files: [
          'main.js',
          'app/**/*'
        ],
        publish: null, // Disable publishing
        mac: {
          category: 'public.app-category.entertainment',
          target: [
            {
              target: 'dmg',
              arch: ['x64', 'arm64']
            }
          ]
        },
        win: {
          target: [
            {
              target: 'nsis',
              arch: ['x64']
            }
          ]
        },
        linux: {
          icon: 'build/icon.png',
          category: 'Game',
          target: [
            {
              target: 'AppImage',  
              arch: ['x64']
            }
          ]
        },
        nsis: {
          oneClick: false,
          allowToChangeInstallationDirectory: true
        }
      }
    };

    await fs.writeFile(
      path.join(this.electronDir, 'package.json'),
      JSON.stringify(packageJson, null, 2)
    );

    // Create main.js (Electron main process)
    const mainJs = `const { app, BrowserWindow, Menu } = require('electron');
const path = require('path');

// Suppress common Node.js warnings in Electron
process.removeAllListeners('warning');
process.on('warning', (warning) => {
  // Suppress specific warnings that are common and harmless in Electron
  if (warning.name === 'DeprecationWarning' || 
      warning.message.includes('child process with shell') ||
      warning.message.includes('experimental')) {
    return; // Suppress these warnings
  }
  console.warn(warning.name + ': ' + warning.message);
});

function createWindow() {
  // Create the browser window
  const mainWindow = new BrowserWindow({
    width: 1024,
    height: 768,
    webPreferences: {
      nodeIntegration: false,
      contextIsolation: true,
      webSecurity: true
    },
    icon: path.join(__dirname, 'build/icon.png'),
    show: false, // Don't show until ready-to-show
    titleBarStyle: process.platform === 'darwin' ? 'hiddenInset' : 'default'
  });

  // Suppress protocol errors and other console warnings
  mainWindow.webContents.on('console-message', (event, level, message) => {
    // Suppress specific protocol and CORS errors that are common in Electron
    if (message.includes('ERR_UNKNOWN_URL_SCHEME') || 
        message.includes('protocol') || 
        message.includes('CORS') ||
        message.includes('Mixed Content') ||
        message.includes('net::')) {
      return; // Suppress these messages
    }
    // Let other console messages through
    console.log('Renderer:', message);
  });

  // Load the ac-pack HTML directly
  mainWindow.loadFile('app/index.html');

  // Show window when ready to prevent visual flash
  mainWindow.once('ready-to-show', () => {
    mainWindow.show();
  });

  // Handle window closed
  mainWindow.on('closed', () => {
    // Dereference the window object
    app.quit();
  });

  // Set up menu
  if (process.platform === 'darwin') {
    // macOS menu
    const template = [
      {
        label: app.getName(),
        submenu: [
          { role: 'about' },
          { type: 'separator' },
          { role: 'hide' },
          { role: 'hideothers' },
          { role: 'unhide' },
          { type: 'separator' },
          { role: 'quit' }
        ]
      },
      {
        label: 'View',
        submenu: [
          { role: 'reload' },
          { role: 'forceReload' },
          { role: 'toggleDevTools' },
          { type: 'separator' },
          { role: 'resetZoom' },
          { role: 'zoomIn' },
          { role: 'zoomOut' },
          { type: 'separator' },
          { role: 'togglefullscreen' }
        ]
      },
      {
        label: 'Window',
        submenu: [
          { role: 'minimize' },
          { role: 'close' }
        ]
      }
    ];
    
    const menu = Menu.buildFromTemplate(template);
    Menu.setApplicationMenu(menu);
  } else {
    // Windows/Linux menu
    Menu.setApplicationMenu(null);
  }
}

// This method will be called when Electron has finished initialization
app.whenReady().then(createWindow);

// Quit when all windows are closed, except on macOS
app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') {
    app.quit();
  }
});

app.on('activate', () => {
  // On macOS, re-create window when dock icon is clicked
  if (BrowserWindow.getAllWindows().length === 0) {
    createWindow();
  }
});

// Security: Prevent new window creation
app.on('web-contents-created', (event, contents) => {
  contents.on('new-window', (navigationEvent, navigationURL) => {
    navigationEvent.preventDefault();
    require('electron').shell.openExternal(navigationURL);
  });
});
`;

    await fs.writeFile(path.join(this.electronDir, 'main.js'), mainJs);

    // Create build directory for icons
    const buildDir = path.join(this.electronDir, 'build');
    await fs.mkdir(buildDir, { recursive: true });

    // Create simple icon files (placeholder - could be enhanced with actual icon generation)
    await this.createIcons(buildDir, pieceName);

    console.log(`✅ Created Electron project structure`);
  }

  async createIcons(buildDir, pieceName) {
    // Look for existing icons in the app directory
    const appDir = path.join(this.electronDir, 'app');
    const pieceIcon512 = path.join(appDir, 'icon', '512x512', `${pieceName}.png`); // Preferred piece-named 512x512 icon
    const pieceIcon256 = path.join(appDir, 'icon', '256x256', `${pieceName}.png`); // Fallback piece-named 256x256 icon
    const pieceIcon128 = path.join(appDir, 'icon', '128x128', `${pieceName}.png`); // Legacy piece-named 128x128 icon
    const faviconIcon = path.join(appDir, 'aesthetic.computer', 'favicon.png');
    
    let sourceIcon = null;
    let needsUpscaling = false;
    
    // Try to find an existing icon to use (prioritize piece-named 512x512 icon)
    try {
      await fs.access(pieceIcon512);
      sourceIcon = pieceIcon512;
      console.log(`🎨 Found 512x512 piece icon: ${path.basename(pieceIcon512)}`);
    } catch {
      try {
        await fs.access(pieceIcon256);
        sourceIcon = pieceIcon256;
        needsUpscaling = true;
        console.log(`🎨 Found 256x256 piece icon: ${path.basename(pieceIcon256)}`);
      } catch {
        try {
          await fs.access(pieceIcon128);
          sourceIcon = pieceIcon128;
          needsUpscaling = true;
          console.log(`🎨 Found 128x128 piece icon: ${path.basename(pieceIcon128)}`);
        } catch {
          try {
            await fs.access(faviconIcon);
            sourceIcon = faviconIcon;
            needsUpscaling = true;
            console.log(`📎 Using favicon as fallback icon: ${path.basename(faviconIcon)}`);
          } catch {
            console.log(`⚠️ No existing icons found, skipping icon creation`);
            return;
          }
        }
      }
    }
    
    if (sourceIcon) {
      const outputIcon = path.join(buildDir, 'icon.png');
      
      if (needsUpscaling) {
        // Upscale to 512x512 for Mac compatibility using ffmpeg
        try {
          const { execSync } = await import('child_process');
          execSync(`ffmpeg -i "${sourceIcon}" -vf scale=512:512:flags=neighbor -y "${outputIcon}"`, { stdio: 'pipe' });
          console.log(`✅ Icon upscaled to 512x512 for Mac compatibility`);
        } catch (error) {
          console.log(`⚠️ Failed to upscale icon with ffmpeg, using original: ${error.message}`);
          await fs.copyFile(sourceIcon, outputIcon);
          console.log(`✅ Icon prepared for building (original size)`);
        }
      } else {
        // Use 512x512 icon directly - no upscaling needed
        await fs.copyFile(sourceIcon, outputIcon);
        console.log(`✅ Using 512x512 icon directly - perfect for Mac compatibility`);
      }
    }
  }

  async copyRecursive(src, dest) {
    const stat = await fs.stat(src);
    if (stat.isDirectory()) {
      await fs.mkdir(dest, { recursive: true });
      const files = await fs.readdir(src);
      for (const file of files) {
        await this.copyRecursive(path.join(src, file), path.join(dest, file));
      }
    } else {
      await fs.copyFile(src, dest);
    }
  }

  async installDependencies() {
    console.log(`📦 Installing Electron dependencies...`);
    
    return new Promise((resolve, reject) => {
      const npm = spawn('npm', ['install'], {
        cwd: this.electronDir,
        stdio: 'inherit'
      });

      npm.on('close', (code) => {
        if (code === 0) {
          console.log(`✅ Dependencies installed`);
          resolve();
        } else {
          reject(new Error(`npm install failed with code ${code}`));
        }
      });

      npm.on('error', (error) => {
        reject(new Error(`npm install failed: ${error.message}`));
      });
    });
  }

  async buildElectronApps(platforms = ['mac', 'windows', 'linux']) {
    console.log(`🔧 Building Electron apps for: ${platforms.join(', ')}...`);
    
    const distDir = path.join(this.electronDir, 'dist');
    
    // Map platform names to electron-builder targets
    const targetMap = {
      'mac': 'MAC',
      'windows': 'WINDOWS',
      'linux': 'LINUX'
    };

    const results = [];

    for (const platform of platforms) {
      const target = targetMap[platform];
      if (!target) {
        console.warn(`⚠️ Unknown platform: ${platform}`);
        continue;
      }

      console.log(`🏗️ Building for ${platform}...`);

      try {
        // Dynamic import of electron-builder to handle ESM compatibility
        const { build } = await import('electron-builder');
        const { Platform } = await import('electron-builder');
        
        const buildResult = await build({
          targets: Platform[target.toUpperCase()].createTarget(),
          projectDir: this.electronDir,
          config: {
            directories: {
              output: distDir
            }
          }
        });

        console.log(`✅ Built ${platform} app successfully`);
        results.push({ platform, success: true, files: buildResult });
      } catch (error) {
        console.error(`❌ Failed to build ${platform} app:`, error.message);
        results.push({ platform, success: false, error: error.message });
      }
    }

    return results;
  }

  async cleanup() {
    console.log('🧹 Cleaning up temporary files...');
    
    try {
      if (this.tempDir) {
        await fs.rm(this.tempDir, { recursive: true, force: true });
        console.log('✅ Temporary extraction directory removed');
      }
    } catch (error) {
      console.warn('⚠️ Could not remove temporary directory:', error.message);
    }
  }

  async listResults() {
    const distDir = path.join(this.electronDir, 'dist');
    
    try {
      const files = await fs.readdir(distDir);
      const appFiles = files.filter(file => 
        file.endsWith('.dmg') || 
        file.endsWith('.exe') || 
        file.endsWith('.AppImage') ||
        file.endsWith('.deb') ||
        file.endsWith('.rpm')
      );

      if (appFiles.length > 0) {
        console.log('\n🎉 Generated Electron apps:');
        for (const file of appFiles) {
          const filePath = path.join(distDir, file);
          const stats = await fs.stat(filePath);
          const sizeInMB = (stats.size / (1024 * 1024)).toFixed(1);
          console.log(`   📱 ${file} (${sizeInMB} MB)`);
        }
        console.log(`\n📁 Apps saved to: ${distDir}`);
      } else {
        console.log('\n⚠️ No app files found in dist directory');
      }
    } catch (error) {
      console.warn('⚠️ Could not list results:', error.message);
    }
  }

  async run(zipPath, platforms = ['mac', 'windows', 'linux']) {
    try {
      console.log('🚢 Aesthetic Computer Electron Shipper\n');

      // Find zip file if not provided
      if (!zipPath) {
        console.log('🔍 Finding latest zip file...');
        zipPath = await this.findLatestZip();
        console.log(`📦 Using: ${path.basename(zipPath)}`);
      }

      // Check if zip file exists
      try {
        await fs.access(zipPath);
      } catch (error) {
        throw new Error(`Zip file not found: ${zipPath}`);
      }

      // Extract the zip file
      await this.extractZip(zipPath);

      // Create Electron project structure
      await this.createElectronProject();

      // Install dependencies
      await this.installDependencies();

      // Build Electron apps for specified platforms
      const results = await this.buildElectronApps(platforms);

      // List the results
      await this.listResults();

      // Clean up temporary files
      await this.cleanup();

      // Summary
      const successful = results.filter(r => r.success);
      const failed = results.filter(r => !r.success);

      console.log('\n📊 Build Summary:');
      console.log(`   ✅ Successful: ${successful.map(r => r.platform).join(', ')}`);
      if (failed.length > 0) {
        console.log(`   ❌ Failed: ${failed.map(r => r.platform).join(', ')}`);
        failed.forEach(f => console.log(`      ${f.platform}: ${f.error}`));
      }

      console.log('\n🎯 Next Steps:');
      console.log('1. Test the generated apps on their respective platforms');
      console.log('2. Consider code signing for distribution');
      console.log('3. Upload to GitHub releases or your preferred distribution method');

    } catch (error) {
      console.error('❌ Error:', error.message);
      await this.cleanup();
      process.exit(1);
    }
  }
}

// Parse command line arguments
function parseArgs() {
  const args = process.argv.slice(2);
  let zipPath = null;
  let platforms = ['mac', 'windows', 'linux'];

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    
    if (arg === '--platforms' && i + 1 < args.length) {
      platforms = args[i + 1].split(',').map(p => p.trim());
      i++; // Skip next argument
    } else if (!arg.startsWith('--') && !zipPath) {
      zipPath = arg;
    }
  }

  return { zipPath, platforms };
}

// Export the class for use as a module
export { ElectronShipper };

// Run the shipper if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const { zipPath, platforms } = parseArgs();
  const shipper = new ElectronShipper();
  shipper.run(zipPath, platforms);
}