#!/usr/bin/env node

// ac-unpack.mjs - Extract and test TEIA packages locally
// Usage: node ac-unpack.mjs <zip-file> [port]

import fs from 'fs/promises';
import path from 'path';
import { spawn } from 'child_process';
import { fileURLToPath } from 'url';
import { createReadStream } from 'fs';
import { createWriteStream } from 'fs';
import { pipeline } from 'stream/promises';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

class TeiaUnpacker {
  constructor() {
    this.outputDir = path.join(__dirname, 'output');
    this.testDir = path.join(this.outputDir, 'test-extract');
  }

  async findLatestZip() {
    try {
      const files = await fs.readdir(this.outputDir);
      const zipFiles = files
        .filter(file => file.endsWith('.zip'))
        .map(file => ({
          name: file,
          path: path.join(this.outputDir, file),
          stats: null
        }));

      // Get file stats to sort by modification time
      for (const file of zipFiles) {
        try {
          file.stats = await fs.stat(file.path);
        } catch (error) {
          console.warn(`⚠️ Could not stat ${file.name}:`, error.message);
        }
      }

      // Sort by modification time (newest first)
      const validFiles = zipFiles.filter(f => f.stats);
      if (validFiles.length === 0) {
        throw new Error('No valid zip files found');
      }

      validFiles.sort((a, b) => b.stats.mtime - a.stats.mtime);
      return validFiles[0].path;
    } catch (error) {
      throw new Error(`Failed to find zip files: ${error.message}`);
    }
  }

  async extractZip(zipPath) {
    console.log(`📦 Extracting ${path.basename(zipPath)}...`);
    
    // Clean up existing test directory
    try {
      await fs.rm(this.testDir, { recursive: true, force: true });
    } catch (error) {
      // Directory might not exist, that's okay
    }

    // Create test directory
    await fs.mkdir(this.testDir, { recursive: true });

    // Extract using unzip command
    return new Promise((resolve, reject) => {
      const unzip = spawn('unzip', ['-q', zipPath, '-d', this.testDir], {
        stdio: 'pipe'
      });

      unzip.on('close', (code) => {
        if (code === 0) {
          console.log(`✅ Extracted to ${this.testDir}`);
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

  async startServer(port = 8002) {
    console.log(`🚀 Starting HTTP server on port ${port}...`);
    
    // Kill any existing server on this port
    try {
      await this.killServer(port);
      // Wait a moment for the port to be freed
      await new Promise(resolve => setTimeout(resolve, 1000));
    } catch (error) {
      // No existing server, that's fine
    }

    return new Promise((resolve, reject) => {
      const server = spawn('python', ['-m', 'http.server', port.toString()], {
        cwd: this.testDir,
        stdio: ['ignore', 'pipe', 'pipe']
      });

      let started = false;

      server.stdout.on('data', (data) => {
        const output = data.toString();
        console.log('📡 Server:', output.trim());
        
        if (output.includes('Serving HTTP') && !started) {
          started = true;
          console.log(`✅ Server running at http://localhost:${port}`);
          resolve(server);
        }
      });

      server.stderr.on('data', (data) => {
        const output = data.toString();
        if (output.includes('Address already in use')) {
          reject(new Error(`Port ${port} is already in use`));
        } else {
          console.error('🚨 Server error:', output.trim());
        }
      });

      server.on('close', (code) => {
        if (!started) {
          reject(new Error(`Server failed to start (exit code ${code})`));
        }
      });

      server.on('error', (error) => {
        reject(new Error(`Failed to start server: ${error.message}`));
      });

      // Timeout after 5 seconds
      setTimeout(() => {
        if (!started) {
          server.kill();
          reject(new Error('Server startup timeout'));
        }
      }, 5000);
    });
  }

  async killServer(port = 8002) {
    return new Promise((resolve) => {
      const kill = spawn('pkill', ['-f', `python.*${port}`], {
        stdio: 'pipe'
      });

      kill.on('close', () => {
        resolve();
      });

      kill.on('error', () => {
        resolve(); // pkill not found or no process to kill
      });
    });
  }

  async openBrowser(port = 8002) {
    const url = `http://localhost:${port}`;
    console.log(`🌐 Opening browser at ${url}...`);
    
    // Try to open browser
    try {
      if (process.env.BROWSER) {
        spawn(process.env.BROWSER, [url], { detached: true });
      } else {
        // Try common browser commands
        const browsers = ['xdg-open', 'open', 'start'];
        for (const browser of browsers) {
          try {
            spawn(browser, [url], { detached: true, stdio: 'ignore' });
            break;
          } catch (error) {
            // Try next browser
          }
        }
      }
    } catch (error) {
      console.log(`⚠️ Could not auto-open browser: ${error.message}`);
      console.log(`📋 Manual URL: ${url}`);
    }
  }

  async listContents() {
    try {
      const files = await fs.readdir(this.testDir, { recursive: true });
      console.log('\n📁 Package contents:');
      files.slice(0, 20).forEach(file => {
        console.log(`   ${file}`);
      });
      if (files.length > 20) {
        console.log(`   ... and ${files.length - 20} more files`);
      }
    } catch (error) {
      console.warn('⚠️ Could not list contents:', error.message);
    }
  }

  async checkAssets() {
    try {
      const assetDir = path.join(this.testDir, 'assets', 'type', 'MatrixChunky8');
      const assets = await fs.readdir(assetDir);
      console.log(`\n🔤 Found ${assets.length} MatrixChunky8 font assets`);
      
      // Check for common characters
      const commonChars = ['0030.json', '0041.json', '0061.json']; // 0, A, a
      const found = commonChars.filter(char => assets.includes(char));
      console.log(`✅ Common characters found: ${found.join(', ')}`);
      
      if (found.length < commonChars.length) {
        const missing = commonChars.filter(char => !assets.includes(char));
        console.log(`⚠️ Missing characters: ${missing.join(', ')}`);
      }
    } catch (error) {
      console.warn('⚠️ Could not check font assets:', error.message);
    }
  }

  async run(zipPath, port = 8002) {
    try {
      console.log('🎭 TEIA Package Unpacker\n');

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

      // Extract package
      await this.extractZip(zipPath);

      // List contents
      await this.listContents();

      // Check assets
      await this.checkAssets();

      // Start server
      const server = await this.startServer(port);

      // Open browser
      await this.openBrowser(port);

      console.log('\n🎯 Testing Instructions:');
      console.log('1. Check browser console for any errors');
      console.log('2. Look for MatrixChunky8 glyphs in the QR code corner');
      console.log('3. Verify no API calls to /api/bdf-glyph');
      console.log('4. Test KidLisp functionality');
      console.log('\n⌨️ Press Ctrl+C to stop server and exit');

      // Keep the process alive
      process.on('SIGINT', async () => {
        console.log('\n🛑 Shutting down...');
        server.kill();
        await this.killServer(port);
        console.log('✅ Server stopped');
        process.exit(0);
      });

      // Keep alive
      await new Promise(() => {});

    } catch (error) {
      console.error('❌ Error:', error.message);
      process.exit(1);
    }
  }
}

// Parse command line arguments
const args = process.argv.slice(2);
const zipPath = args[0];
const port = args[1] ? parseInt(args[1]) : 8002;

// Run the unpacker
const unpacker = new TeiaUnpacker();
unpacker.run(zipPath, port);