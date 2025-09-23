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
    this.testDir = null; // Will be set based on zip file name
    this.server = null; // Keep reference to Caddy server
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
    
    // Set test directory based on zip file name (without .zip extension)
    const zipBaseName = path.basename(zipPath, '.zip');
    this.testDir = path.join(this.outputDir, zipBaseName);
    
    console.log(`📁 Extract directory: ${this.testDir}`);
    
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
      // Create a simple Caddyfile for this port
      const caddyConfig = `:${port} {
  root * .
  file_server
  header Cache-Control no-cache
  header Access-Control-Allow-Origin *
  header Access-Control-Allow-Methods *
  header Access-Control-Allow-Headers *
  
  # Ensure proper MIME types for .mjs files
  @mjs {
    path *.mjs
  }
  header @mjs Content-Type application/javascript
  
  # Log requests to see what's happening
  log {
    output stdout
    format console
  }
}`;

      // Write Caddyfile to test directory
      const caddyfilePath = path.join(this.testDir, 'Caddyfile');
      
      // Write Caddyfile and start server
      fs.writeFile(caddyfilePath, caddyConfig)
        .then(() => {
          const server = spawn('caddy', ['run', '--config', 'Caddyfile', '--adapter', 'caddyfile'], {
            cwd: this.testDir,
            stdio: ['ignore', 'pipe', 'pipe']
          });

          // Store server reference for cleanup
          this.server = server;

          let started = false;

          server.stdout.on('data', (data) => {
            const output = data.toString();
            console.log('📡 Caddy:', output.trim());
            
            if ((output.includes('serving initial configuration') || output.includes('autosaved config')) && !started) {
              started = true;
              console.log(`✅ Caddy server running at http://localhost:${port}`);
              resolve(server);
            }
          });

          server.stderr.on('data', (data) => {
            const output = data.toString();
            if (output.includes('address already in use') || output.includes('bind: address already in use')) {
              reject(new Error(`Port ${port} is already in use`));
            } else if (output.includes('serving initial configuration') && !started) {
              started = true;
              console.log(`✅ Caddy server running at http://localhost:${port}`);
              resolve(server);
            } else {
              console.log('� Caddy info:', output.trim());
            }
          });

          server.on('close', (code) => {
            if (!started) {
              reject(new Error(`Caddy failed to start (exit code ${code})`));
            }
          });

          server.on('error', (error) => {
            if (error.code === 'ENOENT') {
              reject(new Error('Caddy not found - please install Caddy (https://caddyserver.com/docs/install)'));
            } else {
              reject(new Error(`Failed to start Caddy: ${error.message}`));
            }
          });

          // Shorter timeout for Caddy startup since it usually starts quickly
          setTimeout(() => {
            if (!started) {
              // Give Caddy a chance - if it's gotten this far, it's probably working
              console.log(`✅ Caddy server assumed running at http://localhost:${port}`);
              started = true;
              resolve(server);
            }
          }, 3000);
        })
        .catch(error => {
          reject(new Error(`Failed to write Caddyfile: ${error.message}`));
        });
    });
  }

  async cleanup(port) {
    console.log('\n🧹 Cleaning up...');
    
    try {
      // Kill Caddy server if it exists
      if (this.server && this.server.kill) {
        console.log('🛑 Stopping Caddy server...');
        this.server.kill('SIGTERM');
        this.server = null;
      }

      // Also kill any other Caddy processes on this port
      await this.killServer(port);

      // Delete the extracted directory
      if (this.testDir) {
        console.log(`🗑️ Removing extracted directory: ${this.testDir}`);
        try {
          await fs.rm(this.testDir, { recursive: true, force: true });
          console.log('✅ Extracted directory removed');
        } catch (error) {
          console.warn('⚠️ Could not remove extracted directory:', error.message);
        }
      }

      console.log('✅ Cleanup completed');
    } catch (error) {
      console.warn('⚠️ Error during cleanup:', error.message);
    }
  }

  async killServer(port = 8080) {
    console.log(`🔄 Cleaning up any existing servers on port ${port}...`);
    
    return new Promise((resolve) => {
      // First try to kill any existing Caddy processes on this port
      const kill = spawn('pkill', ['-f', `caddy.*${port}`], {
        stdio: 'pipe'
      });

      kill.on('close', () => {
        // Also try to kill any processes using the port directly
        const killPort = spawn('pkill', ['-f', `:${port}`], {
          stdio: 'pipe'
        });
        
        killPort.on('close', () => {
          // Give a moment for processes to clean up
          setTimeout(() => {
            resolve();
          }, 1000);
        });

        killPort.on('error', () => {
          resolve(); // pkill not found or no process to kill
        });
      });

      kill.on('error', () => {
        resolve(); // pkill not found or no process to kill
      });
    });
  }

  async openBrowser(port = 8080) {
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

  async run(zipPath, port = 8080) {
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
        await this.cleanup(port);
        process.exit(0);
      });

      process.on('SIGTERM', async () => {
        console.log('\n🛑 Received SIGTERM, shutting down...');
        await this.cleanup(port);
        process.exit(0);
      });

      // Also handle uncaught exceptions to ensure cleanup
      process.on('uncaughtException', async (error) => {
        console.error('\n❌ Uncaught exception:', error.message);
        await this.cleanup(port);
        process.exit(1);
      });

      process.on('unhandledRejection', async (reason, promise) => {
        console.error('\n❌ Unhandled rejection at:', promise, 'reason:', reason);
        await this.cleanup(port);
        process.exit(1);
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
const port = args[1] ? parseInt(args[1]) : 8080;

// Run the unpacker
const unpacker = new TeiaUnpacker();
unpacker.run(zipPath, port);