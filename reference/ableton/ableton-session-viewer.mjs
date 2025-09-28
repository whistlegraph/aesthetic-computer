#!/usr/bin/env node
/**
 * Ableton Live Session View Viewer
 * 
 * A live session view that simulates Ableton's session interface:
 * - Clips trigger and loop in real-time
 * - Aggregated data flows through tracks
 * - Dynamic session grid visualization
 * 
 * Usage:
 *   node ableton-session-viewer.mjs [path-to-extracted.xml]
 */

import { createReadStream, existsSync } from 'node:fs';
import { resolve, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { SaxesParser } from 'saxes';
import chalk from 'chalk';

const __dirname = dirname(fileURLToPath(import.meta.url));

// Default paths
const DEFAULT_XML_PATH = resolve(__dirname, '../system/public/assets/wipppps/zzzZWAP_extracted.xml');
const FALLBACK_XML_PATH = '/Users/jas/Desktop/code/aesthetic-computer/system/public/assets/wipppps/zzzZWAP_extracted.xml';

class AbletonSessionParser {
  constructor() {
    this.tracks = [];
    this.clips = [];
    this.tempo = 120;
    this.scenes = 8; // Number of scenes (rows)
    
    // Parser state
    this.currentPath = [];
    this.currentTrack = null;
    this.currentClip = null;
    this.attributes = {};
    this.textContent = '';
  }

  async parseXML(xmlPath) {
    console.log(chalk.blue('🎛️  Parsing Ableton XML for Session View...'));
    
    const parser = new SaxesParser();
    const stream = createReadStream(xmlPath);
    
    parser.on('opentag', (node) => {
      this.currentPath.push(node.name);
      this.attributes = node.attributes || {};
      this.textContent = '';
      
      if (node.name.endsWith('Track') && node.name !== 'GroupTrackSlot') {
        this.currentTrack = {
          id: this.attributes.Id,
          type: node.name,
          name: '',
          color: Math.floor(Math.random() * 64), // Random color if not found
          clips: [],
          level: 0,
          activity: 0,
          outputData: []
        };
      }
      
      if (node.name.endsWith('Clip') && !node.name.includes('Slot')) {
        this.currentClip = {
          id: this.attributes.Id,
          name: `Clip ${this.clips.length + 1}`,
          duration: 4 + Math.random() * 8, // Random clip length 4-12 beats
          notes: [],
          scene: Math.floor(Math.random() * this.scenes),
          isActive: false,
          loopLength: 4,
          color: Math.floor(Math.random() * 64)
        };
      }
    });

    parser.on('text', (text) => {
      this.textContent += text.trim();
    });

    parser.on('closetag', (node) => {
      const path = this.currentPath.join('/');
      
      if (path.endsWith('Name/EffectiveName') && this.currentTrack) {
        this.currentTrack.name = this.attributes.Value || this.textContent || `Track ${this.tracks.length + 1}`;
      }
      
      if (path.endsWith('Color') && this.currentTrack && this.attributes.Value) {
        this.currentTrack.color = parseInt(this.attributes.Value);
      }
      
      // Extract MIDI notes for session simulation
      if (path.endsWith('KeyTrack/Notes/MidiNoteEvent')) {
        if (this.currentClip) {
          const note = {
            time: parseFloat(this.attributes.Time || Math.random() * 4),
            duration: parseFloat(this.attributes.Duration || 0.25),
            pitch: parseInt(this.attributes.Pitch || 36 + Math.random() * 48),
            velocity: parseInt(this.attributes.Velocity || 80 + Math.random() * 40)
          };
          this.currentClip.notes.push(note);
        }
      }
      
      if (path.endsWith('MasterTrack/DeviceChain/Mixer/Tempo/Manual') && this.attributes.Value) {
        this.tempo = parseFloat(this.attributes.Value);
      }
      
      if (node.name.endsWith('Track') && this.currentTrack) {
        if (this.currentClip) {
          this.currentTrack.clips.push(this.currentClip);
        }
        this.tracks.push(this.currentTrack);
        this.currentTrack = null;
      }
      
      if (node.name.endsWith('Clip') && this.currentClip) {
        // Add some synthetic notes if none found
        if (this.currentClip.notes.length === 0) {
          for (let i = 0; i < 4 + Math.random() * 8; i++) {
            this.currentClip.notes.push({
              time: Math.random() * this.currentClip.duration,
              duration: 0.125 + Math.random() * 0.5,
              pitch: 36 + Math.floor(Math.random() * 48),
              velocity: 60 + Math.random() * 60
            });
          }
        }
        this.clips.push(this.currentClip);
        this.currentClip = null;
      }
      
      this.currentPath.pop();
    });

    return new Promise((resolve, reject) => {
      parser.on('error', reject);
      parser.on('end', () => {
        this.setupSessionData();
        resolve(this.getSessionData());
      });
      
      stream.on('data', chunk => parser.write(chunk));
      stream.on('end', () => parser.close());
      stream.on('error', reject);
    });
  }

  setupSessionData() {
    // Ensure we have enough tracks
    while (this.tracks.length < 6) {
      this.tracks.push({
        id: this.tracks.length,
        type: Math.random() > 0.5 ? 'MidiTrack' : 'AudioTrack',
        name: `Track ${this.tracks.length + 1}`,
        color: Math.floor(Math.random() * 64),
        clips: [],
        level: 0,
        activity: 0,
        outputData: []
      });
    }

    // Distribute clips across tracks and scenes
    this.clips.forEach((clip, index) => {
      const trackIndex = index % this.tracks.length;
      const track = this.tracks[trackIndex];
      clip.trackIndex = trackIndex;
      track.clips.push(clip);
    });

    // Fill empty slots with placeholder clips
    this.tracks.forEach((track, trackIndex) => {
      for (let scene = 0; scene < this.scenes; scene++) {
        const hasClipInScene = track.clips.some(clip => clip.scene === scene);
        if (!hasClipInScene && Math.random() > 0.3) {
          const syntheticClip = {
            id: `synthetic_${trackIndex}_${scene}`,
            name: `Clip ${scene + 1}`,
            duration: 2 + Math.random() * 6,
            notes: [],
            scene: scene,
            isActive: false,
            loopLength: 2 + Math.random() * 6,
            color: track.color,
            trackIndex: trackIndex,
            synthetic: true
          };
          
          // Add some notes
          for (let i = 0; i < 2 + Math.random() * 6; i++) {
            syntheticClip.notes.push({
              time: Math.random() * syntheticClip.duration,
              duration: 0.125 + Math.random() * 0.5,
              pitch: 36 + Math.floor(Math.random() * 48),
              velocity: 40 + Math.random() * 80
            });
          }
          
          track.clips.push(syntheticClip);
          this.clips.push(syntheticClip);
        }
      }
    });
  }

  getSessionData() {
    return {
      tracks: this.tracks,
      clips: this.clips,
      tempo: this.tempo,
      scenes: this.scenes
    };
  }
}

class SessionVisualizer {
  constructor(sessionData) {
    this.data = sessionData;
    this.currentBeat = 0;
    this.isPlaying = false;
    this.startTime = null;
    this.fps = 20;
    
    // Session state
    this.activeClips = new Map(); // clip.id -> { startBeat, loopCount }
    this.trackOutputs = new Map(); // track index -> activity level
    this.aggregateData = [];
    
    // Auto-trigger some clips initially
    this.autoTriggerClips();
    
    this.setupInput();
  }

  autoTriggerClips() {
    // Randomly trigger some clips to start
    const clipsToTrigger = this.data.clips.filter(() => Math.random() > 0.7);
    clipsToTrigger.forEach(clip => {
      this.triggerClip(clip);
    });
  }

  triggerClip(clip) {
    // Stop other clips in the same scene
    this.data.tracks[clip.trackIndex].clips
      .filter(c => c.scene === clip.scene && c.id !== clip.id)
      .forEach(c => {
        this.activeClips.delete(c.id);
        c.isActive = false;
      });
    
    // Start this clip
    this.activeClips.set(clip.id, {
      startBeat: this.currentBeat,
      loopCount: 0
    });
    clip.isActive = true;
    
    // Random chance to trigger clips in other tracks
    if (Math.random() > 0.8) {
      setTimeout(() => {
        const otherTrackClips = this.data.clips.filter(c => 
          c.trackIndex !== clip.trackIndex && !c.isActive && Math.random() > 0.7
        );
        if (otherTrackClips.length > 0) {
          const randomClip = otherTrackClips[Math.floor(Math.random() * otherTrackClips.length)];
          this.triggerClip(randomClip);
        }
      }, 500 + Math.random() * 2000);
    }
  }

  setupInput() {
    if (process.stdin.isTTY) {
      process.stdin.setRawMode(true);
      process.stdin.resume();
      process.stdin.on('data', (key) => {
        const keyStr = key.toString();
        
        if (key[0] === 3 || keyStr === 'q') { // Ctrl+C or 'q'
          this.stop();
        } else if (keyStr === ' ') {
          this.togglePlayback();
        } else if (keyStr >= '1' && keyStr <= '8') {
          // Trigger scene
          const scene = parseInt(keyStr) - 1;
          this.triggerScene(scene);
        } else if (keyStr === 't') {
          // Random trigger
          this.randomTrigger();
        }
      });
    }
  }

  triggerScene(sceneIndex) {
    if (sceneIndex >= this.data.scenes) return;
    
    this.data.tracks.forEach(track => {
      const sceneClips = track.clips.filter(clip => clip.scene === sceneIndex);
      if (sceneClips.length > 0) {
        const clipToTrigger = sceneClips[Math.floor(Math.random() * sceneClips.length)];
        this.triggerClip(clipToTrigger);
      }
    });
  }

  randomTrigger() {
    const inactiveClips = this.data.clips.filter(clip => !clip.isActive);
    if (inactiveClips.length > 0) {
      const randomClip = inactiveClips[Math.floor(Math.random() * inactiveClips.length)];
      this.triggerClip(randomClip);
    }
  }

  start() {
    console.clear();
    console.log(chalk.green('🎛️  Ableton Live Session View'));
    console.log(chalk.gray('Press SPACE to play/pause, 1-8 to trigger scenes, T for random trigger, Q to quit\n'));
    
    this.isPlaying = true;
    this.startTime = Date.now();
    
    this.interval = setInterval(() => {
      if (this.isPlaying) {
        this.updateCurrentBeat();
        this.updateClipStates();
        this.updateTrackOutputs();
        this.updateAggregateData();
      }
      this.render();
    }, 1000 / this.fps);
  }

  updateCurrentBeat() {
    const elapsed = (Date.now() - this.startTime) / 1000;
    const beatsPerSecond = this.data.tempo / 60;
    this.currentBeat = elapsed * beatsPerSecond;
  }

  updateClipStates() {
    for (const [clipId, clipState] of this.activeClips.entries()) {
      const clip = this.data.clips.find(c => c.id === clipId);
      if (!clip) continue;
      
      const elapsed = this.currentBeat - clipState.startBeat;
      
      // Check if clip should loop
      if (elapsed >= clip.loopLength) {
        clipState.startBeat = this.currentBeat;
        clipState.loopCount++;
        
        // Sometimes stop clips after several loops
        if (clipState.loopCount > 2 + Math.random() * 4) {
          if (Math.random() > 0.7) {
            this.activeClips.delete(clipId);
            clip.isActive = false;
          }
        }
      }
    }
  }

  updateTrackOutputs() {
    this.data.tracks.forEach((track, index) => {
      let activity = 0;
      
      // Calculate activity from active clips
      track.clips.forEach(clip => {
        if (clip.isActive) {
          const clipState = this.activeClips.get(clip.id);
          if (clipState) {
            const elapsed = this.currentBeat - clipState.startBeat;
            const position = elapsed % clip.loopLength;
            
            // Check for notes at current position
            clip.notes.forEach(note => {
              if (Math.abs(note.time - position) < 0.1) {
                activity += note.velocity / 127;
              }
            });
          }
        }
      });
      
      // Smooth activity decay
      const currentActivity = this.trackOutputs.get(index) || 0;
      const newActivity = Math.max(activity, currentActivity * 0.9);
      this.trackOutputs.set(index, newActivity);
      track.activity = newActivity;
      track.level = Math.min(1, newActivity);
    });
  }

  updateAggregateData() {
    // Aggregate all track outputs
    let totalActivity = 0;
    this.data.tracks.forEach(track => {
      totalActivity += track.activity;
    });
    
    this.aggregateData.push(totalActivity);
    if (this.aggregateData.length > 60) {
      this.aggregateData.shift();
    }
  }

  togglePlayback() {
    this.isPlaying = !this.isPlaying;
    if (this.isPlaying) {
      this.startTime = Date.now() - (this.currentBeat / (this.data.tempo / 60)) * 1000;
    }
  }

  stop() {
    if (this.interval) {
      clearInterval(this.interval);
    }
    console.log(chalk.yellow('\n👋 Session ended!'));
    process.exit(0);
  }

  getColorFunction(colorIndex) {
    const colors = [
      chalk.red, chalk.green, chalk.yellow, chalk.blue,
      chalk.magenta, chalk.cyan, chalk.white, chalk.gray,
      chalk.redBright, chalk.greenBright, chalk.yellowBright, chalk.blueBright,
      chalk.magentaBright, chalk.cyanBright, chalk.whiteBright
    ];
    return colors[colorIndex % colors.length] || chalk.white;
  }

  renderSessionGrid() {
    const lines = [];
    
    // Header with track names
    let header = '     ';
    this.data.tracks.forEach((track, index) => {
      const trackName = track.name.substring(0, 8).padEnd(8);
      const colorFn = this.getColorFunction(track.color);
      header += colorFn(trackName) + ' ';
    });
    lines.push(header);
    
    // Scene rows
    for (let scene = 0; scene < this.data.scenes; scene++) {
      let row = `${scene + 1}  │ `;
      
      this.data.tracks.forEach((track, trackIndex) => {
        const sceneClips = track.clips.filter(clip => clip.scene === scene);
        
        if (sceneClips.length > 0) {
          const clip = sceneClips[0];
          const colorFn = this.getColorFunction(clip.color);
          
          if (clip.isActive) {
            const clipState = this.activeClips.get(clip.id);
            const elapsed = clipState ? this.currentBeat - clipState.startBeat : 0;
            const position = elapsed % clip.loopLength;
            const intensity = Math.floor((position / clip.loopLength) * 4);
            const symbols = ['▁', '▃', '▅', '█'];
            row += colorFn(`[${symbols[intensity]}${symbols[intensity]}${symbols[intensity]}]`) + ' ';
          } else {
            row += chalk.dim(colorFn('[░░░]')) + ' ';
          }
        } else {
          row += chalk.gray('  ·  ') + ' ';
        }
      });
      
      lines.push(row);
    }
    
    return lines;
  }

  renderTrackMeters() {
    const lines = [];
    
    this.data.tracks.forEach((track, index) => {
      const activity = track.activity;
      const level = Math.floor(activity * 20);
      const meter = '█'.repeat(Math.min(level, 20)) + '░'.repeat(Math.max(0, 20 - level));
      const colorFn = this.getColorFunction(track.color);
      
      const trackName = track.name.substring(0, 10).padEnd(10);
      lines.push(`${colorFn(trackName)} │${colorFn(meter)}│ ${(activity * 100).toFixed(0).padStart(3)}%`);
    });
    
    return lines;
  }

  renderAggregateStream() {
    if (this.aggregateData.length < 2) return '';
    
    const maxVal = Math.max(...this.aggregateData, 1);
    const stream = this.aggregateData.map(val => {
      const normalized = val / maxVal;
      if (normalized > 0.8) return chalk.red('█');
      if (normalized > 0.6) return chalk.yellow('▇');
      if (normalized > 0.4) return chalk.green('▅');
      if (normalized > 0.2) return chalk.cyan('▃');
      return chalk.gray('▁');
    }).join('');
    
    return stream;
  }

  render() {
    process.stdout.write('\x1B[2J\x1B[H');
    
    const statusIcon = this.isPlaying ? '▶️' : '⏸️';
    const activeClipCount = this.activeClips.size;
    
    // Header
    console.log(chalk.bold.blue('🎛️  Ableton Live Session View'));
    console.log(chalk.gray('━'.repeat(80)));
    
    // Status
    console.log(`${statusIcon}  Beat: ${this.currentBeat.toFixed(2)} | Tempo: ${this.data.tempo} BPM | Active Clips: ${activeClipCount}`);
    console.log('');
    
    // Session Grid
    console.log(chalk.bold('Session Grid:'));
    const gridLines = this.renderSessionGrid();
    gridLines.forEach(line => console.log(line));
    
    console.log('');
    
    // Track Meters
    console.log(chalk.bold('Track Activity:'));
    const meterLines = this.renderTrackMeters();
    meterLines.forEach(line => console.log(line));
    
    console.log('');
    
    // Aggregate Data Stream
    console.log(chalk.bold('Aggregate Output:'));
    console.log(this.renderAggregateStream());
    
    console.log('');
    console.log(chalk.gray('Controls: SPACE=play/pause, 1-8=trigger scenes, T=random trigger, Q=quit'));
  }
}

// Main execution
async function main() {
  const xmlPath = process.argv[2] || 
                 (existsSync(DEFAULT_XML_PATH) ? DEFAULT_XML_PATH : FALLBACK_XML_PATH);
  
  if (!existsSync(xmlPath)) {
    console.error(chalk.red('❌ XML file not found:'), xmlPath);
    console.log(chalk.yellow('Usage: node ableton-session-viewer.mjs [path-to-extracted.xml]'));
    process.exit(1);
  }

  try {
    const parser = new AbletonSessionParser();
    const sessionData = await parser.parseXML(xmlPath);
    
    console.log(chalk.green('✅ Session loaded!'));
    console.log(chalk.gray(`Tracks: ${sessionData.tracks.length}`));
    console.log(chalk.gray(`Clips: ${sessionData.clips.length}`));
    console.log(chalk.gray(`Scenes: ${sessionData.scenes}`));
    console.log('');
    
    const visualizer = new SessionVisualizer(sessionData);
    visualizer.start();
    
  } catch (error) {
    console.error(chalk.red('❌ Error:'), error.message);
    process.exit(1);
  }
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}
