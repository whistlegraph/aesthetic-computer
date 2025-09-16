#!/usr/bin/env node
/**
 * Ableton Live XML Timeline Viewer
 * 
 * A single-command pipeline that:
 * 1. Parses Ableton XML files and extracts timeline data
 * 2. Displays a live, real-time visual timeline in the terminal
 * 
 * Usage:
 *   node ableton-live-viewer.mjs [path-to-extracted.xml]
 * 
 * If no path provided, looks for zzzZWAP_extracted.xml in the parent wipppps folder
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

class AbletonParser {
  constructor() {
    this.tracks = [];
    this.clips = [];
    this.locators = [];
    this.tempo = 120;
    this.timeSignature = [4, 4];
    this.totalLength = 0;
    this.notes = [];
    
    // Parser state
    this.currentPath = [];
    this.currentTrack = null;
    this.currentClip = null;
    this.attributes = {};
    this.textContent = '';
  }

  async parseXML(xmlPath) {
    console.log(chalk.blue('🎵 Parsing Ableton XML...'));
    
    const parser = new SaxesParser();
    const stream = createReadStream(xmlPath);
    
    parser.on('opentag', (node) => {
      this.currentPath.push(node.name);
      this.attributes = node.attributes || {};
      this.textContent = '';
      
      // Track detection
      if (node.name.endsWith('Track') && node.name !== 'GroupTrackSlot') {
        this.currentTrack = {
          id: this.attributes.Id,
          type: node.name,
          name: '',
          color: '',
          clips: []
        };
      }
      
      // Clip detection  
      if (node.name.endsWith('Clip') && !node.name.includes('Slot')) {
        this.currentClip = {
          id: this.attributes.Id,
          name: '',
          start: 0,
          end: 0,
          loop: false,
          notes: []
        };
      }
    });

    parser.on('text', (text) => {
      this.textContent += text.trim();
    });

    parser.on('closetag', (node) => {
      const path = this.currentPath.join('/');
      
      // Extract track name
      if (path.endsWith('Name/EffectiveName') && this.currentTrack) {
        this.currentTrack.name = this.attributes.Value || this.textContent;
      }
      
      // Extract track color
      if (path.endsWith('Color') && this.currentTrack && this.attributes.Value) {
        this.currentTrack.color = parseInt(this.attributes.Value);
      }
      
      // Extract clip timing
      if (this.currentClip) {
        if (path.endsWith('CurrentStart') && this.attributes.Value) {
          this.currentClip.start = parseFloat(this.attributes.Value);
        }
        if (path.endsWith('CurrentEnd') && this.attributes.Value) {
          this.currentClip.end = parseFloat(this.attributes.Value);
        }
      }
      
      // Extract MIDI notes
      if (path.endsWith('KeyTrack/Notes/MidiNoteEvent')) {
        if (this.currentClip) {
          const note = {
            time: parseFloat(this.attributes.Time || 0),
            duration: parseFloat(this.attributes.Duration || 0.25),
            pitch: parseInt(this.attributes.Pitch || 60),
            velocity: parseInt(this.attributes.Velocity || 100)
          };
          this.currentClip.notes.push(note);
          this.notes.push({
            ...note,
            track: this.currentTrack?.name || 'Unknown',
            clipStart: this.currentClip.start
          });
        }
      }
      
      // Extract tempo
      if (path.endsWith('MasterTrack/DeviceChain/Mixer/Tempo/Manual') && this.attributes.Value) {
        this.tempo = parseFloat(this.attributes.Value);
      }
      
      // Track completion
      if (node.name.endsWith('Track') && this.currentTrack) {
        if (this.currentClip) {
          this.currentTrack.clips.push(this.currentClip);
        }
        this.tracks.push(this.currentTrack);
        this.currentTrack = null;
      }
      
      // Clip completion
      if (node.name.endsWith('Clip') && this.currentClip) {
        this.currentClip = null;
      }
      
      this.currentPath.pop();
    });

    return new Promise((resolve, reject) => {
      parser.on('error', reject);
      parser.on('end', () => {
        this.calculateTotalLength();
        resolve(this.getTimelineData());
      });
      
      stream.on('data', chunk => parser.write(chunk));
      stream.on('end', () => parser.close());
      stream.on('error', reject);
    });
  }

  calculateTotalLength() {
    let maxTime = 0;
    this.notes.forEach(note => {
      const noteEnd = note.clipStart + note.time + note.duration;
      if (noteEnd > maxTime) maxTime = noteEnd;
    });
    this.tracks.forEach(track => {
      track.clips.forEach(clip => {
        if (clip.end > maxTime) maxTime = clip.end;
      });
    });
    this.totalLength = Math.max(maxTime, 64); // Minimum 64 beats
  }

  getTimelineData() {
    return {
      tracks: this.tracks,
      clips: this.clips,
      notes: this.notes,
      tempo: this.tempo,
      timeSignature: this.timeSignature,
      totalLength: this.totalLength
    };
  }
}

class LiveVisualizer {
  constructor(timelineData) {
    this.data = timelineData;
    this.currentBeat = 0;
    this.isPlaying = false;
    this.startTime = null;
    this.playbackRate = 1.0;
    this.fps = 30;
    
    // Visual settings
    this.terminalWidth = process.stdout.columns || 120;
    this.timelineWidth = Math.min(80, this.terminalWidth - 40);
    
    this.setupInput();
  }

  setupInput() {
    if (process.stdin.isTTY) {
      process.stdin.setRawMode(true);
      process.stdin.resume();
      process.stdin.on('data', (key) => {
        if (key[0] === 3 || key.toString() === 'q') { // Ctrl+C or 'q'
          this.stop();
        } else if (key.toString() === ' ') {
          this.togglePlayback();
        }
      });
    }
  }

  start() {
    console.clear();
    console.log(chalk.green('🎵 Ableton Live Timeline Viewer'));
    console.log(chalk.gray('Press SPACE to play/pause, Q to quit\n'));
    
    this.isPlaying = true;
    this.startTime = Date.now();
    this.render();
    
    this.interval = setInterval(() => {
      if (this.isPlaying) {
        this.updateCurrentBeat();
      }
      this.render();
    }, 1000 / this.fps);
  }

  updateCurrentBeat() {
    const elapsed = (Date.now() - this.startTime) / 1000;
    const beatsPerSecond = (this.data.tempo / 60) * this.playbackRate;
    this.currentBeat = elapsed * beatsPerSecond;
    
    if (this.currentBeat >= this.data.totalLength) {
      this.currentBeat = 0;
      this.startTime = Date.now();
    }
  }

  togglePlayback() {
    this.isPlaying = !this.isPlaying;
    if (this.isPlaying) {
      this.startTime = Date.now() - (this.currentBeat / ((this.data.tempo / 60) * this.playbackRate)) * 1000;
    }
  }

  stop() {
    if (this.interval) {
      clearInterval(this.interval);
    }
    console.log(chalk.yellow('\n👋 Goodbye!'));
    process.exit(0);
  }

  formatTime(beats) {
    const totalSeconds = beats / (this.data.tempo / 60);
    const minutes = Math.floor(totalSeconds / 60);
    const seconds = Math.floor(totalSeconds % 60);
    const beatInMeasure = Math.floor(beats % 4) + 1;
    return `${minutes.toString().padStart(2, '0')}:${seconds.toString().padStart(2, '0')}.${beatInMeasure}`;
  }

  getActiveNotes() {
    return this.data.notes.filter(note => {
      const noteStart = note.clipStart + note.time;
      const noteEnd = noteStart + note.duration;
      return this.currentBeat >= noteStart && this.currentBeat <= noteEnd;
    });
  }

  getRecentNotes(windowBeats = 0.5) {
    return this.data.notes.filter(note => {
      const noteStart = note.clipStart + note.time;
      return this.currentBeat >= noteStart && this.currentBeat <= noteStart + windowBeats;
    });
  }

  renderProgressBar() {
    const progress = this.currentBeat / this.data.totalLength;
    const filled = Math.floor(progress * this.timelineWidth);
    const empty = this.timelineWidth - filled;
    
    const bar = '█'.repeat(filled) + '░'.repeat(empty);
    const percentage = (progress * 100).toFixed(1);
    
    return chalk.cyan(bar) + ` ${percentage}%`;
  }

  renderTrackActivity() {
    const recentNotes = this.getRecentNotes();
    const trackActivity = {};
    
    // Group recent notes by track
    recentNotes.forEach(note => {
      if (!trackActivity[note.track]) {
        trackActivity[note.track] = [];
      }
      trackActivity[note.track].push(note);
    });

    const lines = [];
    this.data.tracks.forEach(track => {
      if (!track.name) return;
      
      const activity = trackActivity[track.name] || [];
      const symbols = activity.map(note => {
        const pitchClass = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'][note.pitch % 12];
        return pitchClass;
      }).join('');
      
      const colorCode = [
        chalk.red, chalk.green, chalk.yellow, chalk.blue, 
        chalk.magenta, chalk.cyan, chalk.white, chalk.gray
      ][track.color % 8] || chalk.white;
      
      const trackName = track.name.substring(0, 12).padEnd(12);
      const activityBar = symbols.padEnd(20).substring(0, 20);
      
      lines.push(colorCode(`${trackName} │ ${activityBar}`));
    });

    return lines;
  }

  renderNoteStream() {
    const recentNotes = this.getRecentNotes(2).slice(-40); // Last 40 notes
    const stream = recentNotes.map(note => {
      const pitchClass = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'][note.pitch % 12];
      const octave = Math.floor(note.pitch / 12);
      return chalk.yellow(`${pitchClass}${octave}`);
    }).join(' ');
    
    return stream;
  }

  render() {
    // Clear previous frame
    process.stdout.write('\x1B[2J\x1B[H');
    
    const timeDisplay = this.formatTime(this.currentBeat);
    const statusIcon = this.isPlaying ? '▶️' : '⏸️';
    const activeNotes = this.getActiveNotes();
    
    // Header
    console.log(chalk.bold.blue('🎵 Ableton Live Timeline Viewer'));
    console.log(chalk.gray('━'.repeat(this.terminalWidth)));
    
    // Time and status
    console.log(`${statusIcon}  ${chalk.bold(timeDisplay)} | Tempo: ${this.data.tempo} BPM | Beat: ${this.currentBeat.toFixed(2)}`);
    
    // Progress bar
    console.log(`Progress: ${this.renderProgressBar()}`);
    
    // Active notes count
    console.log(chalk.green(`Active Notes: ${activeNotes.length}`));
    console.log('');
    
    // Track activity
    console.log(chalk.bold('Track Activity:'));
    console.log(chalk.gray('Track Name   │ Recent Notes'));
    console.log(chalk.gray('─'.repeat(35)));
    
    const trackLines = this.renderTrackActivity();
    trackLines.forEach(line => console.log(line));
    
    // Note stream
    console.log('');
    console.log(chalk.bold('Recent Notes:'));
    console.log(this.renderNoteStream());
    
    // Controls
    console.log('');
    console.log(chalk.gray('Controls: SPACE = play/pause, Q = quit'));
  }
}

// Main execution
async function main() {
  const xmlPath = process.argv[2] || 
                 (existsSync(DEFAULT_XML_PATH) ? DEFAULT_XML_PATH : FALLBACK_XML_PATH);
  
  if (!existsSync(xmlPath)) {
    console.error(chalk.red('❌ XML file not found:'), xmlPath);
    console.log(chalk.yellow('Usage: node ableton-live-viewer.mjs [path-to-extracted.xml]'));
    process.exit(1);
  }

  try {
    const parser = new AbletonParser();
    const timelineData = await parser.parseXML(xmlPath);
    
    console.log(chalk.green('✅ Parsed successfully!'));
    console.log(chalk.gray(`Tracks: ${timelineData.tracks.length}`));
    console.log(chalk.gray(`Notes: ${timelineData.notes.length}`));
    console.log(chalk.gray(`Total Length: ${timelineData.totalLength.toFixed(2)} beats`));
    console.log('');
    
    const visualizer = new LiveVisualizer(timelineData);
    visualizer.start();
    
  } catch (error) {
    console.error(chalk.red('❌ Error:'), error.message);
    process.exit(1);
  }
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}
