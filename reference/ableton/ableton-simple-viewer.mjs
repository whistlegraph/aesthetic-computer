#!/usr/bin/env node

import fs from "fs";
import { SaxesParser } from "saxes";

class AbletonSessionParser {
  constructor() {
    this.tracks = [];
    this.currentTrack = null;
    this.currentClip = null;
    this.currentElement = "";
    this.currentText = "";
    this.tempo = 120;
  }

  parseFile(filePath) {
    const data = fs.readFileSync(filePath, "utf8");
    const parser = new SaxesParser({ fragment: false });

    parser.on("opentag", (tag) => {
      this.currentElement = tag.name;
      
      if (tag.name === "GroupTrack" || tag.name === "AudioTrack" || tag.name === "MidiTrack") {
        this.currentTrack = {
          name: "Track",
          clips: [],
          id: tag.attributes?.Id?.value
        };
      } else if (tag.name === "ClipSlot" || tag.name === "GroupTrackSlot") {
        this.currentClip = {
          id: tag.attributes?.Id?.value,
          hasClip: false,
          name: "",
          length: 0,
          activity: 0,
          color: 0
        };
      } else if (tag.name === "AudioClip" || tag.name === "MidiClip") {
        if (this.currentClip) {
          this.currentClip.hasClip = true;
          this.currentClip.id = tag.attributes?.Id?.value;
          this.currentClip.length = parseFloat(tag.attributes?.Length?.value || 1);
        }
      }
    });

    parser.on("text", (text) => {
      this.currentText = text.trim();
    });

    parser.on("closetag", (tag) => {
      if (tag.name === "EffectiveName" && this.currentText) {
        if (this.currentTrack && !this.currentClip) {
          this.currentTrack.name = this.currentText;
        }
      } else if (tag.name === "ClipSlot" || tag.name === "GroupTrackSlot") {
        if (this.currentTrack && this.currentClip) {
          this.currentTrack.clips.push({ ...this.currentClip });
        }
        this.currentClip = null;
      } else if (tag.name === "GroupTrack" || tag.name === "AudioTrack" || tag.name === "MidiTrack") {
        if (this.currentTrack) {
          this.tracks.push({ ...this.currentTrack });
        }
        this.currentTrack = null;
      }
      
      this.currentText = "";
      this.currentElement = "";
    });

    parser.write(data);
    parser.close();

    return {
      tracks: this.tracks,
      tempo: this.tempo
    };
  }
}

class SimpleSessionVisualizer {
  constructor(sessionData) {
    this.tracks = sessionData.tracks;
    this.tempo = sessionData.tempo;
    this.isPlaying = true;
    this.beat = 0;
    this.trackActivity = new Map();
    this.aggregateOutput = 0;
    
    // Initialize activity
    this.tracks.forEach((track, i) => {
      this.trackActivity.set(i, 0);
    });

    this.startUpdateLoop();
  }

  clearScreen() {
    process.stdout.write('\x1B[2J\x1B[0f');
  }

  render() {
    this.clearScreen();
    
    // Header
    const playIcon = this.isPlaying ? "▶" : "⏸";
    const activeClips = this.getActiveClipCount();
    console.log(`🎛  ${playIcon} Beat: ${this.beat.toFixed(1)} | ${this.tempo} BPM | Active: ${activeClips}`);
    console.log("━".repeat(60));
    
    // Session Grid (simplified)
    console.log("Session Grid:");
    for (let scene = 0; scene < 8; scene++) {
      let line = `${(scene + 1)} │ `;
      
      for (let t = 0; t < Math.min(this.tracks.length, 12); t++) {
        const track = this.tracks[t];
        const clip = track.clips[scene];
        const activity = this.trackActivity.get(t) || 0;
        
        if (clip && clip.hasClip) {
          const intensity = Math.min(Math.floor(activity / 25), 3);
          const symbols = ["▁", "▃", "▅", "█"];
          line += `${symbols[intensity]} `;
        } else {
          line += "░ ";
        }
      }
      console.log(line);
    }
    
    console.log("━".repeat(60));
    
    // Track Activity (condensed)
    console.log("Track Activity:");
    for (let i = 0; i < Math.min(this.tracks.length, 8); i++) {
      const track = this.tracks[i];
      const activity = this.trackActivity.get(i) || 0;
      const barLength = 20;
      const filled = Math.min(Math.floor((activity / 100) * barLength), barLength);
      const empty = barLength - filled;
      
      const trackName = track.name.length > 12 ? track.name.substring(0, 12) : track.name;
      const bar = "█".repeat(filled) + "░".repeat(empty);
      const percentage = Math.floor(activity);
      
      console.log(`${trackName.padEnd(12)} │${bar}│ ${percentage}%`);
    }
    
    // Aggregate
    const aggBar = "█".repeat(Math.floor(this.aggregateOutput / 2)) + "░".repeat(50 - Math.floor(this.aggregateOutput / 2));
    console.log("━".repeat(60));
    console.log(`Aggregate: │${aggBar}│ ${Math.floor(this.aggregateOutput)}%`);
    console.log("SPACE=pause, 1-8=scenes, T=trigger, Q=quit");
  }

  triggerScene(sceneIndex) {
    this.tracks.forEach((track, trackIndex) => {
      const clip = track.clips[sceneIndex];
      if (clip && clip.hasClip) {
        const activity = 50 + Math.random() * 50;
        this.trackActivity.set(trackIndex, activity);
      }
    });
    this.updateAggregateActivity();
  }

  updateAggregateActivity() {
    let total = 0;
    let count = 0;
    
    this.trackActivity.forEach((activity) => {
      if (activity > 0) {
        total += activity;
        count++;
      }
    });
    
    this.aggregateOutput = count > 0 ? total / count : 0;
  }

  getActiveClipCount() {
    let count = 0;
    this.trackActivity.forEach((activity) => {
      if (activity > 10) count++;
    });
    return count;
  }

  startUpdateLoop() {
    setInterval(() => {
      if (this.isPlaying) {
        this.beat += 0.3; // Fast beat progression
      }

      // Decay activity
      this.trackActivity.forEach((activity, index) => {
        const newActivity = Math.max(0, activity - 4); // Fast decay
        this.trackActivity.set(index, newActivity);
      });

      // Frequent random activity spikes
      if (Math.random() < 0.2) {
        const randomTrack = Math.floor(Math.random() * this.tracks.length);
        const currentActivity = this.trackActivity.get(randomTrack) || 0;
        const spike = Math.random() * 50;
        this.trackActivity.set(randomTrack, Math.min(100, currentActivity + spike));
      }

      // Auto-trigger scenes frequently
      if (Math.random() < 0.1) {
        const randomScene = Math.floor(Math.random() * 8);
        this.triggerScene(randomScene);
      }

      this.updateAggregateActivity();
      this.render();
    }, 100); // Fast updates
  }
}

// Handle keyboard input
process.stdin.setRawMode(true);
process.stdin.resume();
process.stdin.setEncoding('utf8');

process.stdin.on('data', (key) => {
  if (key === '\u0003' || key === 'q') { // Ctrl+C or Q
    process.exit();
  }
});

// Main execution
const xmlPath = process.argv[2] || "/workspaces/aesthetic-computer/system/public/assets/wipppps/zzzZWAP_extracted.xml";

if (!fs.existsSync(xmlPath)) {
  console.error(`XML file not found: ${xmlPath}`);
  console.error("Usage: node ableton-simple-viewer.mjs [path-to-extracted.xml]");
  process.exit(1);
}

console.log("🎵 Parsing Ableton Live project...");
const parser = new AbletonSessionParser();
const sessionData = parser.parseFile(xmlPath);

console.log(`📊 Found ${sessionData.tracks.length} tracks`);
console.log(`🚀 Starting simple session viewer (auto-play)...`);

new SimpleSessionVisualizer(sessionData);
