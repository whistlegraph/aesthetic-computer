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
      } else if (tag.name === "Tempo") {
        this.tempo = parseFloat(tag.attributes?.Manual?.value || 120);
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
      } else if (tag.name === "Name" && this.currentText) {
        if (this.currentClip) {
          this.currentClip.name = this.currentText;
        }
      } else if (tag.name === "ColorIndex" && this.currentClip && this.currentText) {
        this.currentClip.color = parseInt(this.currentText);
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
    this.isPlaying = false;
    this.beat = 0;
    this.trackActivity = new Map();
    
    // Initialize activity
    this.tracks.forEach((track, i) => {
      this.trackActivity.set(i, 0);
    });

    this.setupKeyHandlers();
    this.startUpdateLoop();
  }

  setupKeyHandlers() {
    process.stdin.setRawMode(true);
    process.stdin.resume();
    process.stdin.on('data', (key) => {
      const char = key.toString();
      
      if (char === '\u0003' || char === 'q') { // Ctrl+C or q
        process.exit(0);
      } else if (char === ' ') {
        this.isPlaying = !this.isPlaying;
      } else if (char >= '1' && char <= '8') {
        this.triggerScene(parseInt(char) - 1);
      } else if (char === 't') {
        this.triggerRandomClips();
      } else if (char === 'r') {
        this.reset();
      }
    });
  }

  render() {
    console.clear();
    
    // Header
    const playIcon = this.isPlaying ? "▶️" : "⏸️";
    const activeClips = this.getActiveClipCount();
    console.log(`🎛️  Ableton Live Session View ${playIcon}  Beat: ${this.beat.toFixed(1)} | ${this.tempo} BPM | Active: ${activeClips}`);
    console.log('');

    // Track headers
    let headerLine = '     ';
    for (let t = 0; t < Math.min(this.tracks.length, 12); t++) {
      const track = this.tracks[t];
      const trackName = track.name.length > 8 ? track.name.substring(0, 8) : track.name;
      headerLine += trackName.padEnd(10);
    }
    console.log(headerLine);
    console.log('');

    // Session grid (8 scenes)
    for (let scene = 0; scene < 8; scene++) {
      let line = `${(scene + 1).toString().padStart(2)}  │ `;
      
      for (let t = 0; t < Math.min(this.tracks.length, 12); t++) {
        const track = this.tracks[t];
        const clip = track.clips[scene];
        const activity = this.trackActivity.get(t) || 0;
        
        if (clip && clip.hasClip) {
          const intensity = Math.min(Math.floor(activity / 25), 3);
          const symbols = ["▁▁▁", "▃▃▃", "▅▅▅", "███"];
          line += `[${symbols[intensity]}] `;
        } else {
          line += "[░░░] ";
        }
      }
      console.log(line);
    }
    
    console.log('');

    // Track activity meters
    for (let i = 0; i < Math.min(this.tracks.length, 12); i++) {
      const track = this.tracks[i];
      const activity = this.trackActivity.get(i) || 0;
      const barLength = 20;
      const filled = Math.min(Math.floor((activity / 100) * barLength), barLength);
      const empty = barLength - filled;
      
      const trackName = track.name.length > 12 ? track.name.substring(0, 12) : track.name;
      const bar = "█".repeat(filled) + "░".repeat(empty);
      const percentage = Math.floor(activity);
      
      console.log(`${trackName.padEnd(13)} │${bar}│ ${percentage.toString().padStart(3)}%`);
    }

    console.log('');

    // Aggregate output
    const totalActivity = Array.from(this.trackActivity.values()).reduce((sum, val) => sum + val, 0);
    const avgActivity = this.trackActivity.size > 0 ? totalActivity / this.trackActivity.size : 0;
    const aggregateBarLength = 40;
    const aggregateFilled = Math.min(Math.floor((avgActivity / 100) * aggregateBarLength), aggregateBarLength);
    const aggregateEmpty = aggregateBarLength - aggregateFilled;
    const aggregateBar = "█".repeat(aggregateFilled) + "░".repeat(aggregateEmpty);
    
    console.log(`Aggregate Output: │${aggregateBar}│ ${Math.floor(avgActivity)}%`);
    console.log('');
    console.log('Controls: SPACE=play/pause, 1-8=trigger scenes, T=random, R=reset, Q=quit');
  }

  triggerScene(sceneIndex) {
    this.tracks.forEach((track, trackIndex) => {
      const clip = track.clips[sceneIndex];
      if (clip && clip.hasClip) {
        const activity = 50 + Math.random() * 50;
        this.trackActivity.set(trackIndex, activity);
      }
    });
  }

  triggerRandomClips() {
    this.tracks.forEach((track, trackIndex) => {
      if (Math.random() < 0.3) {
        const activity = 30 + Math.random() * 70;
        this.trackActivity.set(trackIndex, activity);
      }
    });
  }

  getActiveClipCount() {
    let count = 0;
    this.trackActivity.forEach((activity) => {
      if (activity > 10) count++;
    });
    return count;
  }

  reset() {
    this.trackActivity.forEach((_, index) => {
      this.trackActivity.set(index, 0);
    });
    this.beat = 0;
  }

  startUpdateLoop() {
    setInterval(() => {
      if (this.isPlaying) {
        this.beat += 0.1;
      }

      // Decay activity
      this.trackActivity.forEach((activity, index) => {
        const newActivity = Math.max(0, activity - 2);
        this.trackActivity.set(index, newActivity);
      });

      // Random activity spikes
      if (Math.random() < 0.05) {
        const randomTrack = Math.floor(Math.random() * this.tracks.length);
        const currentActivity = this.trackActivity.get(randomTrack) || 0;
        const spike = Math.random() * 30;
        this.trackActivity.set(randomTrack, Math.min(100, currentActivity + spike));
      }

      this.render();
    }, 100);
  }
}

// Main execution
const xmlPath = process.argv[2] || "/workspaces/aesthetic-computer/system/public/assets/wipppps/zzzZWAP_extracted.xml";

if (!fs.existsSync(xmlPath)) {
  console.error(`XML file not found: ${xmlPath}`);
  console.error("Usage: node ableton-session-simple.mjs [path-to-extracted.xml]");
  process.exit(1);
}

console.log("🎵 Parsing Ableton Live project...");
const parser = new AbletonSessionParser();
const sessionData = parser.parseFile(xmlPath);

console.log(`📊 Found ${sessionData.tracks.length} tracks`);
console.log(`🎯 Tempo: ${sessionData.tempo} BPM`);
console.log("🚀 Starting simple session viewer...");
console.log("");

new SimpleSessionVisualizer(sessionData);
