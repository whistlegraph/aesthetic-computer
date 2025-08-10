#!/usr/bin/env node

import blessed from "blessed";
import fs from "fs";
import { SaxesParser } from "saxes";

class AbletonSessionParser {
  constructor() {
    this.tracks = [];
    this.currentTrack = null;
    this.currentClip = null;
    this.clipIdToSlot = new Map();
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
          if (this.currentClip.id) {
            this.clipIdToSlot.set(this.currentClip.id, {
              trackIndex: this.tracks.length,
              clipIndex: this.currentTrack.clips.length - 1
            });
          }
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
      tempo: this.tempo,
      clipIdToSlot: this.clipIdToSlot
    };
  }
}

class BlessedSessionVisualizer {
  constructor(sessionData) {
    this.tracks = sessionData.tracks;
    this.tempo = sessionData.tempo;
    this.clipIdToSlot = sessionData.clipIdToSlot;
    this.isPlaying = true; // Auto-play enabled
    this.beat = 0;
    this.trackActivity = new Map();
    this.aggregateOutput = 0;
    this.sceneColors = ["red", "yellow", "green", "blue", "magenta", "cyan", "white", "gray"];
    
    // Initialize activity
    this.tracks.forEach((track, i) => {
      this.trackActivity.set(i, 0);
    });

    this.setupUI();
    this.setupKeyHandlers();
    this.startUpdateLoop();
  }

  setupUI() {
    // Create screen
    this.screen = blessed.screen({
      smartCSR: true,
      title: "Ableton Live Session View"
    });

    // Header box
    this.headerBox = blessed.box({
      parent: this.screen,
      top: 0,
      left: 0,
      width: "100%",
      height: 3,
      content: "",
      tags: true,
      border: {
        type: "line"
      },
      style: {
        fg: "white",
        bg: "black",
        border: {
          fg: "cyan"
        }
      }
    });

    // Session grid box
    this.gridBox = blessed.box({
      parent: this.screen,
      top: 3,
      left: 0,
      width: "75%",
      height: "60%",
      content: "",
      tags: true,
      border: {
        type: "line"
      },
      style: {
        fg: "white",
        bg: "black",
        border: {
          fg: "green"
        }
      }
    });

    // Track activity box
    this.activityBox = blessed.box({
      parent: this.screen,
      top: 3,
      left: "75%",
      width: "25%",
      height: "60%",
      content: "",
      tags: true,
      border: {
        type: "line"
      },
      style: {
        fg: "white",
        bg: "black",
        border: {
          fg: "yellow"
        }
      }
    });

    // Aggregate output box
    this.outputBox = blessed.box({
      parent: this.screen,
      top: "63%",
      left: 0,
      width: "100%",
      height: 5,
      content: "",
      tags: true,
      border: {
        type: "line"
      },
      style: {
        fg: "white",
        bg: "black",
        border: {
          fg: "magenta"
        }
      }
    });

    // Controls box
    this.controlsBox = blessed.box({
      parent: this.screen,
      top: "68%",
      left: 0,
      width: "100%",
      height: 3,
      content: "{center}{bold}Controls: SPACE=play/pause, 1-8=trigger scenes, T=random trigger, Q=quit{/bold}{/center}",
      tags: true,
      border: {
        type: "line"
      },
      style: {
        fg: "white",
        bg: "black",
        border: {
          fg: "white"
        }
      }
    });

    this.screen.render();
  }

  setupKeyHandlers() {
    this.screen.key(["escape", "q", "C-c"], () => {
      this.screen.destroy();
      process.exit(0);
    });

    this.screen.key("space", () => {
      this.isPlaying = !this.isPlaying;
    });

    // Scene triggers
    for (let i = 1; i <= 8; i++) {
      this.screen.key(i.toString(), () => {
        this.triggerScene(i - 1);
      });
    }

    this.screen.key("t", () => {
      this.triggerRandomClips();
    });

    this.screen.key("r", () => {
      this.reset();
    });
  }

  updateHeader() {
    const playIcon = this.isPlaying ? "▶️" : "⏸️";
    const activeClips = this.getActiveClipCount();
    
    this.headerBox.setContent(
      `{center}{bold}🎛️  Ableton Live Session View{/bold}{/center}\n` +
      `{center}${playIcon}  Beat: ${this.beat.toFixed(2)} | Tempo: ${this.tempo} BPM | Active Clips: ${activeClips}{/center}`
    );
  }

  updateSessionGrid() {
    const maxScenes = 8;
    const maxTracksPerLine = 16;
    let content = "{bold}Session Grid:{/bold}\n";
    
    // Track headers
    let headerLine = "     ";
    for (let t = 0; t < Math.min(this.tracks.length, maxTracksPerLine); t++) {
      const track = this.tracks[t];
      const trackName = track.name.length > 8 ? track.name.substring(0, 8) : track.name;
      headerLine += trackName.padEnd(10);
    }
    content += headerLine + "\n";

    // Scene rows
    for (let scene = 0; scene < maxScenes; scene++) {
      let line = `${(scene + 1).toString().padStart(2)}  │ `;
      
      for (let t = 0; t < Math.min(this.tracks.length, maxTracksPerLine); t++) {
        const track = this.tracks[t];
        const clip = track.clips[scene];
        const activity = this.trackActivity.get(t) || 0;
        
        if (clip && clip.hasClip) {
          const intensity = Math.min(Math.floor(activity / 25), 3);
          const symbols = ["▁▁▁", "▃▃▃", "▅▅▅", "███"];
          const colors = ["gray", "yellow", "green", "red"];
          line += `{${colors[intensity]}-fg}[${symbols[intensity]}]{/${colors[intensity]}-fg} `;
        } else {
          line += "{gray-fg}[░░░]{/gray-fg} ";
        }
        
        if (Math.random() < 0.1) {
          line += "  ·  ";
        }
      }
      
      content += line + "\n";
    }

    this.gridBox.setContent(content);
  }

  updateTrackActivity() {
    let content = "{bold}Track Activity:{/bold}\n";
    
    for (let i = 0; i < this.tracks.length; i++) {
      const track = this.tracks[i];
      const activity = this.trackActivity.get(i) || 0;
      const barLength = 20;
      const filled = Math.min(Math.floor((activity / 100) * barLength), barLength);
      const empty = barLength - filled;
      
      const trackName = track.name.length > 10 ? track.name.substring(0, 10) : track.name;
      const bar = "█".repeat(filled) + "░".repeat(empty);
      const percentage = Math.floor(activity);
      
      let color = "gray";
      if (activity > 75) color = "red";
      else if (activity > 50) color = "yellow";
      else if (activity > 25) color = "green";
      
      content += `${trackName.padEnd(11)}│{${color}-fg}${bar}{/${color}-fg}│ ${percentage.toString().padStart(3)}%\n`;
    }

    this.activityBox.setContent(content);
  }

  updateAggregateOutput() {
    const barLength = 60;
    const filled = Math.min(Math.floor((this.aggregateOutput / 100) * barLength), barLength);
    const empty = barLength - filled;
    
    let color = "gray";
    if (this.aggregateOutput > 75) color = "red";
    else if (this.aggregateOutput > 50) color = "yellow";
    else if (this.aggregateOutput > 25) color = "green";
    
    const bar = "█".repeat(filled) + "░".repeat(empty);
    
    this.outputBox.setContent(
      `{bold}Aggregate Output:{/bold}\n` +
      `{${color}-fg}${bar}{/${color}-fg}\n` +
      `{center}Level: ${Math.floor(this.aggregateOutput)}%{/center}`
    );
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

  triggerRandomClips() {
    this.tracks.forEach((track, trackIndex) => {
      if (Math.random() < 0.3) {
        const activity = 30 + Math.random() * 70;
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

  reset() {
    this.trackActivity.forEach((_, index) => {
      this.trackActivity.set(index, 0);
    });
    this.aggregateOutput = 0;
    this.beat = 0;
  }

  startUpdateLoop() {
    setInterval(() => {
      if (this.isPlaying) {
        this.beat += 0.25; // Faster beat progression
      }

      // Decay activity
      this.trackActivity.forEach((activity, index) => {
        const newActivity = Math.max(0, activity - 3); // Faster decay
        this.trackActivity.set(index, newActivity);
      });

      // More frequent random activity spikes
      if (Math.random() < 0.15) { // Increased from 0.05 to 0.15
        const randomTrack = Math.floor(Math.random() * this.tracks.length);
        const currentActivity = this.trackActivity.get(randomTrack) || 0;
        const spike = Math.random() * 40; // Bigger spikes
        this.trackActivity.set(randomTrack, Math.min(100, currentActivity + spike));
      }

      // Auto-trigger scenes occasionally
      if (Math.random() < 0.08) { // Random scene triggers
        const randomScene = Math.floor(Math.random() * 8);
        this.triggerScene(randomScene);
      }

      this.updateAggregateActivity();
      this.updateHeader();
      this.updateSessionGrid();
      this.updateTrackActivity();
      this.updateAggregateOutput();
      this.screen.render();
    }, 50); // Faster updates: 50ms instead of 100ms
  }
}

// Main execution
const xmlPath = process.argv[2] || "/workspaces/aesthetic-computer/reference/extracted.xml";

if (!fs.existsSync(xmlPath)) {
  console.error(`XML file not found: ${xmlPath}`);
  console.error("Usage: node ableton-session-viewer-blessed.mjs [path-to-extracted.xml]");
  process.exit(1);
}

console.log("🎵 Parsing Ableton Live project...");
const parser = new AbletonSessionParser();
const sessionData = parser.parseFile(xmlPath);

console.log(`📊 Found ${sessionData.tracks.length} tracks`);
console.log(`🎯 Tempo: ${sessionData.tempo} BPM`);

console.log("🚀 Starting blessed session viewer...");
new BlessedSessionVisualizer(sessionData);
