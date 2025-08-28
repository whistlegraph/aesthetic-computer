// Color history for decay effect (module-level since no window object in worker)
let colorHistory = [];

// Timeline zoom smoothing (module-level)
let lastPixelsPerSecond = 60;

// Last MIDI note color for needle theming
let lastMidiNoteColor = { r: 0, g: 255, b: 255 }; // Default to cyan

// Track previous locator to detect section changes
let previousLocatorName = null;

// Timeline visibility toggle
let timelineVisible = true;

// zzzZWAP LOCATOR-SPECIFIC COLOR PALETTES
const ZZZWAP_PALETTES = {
  START: {
    name: "Start",
    colors: [
      { r: 50, g: 50, b: 50 },     // Dark Gray
      { r: 100, g: 100, b: 100 },  // Medium Gray
      { r: 150, g: 150, b: 150 },  // Light Gray
      { r: 80, g: 80, b: 120 },    // Dark Blue Gray
    ]
  },
  ACT_I: {
    name: "Act I",
    colors: [
      { r: 255, g: 100, b: 100 },  // Soft Red
      { r: 255, g: 150, b: 100 },  // Orange
      { r: 255, g: 200, b: 100 },  // Yellow Orange
      { r: 255, g: 120, b: 120 },  // Light Red
      { r: 200, g: 100, b: 100 },  // Deep Red
    ]
  },
  ACT_II: {
    name: "Act II", 
    colors: [
      { r: 100, g: 255, b: 100 },  // Bright Green
      { r: 150, g: 255, b: 120 },  // Light Green
      { r: 100, g: 200, b: 100 },  // Forest Green
      { r: 120, g: 255, b: 150 },  // Mint Green
      { r: 80, g: 180, b: 80 },    // Deep Green
    ]
  },
  ACT_III: {
    name: "Act III",
    colors: [
      { r: 100, g: 100, b: 255 },  // Bright Blue
      { r: 120, g: 150, b: 255 },  // Light Blue
      { r: 150, g: 120, b: 255 },  // Purple Blue
      { r: 100, g: 180, b: 255 },  // Sky Blue
      { r: 80, g: 80, b: 200 },    // Deep Blue
    ]
  },
  ACT_IIII: {
    name: "Act IIII",
    colors: [
      { r: 255, g: 100, b: 255 },  // Bright Magenta
      { r: 255, g: 150, b: 200 },  // Pink
      { r: 200, g: 100, b: 255 },  // Purple
      { r: 255, g: 120, b: 180 },  // Hot Pink
      { r: 180, g: 80, b: 200 },   // Deep Purple
    ]
  },
  ACT_V: {
    name: "Act V",
    colors: [
      { r: 255, g: 255, b: 100 },  // Bright Yellow
      { r: 255, g: 200, b: 150 },  // Gold
      { r: 200, g: 255, b: 100 },  // Yellow Green
      { r: 255, g: 180, b: 100 },  // Orange Yellow
      { r: 200, g: 180, b: 80 },   // Dark Gold
    ]
  },
  ACT_VI: {
    name: "Act VI",
    colors: [
      { r: 255, g: 150, b: 100 },  // Coral
      { r: 100, g: 255, b: 200 },  // Aqua
      { r: 200, g: 100, b: 255 },  // Violet
      { r: 255, g: 200, b: 150 },  // Peach
      { r: 150, g: 255, b: 100 },  // Lime
      { r: 100, g: 200, b: 255 },  // Cyan
    ]
  },
  SURPRISE: {
    name: "Surprise",
    colors: [
      { r: 255, g: 0, b: 0 },      // Pure Red
      { r: 0, g: 255, b: 0 },      // Pure Green  
      { r: 0, g: 0, b: 255 },      // Pure Blue
      { r: 255, g: 255, b: 0 },    // Pure Yellow
      { r: 255, g: 0, b: 255 },    // Pure Magenta
      { r: 0, g: 255, b: 255 },    // Pure Cyan
      { r: 255, g: 255, b: 255 },  // Pure White
    ]
  },
  PAUSE: {
    name: "Pause",
    colors: [
      { r: 80, g: 120, b: 160 },   // Muted Blue
      { r: 120, g: 100, b: 140 },  // Muted Purple
      { r: 100, g: 120, b: 100 },  // Muted Green
      { r: 140, g: 120, b: 100 },  // Muted Brown
    ]
  },
  END: {
    name: "End",
    colors: [
      { r: 200, g: 200, b: 200 },  // Light Gray
      { r: 150, g: 150, b: 150 },  // Medium Gray
      { r: 100, g: 100, b: 100 },  // Dark Gray
      { r: 50, g: 50, b: 50 },     // Very Dark Gray
    ]
  }
};

// FUNCTION TO GET PALETTE FOR CURRENT LOCATOR
function getPaletteForLocator(locatorName) {
  if (!locatorName) return ZZZWAP_PALETTES.START;
  
  const name = locatorName.toUpperCase();
  if (name.includes('ACT VI')) return ZZZWAP_PALETTES.ACT_VI;
  if (name.includes('ACT V')) return ZZZWAP_PALETTES.ACT_V;
  if (name.includes('ACT IIII')) return ZZZWAP_PALETTES.ACT_IIII;
  if (name.includes('ACT III')) return ZZZWAP_PALETTES.ACT_III;
  if (name.includes('ACT II')) return ZZZWAP_PALETTES.ACT_II;
  if (name.includes('ACT I')) return ZZZWAP_PALETTES.ACT_I;
  if (name.includes('SURPRISE')) return ZZZWAP_PALETTES.SURPRISE;
  if (name.includes('PAUSE')) return ZZZWAP_PALETTES.PAUSE;
  if (name.includes('START')) return ZZZWAP_PALETTES.START;
  if (name.includes('END')) return ZZZWAP_PALETTES.END;
  
  return ZZZWAP_PALETTES.START; // Default fallback
}

// Visualizer - zzzZWAP Project Visualizer, 2025.08.18
// Loads specific Ableton project and audio files over network
// Shows only current and next locator during playback

// ALS Project Parser Class - Enhanced with reference tools knowledge
class ALSProject {
  constructor(xmlData) {
    this.rawData = xmlData;
    this.tracks = [];
    this.scenes = [];
    this.locators = [];
    this.clips = [];
    this.devices = [];
    this.warpMarkers = [];
    this.tempo = 120;
    this.timeSignature = { numerator: 4, denominator: 4 };
    this.sampleRate = 44100;
    this.projectDuration = 0;
    
    this.parseXML(xmlData);
  }
  
  parseXML(xmlData) {
    this.parseGlobalSettings(xmlData);
    this.parseTempo(xmlData);
    this.parseTempoChanges(xmlData); // Add tempo changes parsing
    this.parseTracks(xmlData);
    this.parseScenes(xmlData);
    this.parseLocators(xmlData);
    this.parseWarpMarkers(xmlData);
    this.parseClips(xmlData);
    this.parseDevices(xmlData);
    this.projectDuration = this.calculateProjectDuration();
  }
  
  parseGlobalSettings(xmlData) {
    const sampleRateMatch = xmlData.match(/<SampleRate Value="(\d+)"/);
    if (sampleRateMatch) {
      this.sampleRate = parseInt(sampleRateMatch[1]);
    }
  }
  
  parseTempoChanges(xmlData) {
    console.log("Parsing tempo changes...");
    this.tempoChanges = [];
    
    // Look for tempo change events in the arrangement
    const tempoChangeRegex = /<FloatEvent[^>]*Time="([^"]*)"[^>]*Value="([^"]*)"[^>]*\/>/g;
    let match;
    
    while ((match = tempoChangeRegex.exec(xmlData)) !== null) {
      const time = parseFloat(match[1]);
      const tempo = parseFloat(match[2]);
      
      if (!isNaN(time) && !isNaN(tempo)) {
        this.tempoChanges.push({
          time: time,
          beat: this.alsTimeToBeat(time),
          tempo: tempo
        });
      }
    }
    
    // Sort tempo changes by time
    this.tempoChanges.sort((a, b) => a.time - b.time);
    
    console.log(`Found ${this.tempoChanges.length} tempo changes:`, this.tempoChanges);
    
    // If no tempo changes found, create one at the beginning with the main tempo
    if (this.tempoChanges.length === 0) {
      this.tempoChanges.push({
        time: 0,
        beat: 0,
        tempo: this.tempo
      });
    }
  }
  
  // Get the tempo at a specific beat position
  getTempoAtBeat(beat) {
    let currentTempo = this.tempo;
    
    for (const change of this.tempoChanges) {
      if (change.beat <= beat) {
        currentTempo = change.tempo;
      } else {
        break;
      }
    }
    
    return currentTempo;
  }
  
  // Convert beats to seconds accounting for tempo changes
  beatsToSecondsWithTempoMap(beats) {
    if (this.tempoChanges.length <= 1) {
      // No tempo changes, use simple calculation
      return (beats * 60) / this.tempo;
    }
    
    let seconds = 0;
    let currentBeat = 0;
    
    for (let i = 0; i < this.tempoChanges.length; i++) {
      const change = this.tempoChanges[i];
      const nextChange = this.tempoChanges[i + 1];
      
      const segmentStart = Math.max(currentBeat, change.beat);
      const segmentEnd = nextChange ? Math.min(nextChange.beat, beats) : beats;
      
      if (segmentEnd <= segmentStart) continue;
      
      const segmentBeats = segmentEnd - segmentStart;
      const segmentSeconds = (segmentBeats * 60) / change.tempo;
      seconds += segmentSeconds;
      
      currentBeat = segmentEnd;
      
      if (currentBeat >= beats) break;
    }
    
    return seconds;
  }
  
  parseTempo(xmlData) {
    // Try multiple tempo patterns
    let tempoMatch = xmlData.match(/<Tempo>[\s\S]*?<Manual Value="(\d+\.?\d*)"[\s\S]*?<\/Tempo>/);
    if (!tempoMatch) {
      tempoMatch = xmlData.match(/<Tempo Value="(\d+\.?\d*)"/);
    }
    if (!tempoMatch) {
      tempoMatch = xmlData.match(/<Manual Value="(\d+\.?\d*)"/);
    }
    
    if (tempoMatch) {
      this.tempo = parseFloat(tempoMatch[1]);
      console.log("Parsed tempo:", this.tempo, "BPM");
    } else {
      console.log("No tempo found, using default 120 BPM");
      console.log("Tempo search patterns failed in XML. First 1000 chars:", xmlData.substring(0, 1000));
    }
    
    const timeSignatureMatch = xmlData.match(/<TimeSignature>\s*<Numerator Value="(\d+)"\s*\/>\s*<Denominator Value="(\d+)"\s*\/>\s*<\/TimeSignature>/);
    if (timeSignatureMatch) {
      this.timeSignature = {
        numerator: parseInt(timeSignatureMatch[1]),
        denominator: parseInt(timeSignatureMatch[2])
      };
      console.log("Parsed time signature:", this.timeSignature);
    }
  }
  
  parseTempoChanges(xmlData) {
    console.log("Parsing tempo changes...");
    this.tempoChanges = [];
    
    // For now, just create a single tempo change at the beginning
    // More complex tempo change parsing can be added later
    this.tempoChanges.push({
      time: 0,
      beat: 0,
      tempo: this.tempo
    });
    
    console.log(`Using single tempo: ${this.tempo} BPM`);
  }
  
  parseTracks(xmlData) {
    console.log("Starting enhanced track parsing...");
    
    const trackRegex = /<(MidiTrack|AudioTrack|ReturnTrack|MasterTrack|GroupTrack)[\s\S]*?<\/\1>/g;
    let match;
    let trackIndex = 0;
    
    while ((match = trackRegex.exec(xmlData)) !== null) {
      const trackContent = match[0];
      const trackType = match[1];
      
      const track = {
        index: trackIndex,
        type: trackType,
        name: this.extractTrackName(trackContent) || `${trackType} ${trackIndex + 1}`,
        color: this.extractTrackColor(trackContent),
        muted: this.extractTrackMuted(trackContent),
        solo: this.extractTrackSolo(trackContent),
        clips: []
      };
      
      console.log(`Track ${trackIndex}: "${track.name}" (${track.type})`);
      this.tracks.push(track);
      trackIndex++;
    }
    
    console.log(`Parsed ${this.tracks.length} tracks with names`);
  }
  
  parseScenes(xmlData) {
    const sceneRegex = /<Scene[\s\S]*?<\/Scene>/g;
    let match;
    
    while ((match = sceneRegex.exec(xmlData)) !== null) {
      const sceneContent = match[0];
      
      const scene = {
        name: this.extractSceneName(sceneContent) || `Scene ${this.scenes.length + 1}`,
        tempo: this.extractSceneTempo(sceneContent),
        timeSignature: this.extractSceneTimeSignature(sceneContent)
      };
      
      this.scenes.push(scene);
    }
  }
  
  // Parse locators (crucial for song structure visualization)
  parseLocators(xmlData) {
    console.log("Starting locator parsing...");
    
    // First, let's examine the locators section more carefully
    const locatorsSection = xmlData.match(/<Locators>[\s\S]*?<\/Locators>/);
    if (locatorsSection) {
      console.log("Found Locators section, length:", locatorsSection[0].length);
      console.log("Locators section preview:", locatorsSection[0].substring(0, 500));
    }
    
    // Parse individual locator elements with their full content
    const locatorRegex = /<Locator[^>]*>[\s\S]*?<\/Locator>/g;
    let match;
    
    while ((match = locatorRegex.exec(xmlData)) !== null) {
      const locatorContent = match[0];
      console.log("Processing full locator:", locatorContent.substring(0, 300));
      
      // Extract ID from the opening tag
      const idMatch = locatorContent.match(/<Locator[^>]*Id="([^"]*)"[^>]*>/);
      const locatorId = idMatch ? idMatch[1] : this.locators.length.toString();
      
      // Look for Time child element
      const timeMatch = locatorContent.match(/<Time[^>]*Value="([^"]*)"[^>]*\/>/);
      
      // Look for Name child element  
      const nameMatch = locatorContent.match(/<Name[^>]*Value="([^"]*)"[^>]*\/>/);
      
      if (timeMatch) {
        const timeValue = parseFloat(timeMatch[1]);
        
        // Debug raw locator timing
        console.log(`Converting locator timing for "${nameMatch ? nameMatch[1] : 'Unnamed'}": ${timeValue}`);
        
        // Use smart conversion for locators too
        const locator = {
          id: locatorId,
          time: timeValue,
          beat: timeValue, // Keep original for reference
          name: nameMatch ? nameMatch[1] : `Locator ${locatorId}`,
          seconds: this.convertALSTimeToSeconds(timeValue, `locator "${nameMatch ? nameMatch[1] : locatorId}"`)
        };
        
        console.log(`Locator ${locatorId}: ${locator.name} at ${timeValue} = ${locator.seconds.toFixed(2)}s`);
        this.locators.push(locator);
      } else {
        console.log("No time found in locator:", locatorId);
      }
    }
    
    console.log(`Total locators parsed: ${this.locators.length}`);
    
    // Sort locators by time
    this.locators.sort((a, b) => a.time - b.time);
    
    // If still no locators found, create some test locators
    if (this.locators.length === 0) {
      console.log("No locators found, creating test locators...");
      this.locators = [
        { id: "0", time: 0, beat: 0, name: "START", seconds: 0 },
        { id: "1", time: 480, beat: 480, name: "Verse", seconds: this.beatsToSeconds(480) },
        { id: "2", time: 960, beat: 960, name: "Chorus", seconds: this.beatsToSeconds(960) },
        { id: "3", time: 1440, beat: 1440, name: "Bridge", seconds: this.beatsToSeconds(1440) },
        { id: "4", time: 1920, beat: 1920, name: "End", seconds: this.beatsToSeconds(1920) }
      ];
      console.log("Created test locators:", this.locators);
    }
  }
  
  // Parse warp markers (audio time manipulation)
  parseWarpMarkers(xmlData) {
    const warpRegex = /<WarpMarker[\s\S]*?\/>/g;
    let match;
    
    while ((match = warpRegex.exec(xmlData)) !== null) {
      const warpContent = match[0];
      
      const timeMatch = warpContent.match(/SecTime="(\d+\.?\d*)"/);
      const beatTimeMatch = warpContent.match(/BeatTime="(\d+\.?\d*)"/);
      
      if (timeMatch && beatTimeMatch) {
        const warpMarker = {
          secTime: parseFloat(timeMatch[1]),
          beatTime: parseFloat(beatTimeMatch[1])
        };
        
        this.warpMarkers.push(warpMarker);
      }
    }
  }
  
  parseClips(xmlData) {
    this.parseArrangementClips(xmlData);
    this.parseMIDIClips(xmlData);
    this.parseAudioClips(xmlData);
    
    // Enhanced clip parsing for timeline visualization
    this.parseEnhancedClips(xmlData);
  }
  
  parseArrangementClips(xmlData) {
    const clipRegex = /<ClipSlot[\s\S]*?<\/ClipSlot>/g;
    let match;
    
    while ((match = clipRegex.exec(xmlData)) !== null) {
      const clipContent = match[0];
      
      if (clipContent.includes('<MidiClip') || clipContent.includes('<AudioClip')) {
        const clip = {
          type: clipContent.includes('<MidiClip') ? 'midi' : 'audio',
          time: this.extractClipTime(clipContent),
          duration: this.extractClipDuration(clipContent),
          name: this.extractClipName(clipContent),
          trackIndex: this.findTrackForClip(clipContent)
        };
        
        this.clips.push(clip);
      }
    }
  }
  
  // Enhanced clip parsing for better visualization
  parseEnhancedClips(xmlData) {
    console.log("Parsing enhanced clips for visualization...");
    
    // Parse TakeLanes which contain arrangement clips
    const takeLaneRegex = /<TakeLane[\s\S]*?<\/TakeLane>/g;
    let takeLaneMatch;
    let trackIndex = 0;
    
    while ((takeLaneMatch = takeLaneRegex.exec(xmlData)) !== null) {
      const takeLaneContent = takeLaneMatch[0];
      
      // Find clips within this take lane
      const clipRegex = /<(MidiClip|AudioClip)[\s\S]*?<\/\1>/g;
      let clipMatch;
      
      while ((clipMatch = clipRegex.exec(takeLaneContent)) !== null) {
        const clipContent = clipMatch[0];
        const clipType = clipMatch[1];
        
        // Extract detailed clip information
        const clip = {
          id: this.extractClipId(clipContent),
          type: clipType.toLowerCase(),
          trackIndex: trackIndex,
          trackId: this.findTrackIdForClip(clipContent),
          currentStart: this.extractClipCurrentStart(clipContent),
          currentEnd: this.extractClipCurrentEnd(clipContent),
          loopStart: this.extractClipLoopStart(clipContent),
          loopEnd: this.extractClipLoopEnd(clipContent),
          startRelative: this.extractClipStartRelative(clipContent),
          time: this.extractClipTime(clipContent),
          name: this.extractClipName(clipContent) || `${clipType} Clip`,
          color: this.extractClipColor(clipContent),
          notes: clipType === 'MidiClip' ? this.extractMIDINotes(clipContent) : [],
          isLooping: this.extractClipIsLooping(clipContent),
          isActive: false, // Will be calculated based on current time
          velocity: clipType === 'MidiClip' ? this.extractClipVelocity(clipContent) : null
        };
        
        // Convert timing to seconds for visualization using smart conversion
        const rawStart = clip.currentStart || clip.time || 0;
        const rawEnd = clip.currentEnd || (clip.currentStart + (clip.loopEnd - clip.loopStart)) || 0;
        
        console.log(`Converting timing for clip "${clip.name}": rawStart=${rawStart}, rawEnd=${rawEnd}`);
        
        clip.startSeconds = this.convertALSTimeToSeconds(rawStart, `clip "${clip.name}" start`);
        clip.endSeconds = this.convertALSTimeToSeconds(rawEnd, `clip "${clip.name}" end`);
        clip.duration = clip.endSeconds - clip.startSeconds;
        
        this.clips.push(clip);
        console.log(`Enhanced clip: ${clip.name} on track ${clip.trackIndex}, ${clip.startSeconds.toFixed(2)}s - ${clip.endSeconds.toFixed(2)}s`);
      }
      
      trackIndex++;
    }
    
    console.log(`Parsed ${this.clips.length} enhanced clips across ${trackIndex} tracks`);
  }

  parseMIDIClips(xmlData) {
    const clipRegex = /<MidiClip[\s\S]*?<\/MidiClip>/g;
    let match;
    
    while ((match = clipRegex.exec(xmlData)) !== null) {
      const clipContent = match[0];
      
      const clip = {
        type: 'midi',
        time: this.extractClipTime(clipContent),
        duration: this.extractClipDuration(clipContent),
        name: this.extractClipName(clipContent),
        notes: this.extractMIDINotes(clipContent),
        trackIndex: this.findTrackForClip(clipContent)
      };
      
      this.clips.push(clip);
    }
  }
  
  parseAudioClips(xmlData) {
    const clipRegex = /<AudioClip[\s\S]*?<\/AudioClip>/g;
    let match;
    
    while ((match = clipRegex.exec(xmlData)) !== null) {
      const clipContent = match[0];
      
      const clip = {
        type: 'audio',
        time: this.extractClipTime(clipContent),
        duration: this.extractClipDuration(clipContent),
        name: this.extractClipName(clipContent),
        sampleRef: this.extractSampleRef(clipContent),
        warpMarkers: this.extractWarpMarkers(clipContent),
        trackIndex: this.findTrackForClip(clipContent)
      };
      
      this.clips.push(clip);
    }
  }
  
  parseDevices(xmlData) {
    const deviceRegex = /<(PluginDevice|InstrumentRack|Operator|Simpler|Impulse)[\s\S]*?<\/\1>/g;
    let match;
    
    while ((match = deviceRegex.exec(xmlData)) !== null) {
      const deviceContent = match[0];
      const deviceType = match[1];
      
      const device = {
        type: deviceType,
        enabled: this.extractDeviceEnabled(deviceContent),
        parameters: this.extractDeviceParameters(deviceContent)
      };
      
      this.devices.push(device);
    }
  }
  
  // Helper extraction methods
  extractTrackName(content) {
    const nameMatch = content.match(/<EffectiveName Value="([^"]*)"/);
    return nameMatch ? nameMatch[1] : null;
  }
  
  extractTrackColor(content) {
    const colorMatch = content.match(/<ColorIndex Value="(\d+)"/);
    return colorMatch ? parseInt(colorMatch[1]) : 0;
  }
  
  extractTrackMuted(content) {
    const mutedMatch = content.match(/<TrackIsMuted Value="(true|false)"/);
    return mutedMatch ? mutedMatch[1] === 'true' : false;
  }
  
  extractTrackSolo(content) {
    const soloMatch = content.match(/<TrackIsSoloed Value="(true|false)"/);
    return soloMatch ? soloMatch[1] === 'true' : false;
  }
  
  // Enhanced clip extraction methods
  extractClipId(content) {
    const idMatch = content.match(/<(MidiClip|AudioClip)[^>]*Id="([^"]*)"/);
    return idMatch ? idMatch[2] : null;
  }
  
  extractClipCurrentStart(content) {
    const startMatch = content.match(/<CurrentStart Value="([^"]*)"/);
    return startMatch ? parseFloat(startMatch[1]) : 0;
  }
  
  extractClipCurrentEnd(content) {
    const endMatch = content.match(/<CurrentEnd Value="([^"]*)"/);
    return endMatch ? parseFloat(endMatch[1]) : 0;
  }
  
  extractClipLoopStart(content) {
    const loopStartMatch = content.match(/<LoopStart Value="([^"]*)"/);
    return loopStartMatch ? parseFloat(loopStartMatch[1]) : 0;
  }
  
  extractClipLoopEnd(content) {
    const loopEndMatch = content.match(/<LoopEnd Value="([^"]*)"/);
    return loopEndMatch ? parseFloat(loopEndMatch[1]) : 0;
  }
  
  extractClipStartRelative(content) {
    const startRelMatch = content.match(/<StartRelative Value="([^"]*)"/);
    return startRelMatch ? parseFloat(startRelMatch[1]) : 0;
  }
  
  extractClipTime(content) {
    const timeMatch = content.match(/<CurrentStart Value="(\d+\.?\d*)"/);
    return timeMatch ? parseFloat(timeMatch[1]) : 0;
  }
  
  extractClipDuration(content) {
    const durationMatch = content.match(/<Length Value="(\d+\.?\d*)"/);
    return durationMatch ? parseFloat(durationMatch[1]) : 0;
  }
  
  extractClipName(content) {
    const nameMatch = content.match(/<Name Value="([^"]*)"/);
    return nameMatch ? nameMatch[1] : null;
  }
  
  extractClipColor(content) {
    const colorMatch = content.match(/<Color Value="(\d+)"/);
    return colorMatch ? parseInt(colorMatch[1]) : Math.floor(Math.random() * 64);
  }
  
  extractClipIsLooping(content) {
    const loopMatch = content.match(/<IsLooping Value="(true|false)"/);
    return loopMatch ? loopMatch[1] === 'true' : true;
  }
  
  extractClipVelocity(content) {
    // Extract average velocity from MIDI notes
    const notes = this.extractMIDINotes(content);
    if (notes.length > 0) {
      const avgVelocity = notes.reduce((sum, note) => sum + note.velocity, 0) / notes.length;
      return Math.round(avgVelocity);
    }
    return 100;
  }
  
  findTrackIdForClip(content) {
    // This would need more sophisticated logic based on XML structure
    return null;
  }
  
  extractMIDINotes(clipContent) {
    const notes = [];
    const keyTrackRegex = /<KeyTrack[\s\S]*?<\/KeyTrack>/g;
    let keyTrackMatch;
    
    while ((keyTrackMatch = keyTrackRegex.exec(clipContent)) !== null) {
      const keyTrackContent = keyTrackMatch[0];
      const noteRegex = /<MidiNoteEvent[\s\S]*?\/>/g;
      let noteMatch;
      
      while ((noteMatch = noteRegex.exec(keyTrackContent)) !== null) {
        const noteElement = noteMatch[0];
        
        const timeMatch = noteElement.match(/Time="(\d+\.?\d*)"/);
        const durationMatch = noteElement.match(/Duration="(\d+\.?\d*)"/);
        const velocityMatch = noteElement.match(/Velocity="(\d+)"/);
        
        if (timeMatch && durationMatch) {
          const note = {
            time: parseFloat(timeMatch[1]),
            duration: parseFloat(durationMatch[1]),
            velocity: velocityMatch ? parseInt(velocityMatch[1]) : 100,
            pitch: this.extractNotePitch(noteElement)
          };
          
          notes.push(note);
        }
      }
    }
    
    return notes;
  }
  
  extractNotePitch(noteElement) {
    const keyMatch = noteElement.match(/Key="(\d+)"/);
    return keyMatch ? parseInt(keyMatch[1]) : 60; // Default to middle C
  }
  
  findArrangementTrackForClip(takeLaneContent) {
    // This would need more complex logic to match clips to tracks
    return 0;
  }
  
  extractLoopData(content) {
    const loopMatch = content.match(/<Loop[\s\S]*?<\/Loop>/);
    if (loopMatch) {
      const startMatch = loopMatch[0].match(/<StartRelative Value="(\d+\.?\d*)"/);
      const endMatch = loopMatch[0].match(/<EndRelative Value="(\d+\.?\d*)"/);
      
      return {
        start: startMatch ? parseFloat(startMatch[1]) : 0,
        end: endMatch ? parseFloat(endMatch[1]) : 0
      };
    }
    return null;
  }
  
  extractSampleRef(content) {
    const sampleRefMatch = content.match(/<SampleRef[\s\S]*?<\/SampleRef>/);
    if (sampleRefMatch) {
      const fileRefMatch = sampleRefMatch[0].match(/<FileRef[\s\S]*?<\/FileRef>/);
      if (fileRefMatch) {
        const pathMatch = fileRefMatch[0].match(/<Path Value="([^"]*)"/);
        return pathMatch ? pathMatch[1] : null;
      }
    }
    return null;
  }
  
  extractWarpMarkers(content) {
    const markers = [];
    const markerRegex = /<WarpMarker[\s\S]*?\/>/g;
    let match;
    
    while ((match = markerRegex.exec(content)) !== null) {
      const markerElement = match[0];
      const secTimeMatch = markerElement.match(/SecTime="(\d+\.?\d*)"/);
      const beatTimeMatch = markerElement.match(/BeatTime="(\d+\.?\d*)"/);
      
      if (secTimeMatch && beatTimeMatch) {
        markers.push({
          secTime: parseFloat(secTimeMatch[1]),
          beatTime: parseFloat(beatTimeMatch[1])
        });
      }
    }
    
    return markers;
  }
  
  extractValueFromContent(content, tagName) {
    const regex = new RegExp(`<${tagName} Value="([^"]*)"`, 'i');
    const match = content.match(regex);
    return match ? match[1] : null;
  }
  
  extractDeviceEnabled(content) {
    const enabledMatch = content.match(/<On Value="(true|false)"/);
    return enabledMatch ? enabledMatch[1] === 'true' : true;
  }
  
  extractDeviceParameters(content) {
    const parameters = [];
    const paramRegex = /<Parameter[\s\S]*?<\/Parameter>/g;
    let match;
    
    while ((match = paramRegex.exec(content)) !== null) {
      const paramContent = match[0];
      const nameMatch = paramContent.match(/<Name Value="([^"]*)"/);
      const valueMatch = paramContent.match(/<UserValue Value="(\d+\.?\d*)"/);
      
      if (nameMatch && valueMatch) {
        parameters.push({
          name: nameMatch[1],
          value: parseFloat(valueMatch[1])
        });
      }
    }
    
    return parameters;
  }
  
  findArrangementTrackForClip(clipContent) {
    // Simplified - would need more complex logic in real implementation
    return Math.floor(Math.random() * this.tracks.length);
  }
  
  findTrackForClip(clipContent) {
    // Simplified - would need more complex logic in real implementation
    return Math.floor(Math.random() * this.tracks.length);
  }
  
  extractSceneName(content) {
    const nameMatch = content.match(/<Name Value="([^"]*)"/);
    return nameMatch ? nameMatch[1] : null;
  }
  
  extractSceneTempo(content) {
    const tempoMatch = content.match(/<Tempo Value="(\d+\.?\d*)"/);
    return tempoMatch ? parseFloat(tempoMatch[1]) : null;
  }
  
  extractSceneTimeSignature(content) {
    const timeSignatureMatch = content.match(/<TimeSignature>\s*<Numerator Value="(\d+)"\s*\/>\s*<Denominator Value="(\d+)"\s*\/>\s*<\/TimeSignature>/);
    if (timeSignatureMatch) {
      return {
        numerator: parseInt(timeSignatureMatch[1]),
        denominator: parseInt(timeSignatureMatch[2])
      };
    }
    return null;
  }
  
  getTrackColor(type) {
    const colors = {
      'MidiTrack': [100, 149, 237],
      'AudioTrack': [34, 139, 34],
      'ReturnTrack': [255, 140, 0],
      'MasterTrack': [220, 20, 60]
    };
    return colors[type] || [128, 128, 128];
  }
  
  // Calculate current beat position from audio time
  getCurrentBeat(audioTime) {
    return (audioTime / 60) * this.tempo;
  }
  
  // Calculate the actual project duration from clips and locators
  calculateProjectDuration() {
    let maxDuration = 0;
    
    // Check clips
    for (const clip of this.clips) {
      const clipEnd = clip.time + clip.duration;
      if (clipEnd > maxDuration) {
        maxDuration = clipEnd;
      }
    }
    
    // Check locators
    for (const locator of this.locators) {
      if (locator.time > maxDuration) {
        maxDuration = locator.time;
      }
    }
    
    return maxDuration;
  }
  
  // Convert ALS time units to beats (improved conversion)
  alsTimeToBeat(alsTime) {
    // In Ableton Live, different time values use different units:
    // - Locator Time values are typically in beats (quarter notes)  
    // - Clip CurrentStart/CurrentEnd are often in seconds or sample ticks
    // We need to normalize these. For now, try direct beats assumption.
    return alsTime;
  }

  // Convert beats to actual time in seconds
  beatsToSeconds(beats) {
    // Correct formula: seconds = (beats * 60) / BPM
    return (beats * 60) / this.tempo;
  }

  // Smart time conversion that handles different ALS time units
  convertALSTimeToSeconds(rawTime, context = 'clip') {
    if (rawTime === undefined || rawTime === null) return 0;
    
    // Apply different conversion strategies based on the magnitude of the value
    if (rawTime >= 10000) {
      // Very large values (>= 10000) are likely in ticks (480 ticks per beat in Live)
      const beats = rawTime / 480;
      const seconds = this.beatsToSeconds(beats);
      console.log(`  ${context}: ${rawTime} ticks → ${beats.toFixed(2)} beats → ${seconds.toFixed(2)} seconds`);
      return seconds;
    } else if (rawTime >= 500) {
      // Medium values (500-9999) might be in seconds already or high-resolution beats
      // Check if treating as seconds gives reasonable values
      if (rawTime < 1000) {
        // 500-1000 range, could be seconds
        console.log(`  ${context}: ${rawTime} assumed as seconds`);
        return rawTime;
      } else {
        // > 1000, likely high-res beats or ticks
        const beats = rawTime / 480; // Try tick conversion
        const seconds = this.beatsToSeconds(beats);
        console.log(`  ${context}: ${rawTime} treated as ticks → ${beats.toFixed(2)} beats → ${seconds.toFixed(2)} seconds`);
        return seconds;
      }
    } else {
      // Small values (< 500) are likely beats
      const seconds = this.beatsToSeconds(rawTime);
      console.log(`  ${context}: ${rawTime} beats → ${seconds.toFixed(2)} seconds`);
      return seconds;
    }
  }  // Get clips that are active at the current time
  getActiveClips(currentTimeSeconds) {
    return this.clips.filter(clip => {
      if (!clip.startSeconds || !clip.endSeconds) return false;
      
      // Check if current time is within clip bounds
      const isWithinTime = currentTimeSeconds >= clip.startSeconds && currentTimeSeconds <= clip.endSeconds;
      
      // For looping clips, check if it's within the loop boundaries
      if (clip.isLooping && isWithinTime) {
        const clipProgress = (currentTimeSeconds - clip.startSeconds) / (clip.endSeconds - clip.startSeconds);
        const loopDuration = clip.loopEnd - clip.loopStart;
        const clipDuration = clip.endSeconds - clip.startSeconds;
        
        if (loopDuration > 0 && clipDuration > 0) {
          // Calculate position within the loop
          const loopProgress = (clipProgress * clipDuration) % loopDuration;
          return loopProgress >= 0 && loopProgress <= loopDuration;
        }
      }
      
      return isWithinTime;
    });
  }
  
  // Get MIDI notes that should be playing at the current time (SIMPLIFIED)
  getActiveNotes(currentTimeSeconds) {
    const activeNotes = [];
    
    this.clips.forEach(clip => {
      if (clip.type !== 'midiclip' || !clip.notes) return;
      
      // Skip clips with weird timing
      if (!clip.startSeconds && clip.startSeconds !== 0) return;
      
      clip.notes.forEach(note => {
        const noteStartTime = clip.startSeconds + this.beatsToSeconds(this.alsTimeToBeat(note.time || 0));
        const noteEndTime = noteStartTime + this.beatsToSeconds(this.alsTimeToBeat(note.duration || 0.25));
        
        // Check if note is currently playing
        if (currentTimeSeconds >= noteStartTime && currentTimeSeconds <= noteEndTime) {
          activeNotes.push({
            ...note,
            clipName: clip.name,
            trackIndex: clip.trackIndex,
            globalStartTime: noteStartTime,
            globalEndTime: noteEndTime,
            timeInNote: currentTimeSeconds - noteStartTime
          });
        }
      });
    });
    
    return activeNotes;
  }
  
  // Get active clips at a specific beat position
  getActiveClipsAtBeat(beatPosition) {
    return this.clips.filter(clip => {
      const clipStart = this.alsTimeToBeat(clip.time);
      const clipEnd = clipStart + this.alsTimeToBeat(clip.duration);
      return beatPosition >= clipStart && beatPosition < clipEnd;
    });
  }
  
  // Get active MIDI notes at a specific beat position
  getActiveNotesAtBeat(beatPosition) {
    const activeNotes = [];
    
    for (const clip of this.clips) {
      if (clip.type === 'midi' && clip.notes) {
        const clipStart = this.alsTimeToBeat(clip.time);
        
        for (const note of clip.notes) {
          const noteStart = clipStart + this.alsTimeToBeat(note.time);
          const noteEnd = noteStart + this.alsTimeToBeat(note.duration);
          
          if (beatPosition >= noteStart && beatPosition < noteEnd) {
            activeNotes.push({
              ...note,
              absoluteStart: noteStart,
              absoluteEnd: noteEnd,
              clipIndex: this.clips.indexOf(clip)
            });
          }
        }
      }
    }
    
    return activeNotes;
  }
  
  // Get current and next locator at a specific time position
  getCurrentLocator(currentTimeSeconds, actualDuration) {
    let currentLocator = null;
    let nextLocator = null;
    let currentIndex = -1;
    
    // Use locator times directly - no scaling needed since audio is at correct 143 BPM
    
    for (let i = 0; i < this.locators.length; i++) {
      const locator = this.locators[i];
      
      if (locator.seconds <= currentTimeSeconds) {
        currentLocator = locator;
        currentIndex = i;
        
        // Log when we pass a locator (only once per locator)
        if (!passedLocators.has(i)) {
          passedLocators.add(i);
          const timeDiff = currentTimeSeconds - locator.seconds;
          
          // Calculate what this timing should be based on pure audio progress
          const unscaledAudioTime = currentAudioProgress * actualDuration;
          const beatsBasedOnAudio = (unscaledAudioTime * this.tempo) / 60;
          
          // Calculate the beat offset between project and audio
          const beatOffset = locator.time - beatsBasedOnAudio;
          
          // console.log(`🎯 LOCATOR PASSED: "${locator.name}" 
          //   Expected: ${locator.seconds.toFixed(2)}s (no scaling)
          //   Actual: ${currentTimeSeconds.toFixed(2)}s
          //   Audio Time: ${unscaledAudioTime.toFixed(2)}s
          //   Audio Beats: ${beatsBasedOnAudio.toFixed(2)} vs Project Beats: ${locator.time.toFixed(2)}
          //   Beat Offset: ${beatOffset.toFixed(2)} beats (${(beatOffset * 60 / this.tempo).toFixed(2)}s)
          //   Drift: ${timeDiff >= 0 ? '+' : ''}${timeDiff.toFixed(2)}s 
          //   Beat: ${locator.time.toFixed(2)} @ ${this.tempo}BPM`);
        }
      } else if (!nextLocator && currentTimeSeconds < locator.seconds) {
        nextLocator = locator;
        break;
      }
    }
    
    return { current: currentLocator, next: nextLocator, currentIndex };
  }
}

// State
let alsProject = null;
let wavFile = null;
let preloadedAudio = null;
let isPlaying = false;
let playingSfx = null;
let playStartTime = 0;
let audioStartTime = 0;
let message = "Loading zzzZWAP project...";
let netAPI = null;
let progress = 0;
let actualDuration = null;
let passedLocators = new Set(); // Track which locators we've passed
let currentAudioProgress = null; // Store the actual audio progress from speaker worklet
let timelineOffset = 2.0; // Look ahead by this many seconds (adjustable with +/-)

// Boot function to load files over network
export const boot = async ({ net }) => {
  netAPI = net;
  
  try {
    // Load the .als file
    console.log("Loading zzzZWAP.als...");
    const alsResponse = await fetch("https://assets.aesthetic.computer/wipppps/zzzZWAP.als");
    const alsArrayBuffer = await alsResponse.arrayBuffer();
    
    // Try to decompress using native browser decompression (if supported)
    let alsText;
    try {
      console.log("Attempting to decompress ALS file using DecompressionStream...");
      const stream = new Response(alsArrayBuffer).body;
      const decompressedStream = stream.pipeThrough(new DecompressionStream('gzip'));
      const decompressedResponse = new Response(decompressedStream);
      alsText = await decompressedResponse.text();
      console.log("Successfully decompressed ALS file");
    } catch (decompressError) {
      console.log("Native decompression failed, trying as plain text:", decompressError);
      // Fallback: try as plain text
      alsText = new TextDecoder().decode(alsArrayBuffer);
    }
    
    // Parse the ALS project
    alsProject = new ALSProject(alsText);
    console.log("ALS project loaded:", alsProject);
    console.log("Locators found:", alsProject.locators.length);
    console.log("First 1000 chars of ALS file:", alsText.substring(0, 1000));
    
    // Load the audio file
    console.log("Loading zzzZWAP.wav...");
    preloadedAudio = await net.preload("https://assets.aesthetic.computer/wipppps/zzzZWAP.wav");
    
    // Try to get duration immediately from the preloaded audio metadata
    if (preloadedAudio && preloadedAudio.buffer) {
      actualDuration = preloadedAudio.buffer.duration;
      console.log("Got duration from preloaded audio:", actualDuration);
    }
    
    message = "Ready! Tap to play";
    console.log("All files loaded successfully!");
    
  } catch (error) {
    console.error("Error loading files:", error);
    message = "Error loading files";
  }
};

// Initialize
console.log("🎵 VISUALIZER.MJS: zzzZWAP visualizer loaded and ready!");

// TV bars painting buffer - module-level to persist between frames
let tvBarsBuffer = null;
let lastScreenWidth = 0;
let lastScreenHeight = 0;

function paint({ wipe, ink, screen, sound, paintCount, clock, write, box, line, typeface, painting, page, paste, blur, zoom, scroll, poly }) {
  // Initialize or recreate TV bars buffer if needed (first time or screen size changed)
  if (!tvBarsBuffer || screen.width !== lastScreenWidth || screen.height !== lastScreenHeight) {
    tvBarsBuffer = painting(screen.width, screen.height, (api) => {
      // Initialize with transparent background
      api.wipe(0, 0, 0, 0);
    });
    lastScreenWidth = screen.width;
    lastScreenHeight = screen.height;
  }

  // Global TV color tracking
  let globalCurrentColor = null;
  
  // Function to generate unique color for each locator
  function getLocatorColor(locator, index) {
    // Add null safety check
    if (!locator || !locator.name) {
      // Return a default color if locator is invalid
      return { r: 100, g: 100, b: 100 };
    }
    
    // Use locator name hash + index for unique colors
    const nameHash = locator.name.split('').reduce((hash, char) => hash + char.charCodeAt(0), 0);
    const hue = ((nameHash * 37) + (index * 60)) % 360; // Spread colors across spectrum
    const saturation = 0.6; // Softer saturation for ambient colors
    const lightness = 0.4; // Medium brightness
    
    // Convert HSL to RGB
    const c = (1 - Math.abs(2 * lightness - 1)) * saturation;
    const x = c * (1 - Math.abs(((hue / 60) % 2) - 1));
    const m = lightness - c / 2;
    
    let r, g, b;
    if (hue < 60) { r = c; g = x; b = 0; }
    else if (hue < 120) { r = x; g = c; b = 0; }
    else if (hue < 180) { r = 0; g = c; b = x; }
    else if (hue < 240) { r = 0; g = x; b = c; }
    else if (hue < 300) { r = x; g = 0; b = c; }
    else { r = c; g = 0; b = x; }
    
    r = Math.floor((r + m) * 255);
    g = Math.floor((g + m) * 255);
    b = Math.floor((b + m) * 255);
    
    return {r, g, b, name: locator.name};
  }
  
  // If files aren't loaded yet, show loading message on black background
  if (!alsProject || !preloadedAudio) {
    wipe(0, 0, 0);
    ink(255, 255, 255);
    try {
      write(message, 20, screen.height / 2);
    } catch (e) {
      // Fallback if write fails - just continue without text
    }
    return;
  }
  
  // Update progress with detailed timing analysis
  if (isPlaying && playingSfx && actualDuration) {
    // Try to get current time from multiple sources
    const performanceElapsed = (performance.now() - playStartTime) / 1000;
    let audioContextTime = null;
    let audioProgress = null;
    
    // Method 1: Performance.now() elapsed time (fallback)
    let currentTime = performanceElapsed;
    let timingMethod = "performance";
    
    // Method 2: Try to get from audio context if available
    if (playingSfx.playResult && playingSfx.playResult.source && playingSfx.playResult.source.context) {
      const audioContext = playingSfx.playResult.source.context;
      const startTime = playingSfx.playResult.startTime;
      if (startTime !== undefined) {
        audioContextTime = audioContext.currentTime - startTime;
        currentTime = audioContextTime;
        timingMethod = "audioContext";
      }
    }
    
    // Method 3: Audio progress from speaker worklet (most accurate) 
    if (currentAudioProgress != null && !isNaN(currentAudioProgress)) {
      const progressBasedTime = currentAudioProgress * actualDuration;
      currentTime = progressBasedTime;
      timingMethod = "audioProgress";
      audioProgress = currentAudioProgress; // For logging
    }
    
    progress = Math.min(currentTime / actualDuration, 1);
    
    // Timing analysis disabled for performance
  }
  
  // Calculate current time in seconds based on project timeline, not audio duration
  let currentTimeSeconds;
  if (isPlaying && progress && alsProject) {
    // Use the actual audio timeline as the master - visuals follow audio completely
    // No offset needed since we're using locator times directly
    currentTimeSeconds = progress * actualDuration;
  } else {
    currentTimeSeconds = 0;
  }

  // === PRIMARY TV BAR COMPOSITION (SEPARATE BUFFER) ===
  // Render TV bars to their own buffer for isolation from UI elements
  
  // Clear and render to TV bars buffer
  page(tvBarsBuffer); // Switch to TV bars buffer

  wipe(0, 0, 0, 0); // Clear the buffer with transparent black each frame
  
  // The TV bar is the main visual element that fills the entire screen
  // All other elements are overlays on top of this base composition
  
  // ACTIVE ELEMENTS TRACKER - Collect all active/visible elements per frame
  const activeElements = {
    timestamp: currentTimeSeconds,
    currentLocators: [],
    nearbyLocators: [],
    visibleClips: [],
    activeNotes: [],
    visible3pxColors: [] // Track colors of currently playing MIDI notes
  };
  
  // Analyze current musical content to generate TV bar colors
  const centerX = screen.width / 2;
  
  // Get current locator to determine which palette to use
  const { current: currentLocator } = alsProject.getCurrentLocator(currentTimeSeconds, actualDuration);
  const currentPalette = getPaletteForLocator(currentLocator?.name);
  
  // Check for locator changes and clear decay on section transitions
  const currentLocatorName = currentLocator?.name || null;
  if (currentLocatorName !== previousLocatorName) {
    // Locator changed - clear all decaying colors for clean section transition
    colorHistory = [];
    previousLocatorName = currentLocatorName;
    console.log(`🎬 Locator changed to: ${currentLocatorName} - cleared color decay`);
  }
  
  // Process MIDI notes for current time to collect active colors
  if (alsProject && alsProject.locators && alsProject.locators.length > 0) {
    for (let locatorIndex = 0; locatorIndex < alsProject.locators.length; locatorIndex++) {
      const locator = alsProject.locators[locatorIndex];
      const segmentStart = locator.seconds;
      const segmentEnd = alsProject.locators[locatorIndex + 1]?.seconds || actualDuration;
      
      // Check ALL segments for currently playing notes, not just the current segment
      // This allows multiple notes from different segments to play simultaneously
      
      if (alsProject.clips) {
        alsProject.clips.forEach((clip, clipIndex) => {
          if (clip.type === 'midiclip' && clip.notes && clip.notes.length > 0) {
            clip.notes.forEach((note, noteIndex) => {
              // Calculate note timing in seconds
              const noteStartTime = clip.startSeconds + alsProject.beatsToSeconds(alsProject.alsTimeToBeat(note.time || 0));
              const noteEndTime = noteStartTime + alsProject.beatsToSeconds(alsProject.alsTimeToBeat(note.duration || 0.25));
              
              // Check if note is currently playing (regardless of which segment it's in)
              if (currentTimeSeconds >= noteStartTime && currentTimeSeconds <= noteEndTime) {
                const velocity = note.velocity || 100;
                const pitch = note.key || 60;
                const noteDuration = noteEndTime - noteStartTime;
                
                // Only log occasionally to avoid spam
                // if (Math.random() < 0.01) {
                //   console.log(`🎵 Found playing note: pitch=${pitch}, velocity=${velocity}, start=${noteStartTime.toFixed(2)}s, end=${noteEndTime.toFixed(2)}s, clip=${clipIndex}`);
                // }
                
                // LOCATOR-SPECIFIC PALETTE COLOR SELECTION
                // Use current locator to determine palette, then pick color from that palette
                const colorIndex = (clipIndex * 3 + noteIndex * 5 + pitch * 2) % currentPalette.colors.length;
                const baseColor = currentPalette.colors[colorIndex];
                
                // Apply velocity-based brightness variation
                const velocityFactor = 0.7 + (velocity / 127) * 0.3; // 0.7 to 1.0
                
                let r = Math.floor(baseColor.r * velocityFactor);
                let g = Math.floor(baseColor.g * velocityFactor);
                let b = Math.floor(baseColor.b * velocityFactor);
                
                // Ensure minimum brightness
                r = Math.max(30, r);
                g = Math.max(30, g);
                b = Math.max(30, b);
                
                const colorInfo = {
                  r: r,
                  g: g,
                  b: b,
                  pitch: pitch,
                  velocity: velocity,
                  duration: noteDuration,
                  startTime: noteStartTime,
                  clipIndex: clipIndex,
                  noteIndex: noteIndex,
                  paletteIndex: colorIndex, // Track which palette color was used
                  paletteName: currentPalette.name // Track which palette was used
                };
                
                activeElements.visible3pxColors.push(colorInfo);
              }
            });
          }
        });
      }
      
      // Track current locator for fallback color (only for the current segment)
      const isCurrentSegment = currentTimeSeconds >= segmentStart && currentTimeSeconds < segmentEnd;
      if (isCurrentSegment) {
        const locatorColor = getLocatorColor(locator, locatorIndex);
        activeElements.currentLocators.push({
          name: locator.name,
          color: locatorColor,
          startTime: segmentStart,
          endTime: segmentEnd
        });
      }
    }
  }
  
  // RENDER FULL-SCREEN TV BAR COMPOSITION
  const playingColors = activeElements.visible3pxColors;
  
  // Debug: Log raw playing colors before deduplication
  // if (playingColors.length > 0 && Math.random() < 0.05) {
  //   console.log(`🎨 DEBUG: ${playingColors.length} raw colors before dedup:`, playingColors.map(c => `pitch=${c.pitch}, start=${c.startTime.toFixed(2)}, RGB=(${c.r},${c.g},${c.b})`));
  // }
  
  // Remove duplicates based on startTime AND pitch to allow multiple notes starting at same time
  const uniqueColors = [];
  const seenNotes = new Set();
  
  playingColors.forEach(colorInfo => {
    // Create unique key combining startTime, pitch, clipIndex, and noteIndex
    // This prevents duplicate notes from same clip and allows multiple different notes
    const noteKey = `${colorInfo.startTime}-${colorInfo.pitch}-${colorInfo.clipIndex}-${colorInfo.noteIndex}`;
    if (!seenNotes.has(noteKey)) {
      seenNotes.add(noteKey);
      uniqueColors.push(colorInfo);
    }
  });
  
  // Debug logging (reduced frequency)
  if (uniqueColors.length > 0 && Math.random() < 0.1) {
    const paletteName = uniqueColors[0]?.paletteName || "Unknown";
    // console.log(`🎨 TV Bar: ${uniqueColors.length} unique colors detected after dedup (Palette: ${paletteName})`);
    if (uniqueColors.length > 1) {
      // console.log(`🎨 Multiple colors:`, uniqueColors.map(c => `pitch=${c.pitch}, clip=${c.clipIndex}, note=${c.noteIndex}, ${c.paletteName}[${c.paletteIndex}], RGB=(${c.r},${c.g},${c.b})`));
    }
  }
  
  // COLOR DECAY SYSTEM - restore visual persistence of notes
  // First, identify which colors are truly NEW (not in history yet)
  const newColorKeys = new Set();
  
  uniqueColors.forEach(colorInfo => {
    // Make each note much more unique so we get rapid bar addition
    // Include more specific details to ensure almost every note gets its own bar
    const colorKey = `${colorInfo.r}-${colorInfo.g}-${colorInfo.b}-${colorInfo.pitch || 0}-${colorInfo.clipIndex || 0}-${colorInfo.noteIndex || 0}-${Math.floor((colorInfo.startTime || 0) * 10)}`;
    const existingIndex = colorHistory.findIndex(h => h.colorKey === colorKey);
    
    if (existingIndex >= 0) {
      // Update existing color - reset decay
      colorHistory[existingIndex].alpha = 1.0;
      colorHistory[existingIndex].lastSeen = performance.now();
      colorHistory[existingIndex].isDecaying = false;
    } else {
      // Mark this as a new color that should flash
      newColorKeys.add(colorKey);
      
      // Add new color to history with unique key and better positioning
      // Create more spread-out positioning using full pitch range and stronger randomization
      const pitchNormalized = Math.min(127, Math.max(0, colorInfo.pitch || 60)) / 127; // Full MIDI range 0-127
      const timePosition = ((colorInfo.startTime || 0) % 8) / 8; // Longer time cycle for more spread
      const randomSpread = Math.random(); // Full 0-1 random for good distribution
      const preferredPosition = (pitchNormalized * 0.4 + timePosition * 0.2 + randomSpread * 0.4); // More randomness
      
      colorHistory.push({
        r: colorInfo.r,
        g: colorInfo.g,
        b: colorInfo.b,
        alpha: 1.0,
        lastSeen: performance.now(),
        isDecaying: false,
        preferredPosition: preferredPosition,
        colorKey: colorKey,
        birthTime: performance.now(), // Track when this bar was born for flash effect
        pitch: colorInfo.pitch || 60 // Store pitch for debugging
      });
    }
  });
  
  // Update decay for all colors in history
  const now = performance.now();
  const decayDuration = 3000; // Reduced from 5 seconds to 3 seconds for faster clearing
  
  colorHistory.forEach(color => {
    const timeSinceLastSeen = now - color.lastSeen;
    if (timeSinceLastSeen > 100) { // Start decaying after 100ms (faster than before)
      color.isDecaying = true;
      color.alpha = Math.max(0, 1.0 - (timeSinceLastSeen - 100) / decayDuration);
    }
  });
  
  // Remove fully decayed colors
  colorHistory = colorHistory.filter(color => color.alpha > 0.05);
  
  // Combine active and decaying colors - add new bars to CENTER, alternating left/right
  // Only give birthTime to ACTUALLY new colors (using the newColorKeys set)
  const allActiveColors = uniqueColors.map(color => {
    // Check if this exact color was marked as new
    const colorKey = `${color.r}-${color.g}-${color.b}-${color.pitch || 0}-${color.clipIndex || 0}-${color.noteIndex || 0}-${Math.floor((color.startTime || 0) * 10)}`;
    const isNewColor = newColorKeys.has(colorKey);
    
    return {
      ...color,
      birthTime: isNewColor ? performance.now() : null, // Only truly new colors get birthTime
      isNewBar: isNewColor // Mark as new only if it was just added
    };
  });
  
  const allDecayingColors = colorHistory.filter(h => h.isDecaying); // Decaying colors
  
  // Create center-out arrangement: put decaying colors on edges, new ones in center
  // Alternate new bars left and right of center
  const centerArrangement = [];
  const centerIndex = Math.floor((allDecayingColors.length + allActiveColors.length) / 2);
  
  // First, place all decaying colors on the edges
  allDecayingColors.forEach((color, index) => {
    if (index % 2 === 0) {
      centerArrangement.push(color); // Even indices go left
    } else {
      centerArrangement.unshift(color); // Odd indices go right (unshift = add to start)
    }
  });
  
  // Then add new active colors in the center, alternating left/right
  allActiveColors.forEach((color, index) => {
    const midPoint = Math.floor(centerArrangement.length / 2);
    if (index % 2 === 0) {
      centerArrangement.splice(midPoint, 0, color); // Even: insert at center-left
    } else {
      centerArrangement.splice(midPoint + 1, 0, color); // Odd: insert at center-right
    }
  });
  
  const activeSegments = centerArrangement;
  
  if (activeSegments.length > 0) {
    // Show ALL colors - no arbitrary limits, let natural decay control the count
    const colorsToShow = activeSegments;
    
    // Don't sort - keep new bars on the right side
    // Order: decaying colors first (left), then new active colors (right)
    const positionSortedColors = colorsToShow;
    
    // Equal width bars that divide the screen evenly - ensure no cut-off
    const totalBars = positionSortedColors.length;
    const barWidth = totalBars > 0 ? Math.floor(screen.width / totalBars) : screen.width;
    
    positionSortedColors.forEach((colorInfo, index) => {
      try {
        // Each bar gets an equal slice, positioned sequentially from left to right
        const startX = index * barWidth;
        // Ensure last bar doesn't go off screen
        const actualWidth = (index === totalBars - 1) ? (screen.width - startX) : barWidth;
        const clampedStartX = Math.min(startX, screen.width - actualWidth);
        
        // Handle white flash for new bars and fading for decaying bars
        const now = performance.now();
        
        if (colorInfo.isDecaying) {
          // Fade to black for decaying colors
          const alpha = colorInfo.alpha;
          const fadedR = Math.floor(colorInfo.r * alpha);
          const fadedG = Math.floor(colorInfo.g * alpha);
          const fadedB = Math.floor(colorInfo.b * alpha);
          ink(fadedR, fadedG, fadedB);
        } else {
          // Check for gradient flash - ALL new bars (active colors) should flash yellow-white
          const barAge = now - (colorInfo.birthTime || 0);
          const flashDuration = 16.67; // Exactly 1 frame at 60fps
          
          // Flash yellow-white gradient if it's a new bar (has birthTime and is young enough)
          if (colorInfo.birthTime && barAge < flashDuration) {
            // Yellow-white gradient flash with vertical direction for first frame ONLY
            ink("fade:yellow-white:vertical");
            
            // Debug flash - log very rarely to avoid spam
            if (Math.random() < 0.01) {
              console.log(`⚡ VERTICAL GRADIENT FLASH! Bar ${index}: yellow-white vertical`);
            }
          } else {
            // Normal RGB color after flash period (not gradient)
            ink(colorInfo.r, colorInfo.g, colorInfo.b);
          }
        }
        
        // Get audio data for this bar - now with frequency bands!
        const amplitude = sound.speaker?.amplitudes?.left || 0; // Single number for overall intensity
        const audioWaveform = sound.speaker?.waveforms?.left || []; // Array for per-bar variation
        const frequencyBands = sound.speaker?.frequencies?.left || []; // Array of frequency band objects
        
        // Calculate amplitude-based height with frequency band analysis
        let amplitudeHeight = screen.height;
        
        // Try to use frequency bands first (more musical), fallback to waveform
        if (frequencyBands.length > 0) {
          // Map TV bars to frequency bands
          const bandIndex = index % frequencyBands.length;
          const frequencyBand = frequencyBands[bandIndex];
          const bandAmplitude = frequencyBand?.amplitude || 0;
          
          // Combine overall amplitude (50%) with frequency band amplitude (50%)
          const combinedAmplitude = (amplitude * 0.5) + (bandAmplitude * 0.5);
          
          // Scale amplitude to height
          const minHeight = screen.height * 0.1; // 10% minimum
          const maxHeight = screen.height;
          amplitudeHeight = minHeight + combinedAmplitude * (maxHeight - minHeight);
          
          // Optional: Debug frequency band data occasionally
          if (index === 0 && Math.random() < 0.05) {
            console.log(`🎵 Frequency bands:`, frequencyBands.map(b => `${b.name}:${(b.amplitude * 100).toFixed(0)}%`).join(' '));
          }
          
        } else if (audioWaveform.length > 0) {
          // Fallback to waveform approach
          const waveformIndex = Math.floor((index / totalBars) * audioWaveform.length);
          const waveformValue = Math.abs(audioWaveform[waveformIndex] || 0);
          
          // Combine overall amplitude (70%) with per-bar waveform variation (30%)
          const combinedAmplitude = (amplitude * 0.7) + (waveformValue * 0.3);
          
          // Scale amplitude to height (amplitude values are 0-1)
          const minHeight = screen.height * 0.1; // 10% minimum
          const maxHeight = screen.height;
          amplitudeHeight = minHeight + combinedAmplitude * (maxHeight - minHeight);
        } else {
          // Fallback to just amplitude if no waveform data
          const minHeight = screen.height * 0.1;
          const maxHeight = screen.height;
          amplitudeHeight = minHeight + amplitude * (maxHeight - minHeight);
        }
        
        // Center the bar vertically
        const barHeight = Math.floor(amplitudeHeight);
        const barY = Math.floor((screen.height - barHeight) / 2);
        
        // Draw the bar with amplitude-based height
        box(clampedStartX, barY, actualWidth, barHeight);
        
        // CRITICAL: Reset fade mode by calling ink with a non-fade string
        // This clears the global fadeMode, fadeColors, fadeDirection variables
        ink("black"); // Forces findColor to reset fade state
      } catch (error) {
        console.error('Error rendering TV bar:', error);
        // Fallback: render in red to show something went wrong
        ink(255, 0, 0);
        box(10 + index * 20, 0, 15, screen.height);
      }
    });
    
  } else {
    // Fallback: show current locator theme color across full screen with amplitude height
    if (currentPalette && currentPalette.colors && currentPalette.colors.length > 0) {
      // Use the same base color logic as timeline background and segments
      // Find the current locator index to match the segment color logic
      let currentLocatorIndex = 0;
      if (alsProject && alsProject.locators && currentLocator) {
        currentLocatorIndex = alsProject.locators.findIndex(loc => loc.name === currentLocator.name);
        if (currentLocatorIndex === -1) currentLocatorIndex = 0;
      }
      const colorIndex = currentLocatorIndex % currentPalette.colors.length;
      const bgColor = currentPalette.colors[colorIndex];
      ink(bgColor.r, bgColor.g, bgColor.b); // Solid palette color, no pulse
      
      // Apply amplitude-based height to fallback as well
      const amplitude = sound.speaker?.amplitudes?.left || 0;
      let amplitudeHeight = screen.height;
      
      if (amplitude > 0) {
        const minHeight = screen.height * 0.1;
        const maxHeight = screen.height;
        amplitudeHeight = minHeight + amplitude * (maxHeight - minHeight);
      }
      
      const barHeight = Math.floor(amplitudeHeight);
      const barY = Math.floor((screen.height - barHeight) / 2);
      box(0, barY, screen.width, barHeight); // Amplitude-based height
    } else {
      // Default background when no content is playing - match timeline fallback
      ink(50, 50, 50);
      
      // Even default gets amplitude if available
      const amplitude = sound.speaker?.amplitudes?.left || 0;
      let amplitudeHeight = screen.height;
      
      if (amplitude > 0) {
        const minHeight = screen.height * 0.1;
        const maxHeight = screen.height;
        amplitudeHeight = minHeight + amplitude * (maxHeight - minHeight);
      }
      
      const barHeight = Math.floor(amplitudeHeight);
      const barY = Math.floor((screen.height - barHeight) / 2);
      box(0, barY, screen.width, barHeight); // Amplitude-based height
    }
  }

  // === WAVEFORM OVERLAY ON TV BARS ===
  // Draw waveform on top of the TV bars (inspired by whistle.mjs)
  const waveform = sound.speaker?.waveforms?.left || [];
  const amplitude = sound.speaker?.amplitudes?.left || 0;
  
  if (waveform.length > 0) {
    // Waveform rendering (similar to whistle.mjs paintSound function)
    const xStep = screen.width / waveform.length;
    const yMid = screen.height / 2;
    const yMax = screen.height / 2;
    
    // Create waveform points
    const waveformPoints = waveform.map((v, i) => [
      i * xStep, 
      yMid + v * yMax * 0.8 // Scale down to 80% to fit nicely over TV bars
    ]);
    
    // Draw waveform as a translucent white/cyan line over the TV bars
    ink(255, 255, 255, 128); // Semi-transparent white
    poly(waveformPoints);
    
    // Optional: Draw amplitude bounding box (like whistle.mjs)
    // ink(255, 255, 0, 64); // Semi-transparent yellow
    // box(screen.width / 2, yMid, screen.width * 0.8, amplitude * yMax * 2, "*center");
  }

  zoom(0.5);
  scroll(paintCount, paintCount);

  // === END TV BARS RENDERING ===
  // Switch back to main screen and paste the TV bars buffer with blur effect
  page(screen);
  paste(tvBarsBuffer);
  // blur(1); // Apply blur to the TV bars buffer to test separation
  // zoom(0.5);

  // === DYNAMIC TIMELINE ZOOM BASED ON NOTE DENSITY ===
  
  // Function to calculate note density in a time window
  function calculateNoteDensity(startTime, endTime) {
    let noteCount = 0;
    const timeWindow = endTime - startTime;
    
    if (alsProject && alsProject.clips) {
      alsProject.clips.forEach(clip => {
        if (clip.type === 'midiclip' && clip.notes) {
          clip.notes.forEach(note => {
            const noteStartTime = clip.startSeconds + alsProject.beatsToSeconds(alsProject.alsTimeToBeat(note.time || 0));
            if (noteStartTime >= startTime && noteStartTime <= endTime) {
              noteCount++;
            }
          });
        }
      });
    }
    
    return timeWindow > 0 ? noteCount / timeWindow : 0; // notes per second
  }
  
  // Fixed zoom level for consistent behavior (no dynamic scaling)
  const pixelsPerSecond = 60; // Increased from 35 to 60 px/sec for faster scrolling / more zoomed in view

  // === TIMELINE OVERLAY (MINIMAL & FAST) ===
  // The timeline is now an overlay element that sits on top of the TV bar composition
  
  if (timelineVisible) {
    // Minimal timeline layout parameters
    const timelineHeight = 25; // Reduced from 50 to 25 - much more minimal
    const timelineY = screen.height - timelineHeight; // Position at bottom edge
    const leadTimeSeconds = 2.0; // Show 2 seconds of future time
    const timelineWindowSeconds = screen.width / pixelsPerSecond;
    // Calculate current view window - align to start at the red center line
    const viewStartTime = currentTimeSeconds;
    const viewEndTime = Math.min(actualDuration, currentTimeSeconds + timelineWindowSeconds);
    
    // Themed timeline background - matches current TV color palette exactly
    if (currentPalette && currentPalette.colors && currentPalette.colors.length > 0) {
      // Use the same base color logic as timeline segments and TV bars
      // Find the current locator index to match the segment color logic
      let currentLocatorIndex = 0;
      if (alsProject && alsProject.locators && currentLocator) {
        currentLocatorIndex = alsProject.locators.findIndex(loc => loc.name === currentLocator.name);
        if (currentLocatorIndex === -1) currentLocatorIndex = 0;
      }
      const colorIndex = currentLocatorIndex % currentPalette.colors.length;
      const bgColor = currentPalette.colors[colorIndex];
      ink(bgColor.r, bgColor.g, bgColor.b); // Solid palette color, no pulse
    } else {
      ink(50, 50, 50); // Fallback to match TV default { r: 50, g: 50, b: 50 }
    }
    box(0, timelineY, screen.width, timelineHeight); // Simple strip at bottom
  
  // Draw locator segments on timeline overlay
  if (alsProject && alsProject.locators) {
    for (let i = 0; i < alsProject.locators.length; i++) {
      const locator = alsProject.locators[i];
      const segmentStart = locator.seconds;
      const segmentEnd = alsProject.locators[i + 1]?.seconds || actualDuration;
      
      // Calculate screen coordinates for this segment relative to red center line
      const segmentStartX = centerX + (segmentStart - currentTimeSeconds) * pixelsPerSecond;
      const segmentEndX = centerX + (segmentEnd - currentTimeSeconds) * pixelsPerSecond;
      const segmentWidth = segmentEndX - segmentStartX;
      
      // Only draw if segment is visible on screen
      if (segmentEndX > -50 && segmentStartX < screen.width + 50 && segmentWidth > 0) {
        // Use the same palette-based color logic as TV bars instead of getLocatorColor
        const segmentPalette = getPaletteForLocator(locator.name);
        const colorIndex = i % segmentPalette.colors.length; // Simple index-based selection for timeline
        const paletteColor = segmentPalette.colors[colorIndex];
        
        const isCurrentTime = currentTimeSeconds >= segmentStart && currentTimeSeconds < segmentEnd;
        
        let r = paletteColor.r;
        let g = paletteColor.g; 
        let b = paletteColor.b;
        
        // No brightness modifications - use raw palette colors to match base backgrounds
        
        // Draw segment with proper clipping
        ink(r, g, b);
        const clampedX = Math.max(0, segmentStartX);
        const effectiveWidth = segmentStartX < 0 ? segmentWidth + segmentStartX : segmentWidth;
        const clampedWidth = Math.min(effectiveWidth, screen.width - clampedX);
        if (clampedWidth > 0) {
          box(clampedX, timelineY, clampedWidth, timelineHeight);
        }
      }
    }
  }
  
  // Draw MIDI notes overlay on timeline
  if (alsProject && alsProject.locators && alsProject.locators.length > 0) {
    for (let locatorIndex = 0; locatorIndex < alsProject.locators.length; locatorIndex++) {
      const locator = alsProject.locators[locatorIndex];
      const segmentStart = locator.seconds;
      const segmentEnd = alsProject.locators[locatorIndex + 1]?.seconds || actualDuration;
      
      // Calculate segment position in the scrolling view
      const segmentStartX = (segmentStart - viewStartTime) * pixelsPerSecond;
      const segmentEndX = (segmentEnd - viewStartTime) * pixelsPerSecond;
      const segmentWidth = segmentEndX - segmentStartX;
      
      // Always process segments to check for individual note visibility
      // Don't skip segments just because the segment boundary is offscreen
      if (segmentWidth > 1) {
        // Get all MIDI notes for this segment
        const segmentNotes = [];
        
        if (alsProject.clips) {
          alsProject.clips.forEach((clip, clipIndex) => {
            if (clip.type === 'midiclip' && clip.notes && clip.notes.length > 0) {
              clip.notes.forEach((note, noteIndex) => {
                const noteStartTime = clip.startSeconds + alsProject.beatsToSeconds(alsProject.alsTimeToBeat(note.time || 0));
                const noteEndTime = noteStartTime + alsProject.beatsToSeconds(alsProject.alsTimeToBeat(note.duration || 0.25));
                
                // Check if note belongs to this segment AND if the note itself is potentially visible
                if (noteStartTime >= segmentStart && noteStartTime < segmentEnd) {
                  const noteX = centerX + (noteStartTime - currentTimeSeconds) * pixelsPerSecond;
                  const noteDuration = noteEndTime - noteStartTime;
                  const noteWidth = Math.max(1, Math.floor(noteDuration * pixelsPerSecond));
                  
                  // Only include notes that are actually visible on screen
                  if (noteX + noteWidth >= -10 && noteX < screen.width + 10) {
                    segmentNotes.push({
                      ...note,
                      startTime: noteStartTime,
                      endTime: noteEndTime,
                      clipName: clip.name,
                      clipIndex: clipIndex, // Add clip index for color calculation
                      noteIndex: noteIndex, // Add note index for color calculation
                      locatorIndex: locatorIndex, // Add locator index for stable row calculation
                      trackIndex: clip.trackIndex
                    });
                  }
                }
              });
            }
          });
        }
        
        if (segmentNotes.length > 0) {
          // Draw MIDI notes as small rectangles with no gaps
          const notesAreaHeight = timelineHeight - 2; // Use almost full timeline height
          const notesAreaStartY = timelineY + 1; // Start 1px from top
          const noteHeight = Math.max(2, Math.floor(notesAreaHeight / 10)); // Fixed height, max 10 rows
          const maxRows = Math.floor(notesAreaHeight / noteHeight);
          
          segmentNotes.forEach((note, noteIndex) => {
            const noteX = centerX + (note.startTime - currentTimeSeconds) * pixelsPerSecond;
            const pitch = note.key || 60;
            // Use stable row calculation based on original note position
            const stableIndex = (note.locatorIndex * 100) + (note.clipIndex * 20) + note.noteIndex;
            const row = stableIndex % maxRows;
            const noteY = notesAreaStartY + row * noteHeight; // No spacing, notes touch
            
            // Note is already pre-filtered for visibility, so just draw it
            const velocity = note.velocity || 100;
            const noteDuration = note.endTime - note.startTime; // Single declaration for the whole block
            const velocityNorm = velocity / 127; // Move this outside color block so it's available everywhere
              
              // Generate themed color using the palette of the note's own locator section
              let r, g, b;
              // Get the palette for the locator that this note belongs to
              const noteLocator = alsProject.locators[note.locatorIndex];
              const notePalette = getPaletteForLocator(noteLocator?.name);
              
              if (notePalette && notePalette.colors && notePalette.colors.length > 0) {
                // Use the note's own locator palette, not the current active palette
                const colorIndex = (note.clipIndex * 3 + note.noteIndex * 5 + pitch * 2) % notePalette.colors.length;
                const baseColor = notePalette.colors[colorIndex];
                
                // Apply velocity-based brightness variation (same as TV bars)
                const velocityFactor = 0.7 + (velocityNorm * 0.3); // 0.7 to 1.0
                
                r = Math.floor(baseColor.r * velocityFactor);
                g = Math.floor(baseColor.g * velocityFactor);
                b = Math.floor(baseColor.b * velocityFactor);
                
                // Ensure minimum brightness (same as TV bars)
                r = Math.max(30, r);
                g = Math.max(30, g);
                b = Math.max(30, b);
              } else {
                // Fallback to original HSL method if no palette
                const hue = (pitch * 7 + row * 60) % 360;
                const saturation = 0.7 + (velocityNorm * 0.3);
                const lightness = 0.3 + (velocityNorm * 0.5);
                
                const c = (1 - Math.abs(2 * lightness - 1)) * saturation;
                const x = c * (1 - Math.abs(((hue / 60) % 2) - 1));
                const m = lightness - c / 2;
                
                if (hue < 60) { r = c; g = x; b = 0; }
                else if (hue < 120) { r = x; g = c; b = 0; }
                else if (hue < 180) { r = 0; g = c; b = x; }
                else if (hue < 240) { r = 0; g = x; b = c; }
                else if (hue < 300) { r = x; g = 0; b = c; }
                else { r = c; g = 0; b = x; }
                
                r = Math.floor((r + m) * 255);
                g = Math.floor((g + m) * 255);
                b = Math.floor((b + m) * 255);
              }
              
              // Simple note width based on duration only
              const minWidth = 1;
              const durationWidth = Math.max(minWidth, Math.floor(noteDuration * pixelsPerSecond));
              // Use the fixed noteHeight calculated above for consistent spacing
              const finalHeight = noteHeight;
              
              // Check playing state - simplified without decay
              const noteEnd = note.endTime || (note.startTime + 0.25);
              const isPlaying = currentTimeSeconds >= note.startTime && currentTimeSeconds <= noteEnd;
              
              // Simple note width based on duration
              const noteWidth = Math.max(1, Math.floor(noteDuration * pixelsPerSecond));
              
              // Draw the note
              let noteR = r, noteG = g, noteB = b;
              if (isPlaying) {
                // Capture the base color BEFORE brightening for needle theming
                lastMidiNoteColor = { r: r, g: g, b: b };
                
                // Note is actively playing - brighten for display
                noteR = Math.min(255, r + 60);
                noteG = Math.min(255, g + 60);
                noteB = Math.min(255, b + 60);
              }
              ink(noteR, noteG, noteB);
              box(noteX, noteY, noteWidth, finalHeight);
          });
        }
      }
    }
  } // Close the segmentNotes.forEach or similar loop that was missing
    
    // Draw themed needle with MIDI note color
    const needleX = centerX; // Center perfectly
    
    // Use the exact color of the last played MIDI note for the needle
    const needleColor = {
      r: lastMidiNoteColor.r,
      g: lastMidiNoteColor.g,
      b: lastMidiNoteColor.b
    };
    
    // Draw single needle line matching MIDI note color exactly
    // timelineY and timelineHeight are already declared at the top of the timeline conditional
    ink(needleColor.r, needleColor.g, needleColor.b, 255); // Full opacity
    line(needleX, timelineY, needleX, timelineY + timelineHeight); // Single centered line
    
  } // Close timeline visibility conditional
  
} // Close paint function

function act({ event: e, sound }) {
  // Toggle timeline visibility with 't' key or tab key (using standard tab handler pattern)
  if (e.is("keyboard:t") || (e.is("keyboard:down:tab") && e.key === "Tab")) {
    timelineVisible = !timelineVisible;
  }
  
  // Simple play/pause control
  if (e.is("keyboard:p")) {
    timelineOffset += 0.5; // Look further ahead
  }
  
  if (e.is("keyboard:minus")) {
    timelineOffset = Math.max(0, timelineOffset - 0.5); // Look less ahead (minimum 0)
  }
  
  // Handle play/pause
  if (e.is("touch") && preloadedAudio) {
    if (!isPlaying) {
      // Reset tracking when starting playback
      passedLocators.clear();
      console.log("🎬 Starting playback - reset locator tracking");
      
      // Start playing
      playingSfx = sound.play(preloadedAudio);
      if (playingSfx) {
        isPlaying = true;
        playStartTime = performance.now();
        
        // Try multiple ways to get duration
        console.log("playingSfx object:", playingSfx);
        
        // Method 1: From the BIOS sample info
        if (sound.speaker && sound.speaker.length && sound.speaker.sampleRate) {
          actualDuration = sound.speaker.length / sound.speaker.sampleRate;
          console.log("Got duration from speaker:", actualDuration, "length:", sound.speaker.length, "sampleRate:", sound.speaker.sampleRate);
        }
        
        // Method 2: From playResult
        if (playingSfx.playResult && playingSfx.playResult.buffer) {
          actualDuration = playingSfx.playResult.buffer.duration;
          console.log("Got duration from playResult buffer:", actualDuration);
        }
        
        // Method 3: From the preloaded audio metadata
        if (preloadedAudio && preloadedAudio.buffer && preloadedAudio.buffer.duration) {
          actualDuration = preloadedAudio.buffer.duration;
          console.log("Got duration from preloaded audio:", actualDuration);
        }
        
        // Fallback: calculate from sample info in console logs
        // We saw: length: 8831329, sampleRate: 48000
        if (!actualDuration) {
          actualDuration = 8831329 / 48000; // About 184 seconds based on the console output
          console.log("Using fallback duration calculation:", actualDuration);
        }
        
        console.log("Final duration:", actualDuration);
        console.log("Started playing zzzZWAP");
      }
    } else {
      // Pause/stop
      if (playingSfx) {
        playingSfx.kill();
        playingSfx = null;
      }
      isPlaying = false;
      console.log("Stopped playing");
    }
  }
}

// Simple auto-stop when audio ends
function sim({ sound }) {
  if (isPlaying && playingSfx && playingSfx.killed) {
    isPlaying = false;
    progress = 0;
    playingSfx = null;
  }
  
  // Get actual audio progress from speaker worklet (following stample pattern)
  if (isPlaying && playingSfx && playingSfx.progress && typeof playingSfx.progress === 'function') {
    playingSfx.progress().then((p) => {
      if (p && typeof p.progress === 'number') {
        currentAudioProgress = p.progress;
      }
    }).catch(err => {
      // Silent catch - don't spam console
    });
  }
  
  // Poll speaker for real-time analysis (following wipppps pattern)
  sound.speaker?.poll();
}

export { paint, act, sim };
