// Ableton Live Project Visualizer, 2025.08.10
// Enhanced ALS parser with sophisticated timeline analysis
// Based on reference/analyze-ableton.mjs architecture

// ALS Project Parser Class - Enhanced with reference tools knowledge
class ALSProject {
  constructor(xmlData) {
    this.tracks = [];
    this.scenes = [];
    this.clips = [];
    this.devices = [];
    this.locators = [];
    this.warpMarkers = [];
    this.tempoEvents = [];
    this.tempo = 120;
    this.timeSignature = { numerator: 4, denominator: 4 };
    this.creator = "Unknown";
    this.version = "Unknown";
    this.projectName = "Untitled";
    this.debugMode = true;
    this.totalElements = 0;
    this.notes = [];
    this.automationEnvelopes = [];
    
    // Performance tracking
    this.parseStats = {
      startTime: 0,
      parseTime: 0,
      elementCount: 0
    };
    
    if (xmlData) {
      this.parseXML(xmlData);
      this.analyzeProjectStructure();
    }
  }
  
  parseXML(xmlData) {
    try {
      console.log("🎵 Parsing ALS XML data...");
      this.parseStats.startTime = performance.now();
      
      // Parse different components in order of importance
      this.parseGlobalSettings(xmlData);
      this.parseLocators(xmlData);  // Parse locators early for structure
      this.parseTempo(xmlData);
      this.parseTracks(xmlData);
      this.parseClips(xmlData);
      this.parseWarpMarkers(xmlData);
      this.parseDevices(xmlData);
      this.parseScenes(xmlData);
      
      // Calculate parse performance
      this.parseStats.parseTime = performance.now() - this.parseStats.startTime;
      
      console.log(`🎵 Parse complete: ${this.parseStats.parseTime.toFixed(1)}ms`);
      console.log(`📊 Found: ${this.tracks.length} tracks, ${this.clips.length} clips, ${this.locators.length} locators, ${this.warpMarkers.length} warp markers`);
      
    } catch (error) {
      console.error("❌ Error parsing ALS XML:", error);
      this.tempo = 120; // Fallback
    }
  }
  
  parseGlobalSettings(xmlData) {
    // Extract project-level metadata
    const creatorMatch = xmlData.match(/Creator="([^"]+)"/);
    if (creatorMatch) {
      this.creator = creatorMatch[1];
      const versionMatch = creatorMatch[1].match(/(\d+\.\d+)/);
      if (versionMatch) this.version = versionMatch[1];
    }
    
    // Extract project name if available
    const projectNameMatch = xmlData.match(/<ProjectName[^>]*Value="([^"]+)"/);
    if (projectNameMatch) {
      this.projectName = projectNameMatch[1];
    }
  }
  
  parseTempo(xmlData) {
    console.log("🎵 Starting tempo parsing...");
    
    // Pattern 1: Look for the Tempo structure we found in XML analysis
    const tempoMatch = xmlData.match(/<Tempo>[\s\S]*?<Manual Value="([^"]+)"[\s\S]*?<\/Tempo>/);
    if (tempoMatch) {
      this.tempo = parseFloat(tempoMatch[1]);
      console.log(`🎵 Found tempo: ${this.tempo} BPM`);
      return;
    }
    
    // Pattern 2: Try alternative pattern with just Manual Value
    const manualMatch = xmlData.match(/<Manual Value="([^"]+)"/);
    if (manualMatch) {
      const value = parseFloat(manualMatch[1]);
      if (value >= 60 && value <= 300) {
        this.tempo = value;
        console.log(`🎵 Found tempo (Manual): ${this.tempo} BPM`);
        return;
      }
    }
    
    // Pattern 3: Extract from filename (zzzZWAP 143bpm)
    const nameMatch = this.projectName.match(/(\d+)bpm/i);
    if (nameMatch) {
      this.tempo = parseInt(nameMatch[1]);
      console.log(`🎵 Extracted tempo from filename: ${this.tempo} BPM`);
      return;
    }
    
    // If no tempo found, keep default
    if (this.tempo === 120) {
      console.warn(`⚠️ Could not find tempo in ALS file, using default ${this.tempo} BPM`);
    }
    
    // Time signature parsing
    const sigNumMatch = xmlData.match(/<TimeSignatureNumerator Value="([^"]+)"/);
    const sigDenMatch = xmlData.match(/<TimeSignatureDenominator Value="([^"]+)"/);
    if (sigNumMatch && sigDenMatch) {
      this.timeSignature = {
        numerator: parseInt(sigNumMatch[1]),
        denominator: parseInt(sigDenMatch[1])
      };
      console.log(`🎵 Time signature: ${this.timeSignature.numerator}/${this.timeSignature.denominator}`);
    }
  }
  
  parseTracks(xmlData) {
    // Parse all track types with detailed information
    const trackTypes = ['MidiTrack', 'AudioTrack', 'ReturnTrack', 'GroupTrack'];
    
    trackTypes.forEach(trackType => {
      const regex = new RegExp(`<${trackType}[^>]*Id="([^"]*)"[^>]*>([\\s\\S]*?)<\/${trackType}>`, 'g');
      let match;
      
      while ((match = regex.exec(xmlData)) !== null) {
        const trackId = match[1];
        const trackContent = match[2];
        
        const track = {
          id: trackId,
          type: trackType,
          name: this.extractTrackName(trackContent),
          color: this.extractTrackColor(trackContent),
          muted: this.extractTrackMuted(trackContent),
          solo: this.extractTrackSolo(trackContent),
          clipSlots: [],
          devices: []
        };
        
        this.tracks.push(track);
      }
    });
    
    console.log(`🎵 Parsed tracks: ${this.tracks.map(t => `${t.name}(${t.type})`).join(', ')}`);
  }
  
  parseScenes(xmlData) {
    // Extract scene information for session view
    const sceneRegex = /<Scene[^>]*Id="([^"]*)"[^>]*>([\s\S]*?)<\/Scene>/g;
    let match;
    
    while ((match = sceneRegex.exec(xmlData)) !== null) {
      const sceneId = match[1];
      const sceneContent = match[2];
      
      const scene = {
        id: sceneId,
        name: this.extractSceneName(sceneContent) || `Scene ${this.scenes.length + 1}`,
        tempo: this.extractSceneTempo(sceneContent),
        timeSignature: this.extractSceneTimeSignature(sceneContent)
      };
      
      this.scenes.push(scene);
    }
  }
  
  // NEW: Parse locators (crucial for song structure visualization)
  parseLocators(xmlData) {
    console.log("🎯 Parsing locators...");
    
    // Find all Locator elements - these mark important song positions
    const locatorRegex = /<Locator[^>]*Id="([^"]*)"[^>]*>([\s\S]*?)<\/Locator>/g;
    let match;
    
    while ((match = locatorRegex.exec(xmlData)) !== null) {
      const locatorId = match[1];
      const locatorContent = match[2];
      
      // Extract time and name
      const timeMatch = locatorContent.match(/<Time Value="([^"]*)"/);
      const nameMatch = locatorContent.match(/<Name Value="([^"]*)"/);
      
      if (timeMatch) {
        const locator = {
          id: locatorId,
          time: parseFloat(timeMatch[1]),
          name: nameMatch ? nameMatch[1] : `Locator ${this.locators.length}`,
          beat: this.getCurrentBeat(parseFloat(timeMatch[1]) * 60 / this.tempo) // Convert to beat
        };
        
        this.locators.push(locator);
      }
    }
    
    // Sort locators by time
    this.locators.sort((a, b) => a.time - b.time);
    
    console.log(`🎯 Found ${this.locators.length} locators:`, 
      this.locators.slice(0, 5).map(l => `${l.name}@${l.time.toFixed(1)}`).join(', '));
  }
  
  // NEW: Parse warp markers (audio time manipulation)
  parseWarpMarkers(xmlData) {
    console.log("🌊 Parsing warp markers...");
    
    // Find all WarpMarker elements
    const warpRegex = /<WarpMarker[^>]+SecTime="([^"]+)"[^>]+BeatTime="([^"]+)"[^>]*\/>/g;
    let match;
    
    while ((match = warpRegex.exec(xmlData)) !== null) {
      const warpMarker = {
        secTime: parseFloat(match[1]),
        beatTime: parseFloat(match[2]),
        ratio: parseFloat(match[2]) / parseFloat(match[1]) // Beat/second ratio
      };
      
      this.warpMarkers.push(warpMarker);
    }
    
    console.log(`🌊 Found ${this.warpMarkers.length} warp markers`);
  }
  
  parseClips(xmlData) {
    console.log("🎵 Starting enhanced clip parsing...");
    
    // Parse clips using the more sophisticated approach from reference tools
    this.parseArrangementClips(xmlData);
    this.parseMIDIClips(xmlData);
    this.parseAudioClips(xmlData);
    
    // Sort clips by time for timeline visualization
    this.clips.sort((a, b) => a.time - b.time);
    
    console.log(`🎵 Total clips parsed: ${this.clips.length}`);
    console.log(`📊 Clip types: MIDI=${this.clips.filter(c => c.type === 'MIDI').length}, Audio=${this.clips.filter(c => c.type === 'Audio').length}`);
  }
  
  parseArrangementClips(xmlData) {
    console.log("🎼 Parsing arrangement clips with enhanced extraction...");
    let arrangementClips = 0;
    
    // Enhanced parsing based on reference tools - look for clips with CurrentStart/CurrentEnd
    const clipPatterns = [
      // Pattern 1: MidiClip with Time (arrangement)
      /<MidiClip[^>]*Time="([^"]+)"[^>]*>([\s\S]*?)<\/MidiClip>/g,
      // Pattern 2: AudioClip with Time (arrangement)  
      /<AudioClip[^>]*Time="([^"]+)"[^>]*>([\s\S]*?)<\/AudioClip>/g
    ];
    
    clipPatterns.forEach((pattern, patternIndex) => {
      const clipType = patternIndex === 0 ? 'MIDI' : 'Audio';
      let match;
      
      while ((match = pattern.exec(xmlData)) !== null) {
        const clipTime = parseFloat(match[1]);
        const clipContent = match[2];
        
        // Enhanced clip data extraction using reference methods
        const currentStart = this.extractValueFromContent(clipContent, 'CurrentStart') || 0;
        const currentEnd = this.extractValueFromContent(clipContent, 'CurrentEnd') || clipTime + 1;
        const loopStart = this.extractValueFromContent(clipContent, 'LoopStart') || 0;
        const loopEnd = this.extractValueFromContent(clipContent, 'LoopEnd') || currentEnd - currentStart;
        
        const clip = {
          id: `arrangement_${arrangementClips}`,
          type: clipType,
          time: currentStart, // Use CurrentStart for more accurate timing
          duration: currentEnd - currentStart,
          currentStart: parseFloat(currentStart),
          currentEnd: parseFloat(currentEnd),
          loopStart: parseFloat(loopStart),
          loopEnd: parseFloat(loopEnd),
          name: this.extractClipName(clipContent) || `${clipType} Clip ${arrangementClips}`,
          notes: clipType === 'MIDI' ? this.extractMIDINotes(clipContent) : [],
          sampleRef: clipType === 'Audio' ? this.extractSampleRef(clipContent) : null,
          warpMarkers: clipType === 'Audio' ? this.extractWarpMarkers(clipContent) : [],
          trackRef: this.findArrangementTrackForClip(clipContent),
          source: 'arrangement'
        };
        
        console.log(`🎼 Found ${clipType} clip: "${clip.name}" [${clip.currentStart.toFixed(2)}-${clip.currentEnd.toFixed(2)}] (${clip.notes.length} notes)`);
        this.clips.push(clip);
        arrangementClips++;
      }
    });
    
    console.log(`🎼 Total arrangement clips: ${arrangementClips}`);
  }

  parseMIDIClips(xmlData) {
    const clipRegex = /<MidiClip[^>]*Id="([^"]*)"[^>]*>([\s\S]*?)<\/MidiClip>/g;
    let match;
    
    while ((match = clipRegex.exec(xmlData)) !== null) {
      const clipId = match[1];
      const clipContent = match[2];
      
      const clip = {
        id: clipId,
        type: 'MIDI',
        time: this.extractClipTime(clipContent),
        duration: this.extractClipDuration(clipContent),
        name: this.extractClipName(clipContent) || `MIDI Clip ${clipId}`,
        notes: this.extractMIDINotes(clipContent),
        loop: this.extractLoopData(clipContent),
        trackRef: this.findTrackForClip(clipContent)
      };
      
      this.clips.push(clip);
    }
  }
  
  parseAudioClips(xmlData) {
    const clipRegex = /<AudioClip[^>]*Id="([^"]*)"[^>]*>([\s\S]*?)<\/AudioClip>/g;
    let match;
    
    while ((match = clipRegex.exec(xmlData)) !== null) {
      const clipId = match[1];
      const clipContent = match[2];
      
      const clip = {
        id: clipId,
        type: 'Audio',
        time: this.extractClipTime(clipContent),
        duration: this.extractClipDuration(clipContent),
        name: this.extractClipName(clipContent) || `Audio Clip ${clipId}`,
        sampleRef: this.extractSampleRef(clipContent),
        warpMarkers: this.extractWarpMarkers(clipContent),
        loop: this.extractLoopData(clipContent),
        trackRef: this.findTrackForClip(clipContent)
      };
      
      this.clips.push(clip);
    }
  }
  
  parseDevices(xmlData) {
    // Parse instruments and effects
    const deviceTypes = ['Wavetable', 'Operator', 'Impulse', 'Eq8', 'Compressor2', 'Reverb'];
    
    deviceTypes.forEach(deviceType => {
      const regex = new RegExp(`<${deviceType}[^>]*Id="([^"]*)"[^>]*>([\\s\\S]*?)<\/${deviceType}>`, 'g');
      let match;
      
      while ((match = regex.exec(xmlData)) !== null) {
        const deviceId = match[1];
        const deviceContent = match[2];
        
        const device = {
          id: deviceId,
          type: deviceType,
          name: deviceType,
          enabled: this.extractDeviceEnabled(deviceContent),
          parameters: this.extractDeviceParameters(deviceContent)
        };
        
        this.devices.push(device);
      }
    });
  }
  
  // Helper extraction methods
  extractTrackName(content) {
    const nameMatch = content.match(/<UserName Value="([^"]*)"/);
    return nameMatch ? nameMatch[1] : 'Untitled';
  }
  
  extractTrackColor(content) {
    const colorMatch = content.match(/<Color Value="([^"]*)"/);
    if (colorMatch) {
      const colorInt = parseInt(colorMatch[1]);
      return {
        r: (colorInt >> 16) & 255,
        g: (colorInt >> 8) & 255,
        b: colorInt & 255,
        hex: `#${colorInt.toString(16).padStart(6, '0')}`
      };
    }
    return { r: 128, g: 128, b: 128, hex: '#808080' };
  }
  
  extractTrackMuted(content) {
    const mutedMatch = content.match(/<Muted Value="([^"]*)"/);
    return mutedMatch ? mutedMatch[1] === 'true' : false;
  }
  
  extractTrackSolo(content) {
    const soloMatch = content.match(/<Solo Value="([^"]*)"/);
    return soloMatch ? soloMatch[1] === 'true' : false;
  }
  
  extractClipTime(content) {
    const timeMatch = content.match(/<Time Value="([^"]*)"/);
    return timeMatch ? parseFloat(timeMatch[1]) : 0;
  }
  
  extractClipDuration(content) {
    const durationMatch = content.match(/<Duration Value="([^"]*)"/);
    return durationMatch ? parseFloat(durationMatch[1]) : 1;
  }
  
  extractClipName(content) {
    const nameMatch = content.match(/<Name Value="([^"]*)"/);
    return nameMatch ? nameMatch[1] : '';
  }
  
  extractMIDINotes(clipContent) {
    const notes = [];
    
    // Enhanced regex based on reference analysis findings
    // The zzzZWAP analysis showed MidiNoteEvent with these patterns
    const notePatterns = [
      // Pattern 1: Full MidiNoteEvent with all attributes
      /<MidiNoteEvent[^>]*Time="([^"]*)"[^>]*Duration="([^"]*)"[^>]*Velocity="([^"]*)"[^>]*Key="([^"]*)"[^>]*\/>/g,
      // Pattern 2: MidiNoteEvent without Key (will need to infer pitch)
      /<MidiNoteEvent[^>]*Time="([^"]*)"[^>]*Duration="([^"]*)"[^>]*Velocity="([^"]*)"[^>]*\/>/g,
      // Pattern 3: Legacy MidiNote format
      /<MidiNote[^>]+Time="([^"]+)"[^>]+Duration="([^"]+)"[^>]+Velocity="([^"]+)"[^>]+Key="([^"]+)"[^>]*\/>/g
    ];
    
    // Try each pattern
    notePatterns.forEach((pattern, patternIndex) => {
      let noteMatch;
      
      while ((noteMatch = pattern.exec(clipContent)) !== null) {
        const note = {
          time: parseFloat(noteMatch[1]),
          duration: parseFloat(noteMatch[2]), 
          velocity: parseInt(noteMatch[3]),
          pitch: noteMatch[4] ? parseInt(noteMatch[4]) : this.inferPitchFromContext(clipContent, notes.length)
        };
        
        // Validate note data
        if (note.time >= 0 && note.duration > 0 && note.velocity > 0 && note.pitch >= 0 && note.pitch <= 127) {
          notes.push(note);
        }
      }
    });
    
    // Try to extract pitch from KeyTrack if no Key attributes found
    if (notes.length > 0 && notes.every(n => n.pitch === 60)) {
      this.enhanceNotesWithKeyTrack(clipContent, notes);
    }
    
    return notes;
  }
  
  // NEW: Infer pitch from context when Key attribute is missing
  inferPitchFromContext(clipContent, noteIndex) {
    // Look for KeyTrack information
    const keyTrackMatch = clipContent.match(/<KeyTrack[^>]*>([\s\S]*?)<\/KeyTrack>/);
    if (keyTrackMatch) {
      // Try to extract pitch from KeyTrack structure
      const keyMatch = keyTrackMatch[1].match(/<Key Value="([^"]*)"/);
      if (keyMatch) {
        return parseInt(keyMatch[1]);
      }
    }
    
    // Fallback to common drum/instrument mappings based on analysis
    const commonPitches = [36, 38, 42, 46, 49, 51, 60, 62, 64, 67, 69, 72]; // Common drum + melodic pitches
    return commonPitches[noteIndex % commonPitches.length] || 60;
  }
  
  // NEW: Enhance notes with KeyTrack information
  enhanceNotesWithKeyTrack(clipContent, notes) {
    const keyTracks = [];
    const keyTrackRegex = /<KeyTrack[^>]*Id="([^"]*)"[^>]*>([\s\S]*?)<\/KeyTrack>/g;
    let match;
    
    while ((match = keyTrackRegex.exec(clipContent)) !== null) {
      const keyTrackId = match[1];
      const keyTrackContent = match[2];
      
      const keyMatch = keyTrackContent.match(/<Key Value="([^"]*)"/);
      if (keyMatch) {
        keyTracks.push({
          id: keyTrackId,
          pitch: parseInt(keyMatch[1])
        });
      }
    }
    
    // Map notes to key tracks if available
    notes.forEach((note, index) => {
      if (keyTracks[index % keyTracks.length]) {
        note.pitch = keyTracks[index % keyTracks.length].pitch;
      }
    });
  }
  
  extractNotePitch(noteElement) {
    // Try to extract MIDI note number from context or use drum mapping
    // For drum tracks, we'll map based on typical patterns
    return 60; // Default middle C
  }
  
  findArrangementTrackForClip(takeLaneContent) {
    // Find the track ID that this TakeLane belongs to by looking at the track structure
    // This is a simplified approach - may need refinement based on actual XML structure
    const trackMatch = takeLaneContent.match(/TrackId="([^"]*)"/);
    return trackMatch ? trackMatch[1] : null;
  }
  
  extractLoopData(content) {
    const loopMatch = content.match(/<Loop>([\s\S]*?)<\/Loop>/);
    if (loopMatch) {
      const loopContent = loopMatch[1];
      return {
        start: this.extractValueFromContent(loopContent, 'LoopStart'),
        end: this.extractValueFromContent(loopContent, 'LoopEnd'),
        enabled: this.extractValueFromContent(loopContent, 'LoopOn') === 'true'
      };
    }
    return { start: 0, end: 1, enabled: false };
  }
  
  extractSampleRef(content) {
    const sampleMatch = content.match(/<SampleRef>([\s\S]*?)<\/SampleRef>/);
    if (sampleMatch) {
      const nameMatch = sampleMatch[1].match(/<Name Value="([^"]*)"/);
      return nameMatch ? nameMatch[1] : 'Unknown Sample';
    }
    return null;
  }
  
  extractWarpMarkers(content) {
    const markers = [];
    const markerRegex = /<WarpMarker[^>]+SecTime="([^"]+)"[^>]+BeatTime="([^"]+)"[^>]*\/>/g;
    let match;
    
    while ((match = markerRegex.exec(content)) !== null) {
      markers.push({
        secTime: parseFloat(match[1]),
        beatTime: parseFloat(match[2])
      });
    }
    
    return markers;
  }
  
  extractValueFromContent(content, tagName) {
    const match = content.match(new RegExp(`<${tagName} Value="([^"]*)"`, 'i'));
    return match ? match[1] : null;
  }
  
  extractDeviceEnabled(content) {
    const onMatch = content.match(/<On>[\s\S]*?<Manual Value="([^"]*)"[\s\S]*?<\/On>/);
    return onMatch ? onMatch[1] === 'true' : true;
  }
  
  extractDeviceParameters(content) {
    // Simplified parameter extraction
    const params = [];
    const paramRegex = /<Parameter[^>]+Id="([^"]*)"[^>]*>([\s\S]*?)<\/Parameter>/g;
    let match;
    
    while ((match = paramRegex.exec(content)) !== null) {
      params.push({
        id: match[1],
        value: this.extractValueFromContent(match[2], 'Manual') || 0
      });
    }
    
    return params;
  }
  
  findArrangementTrackForClip(clipContent) {
    // Enhanced track finding for arrangement clips
    // Look for track context in surrounding XML structure
    
    // Method 1: Look for TrackId in clip content
    const trackIdMatch = clipContent.match(/<TrackId Value="([^"]*)"/);
    if (trackIdMatch) {
      return trackIdMatch[1];
    }
    
    // Method 2: Look for track references in parent context
    const trackRefMatch = clipContent.match(/TrackIndex="([^"]*)"/);
    if (trackRefMatch) {
      const trackIndex = parseInt(trackRefMatch[1]);
      return this.tracks[trackIndex]?.id || trackIndex;
    }
    
    // Method 3: Infer from clip position or content patterns
    const clipName = this.extractClipName(clipContent).toLowerCase();
    
    // Map common instrument names to track types
    if (clipName.includes('kick') || clipName.includes('drum')) {
      return this.tracks.find(t => t.name.toLowerCase().includes('drum'))?.id || 0;
    }
    if (clipName.includes('bass')) {
      return this.tracks.find(t => t.name.toLowerCase().includes('bass'))?.id || 1;
    }
    if (clipName.includes('lead') || clipName.includes('melody')) {
      return this.tracks.find(t => t.type === 'MidiTrack')?.id || 2;
    }
    
    // Fallback: distribute clips across tracks
    return this.clips.length % Math.max(1, this.tracks.length);
  }
  
  findTrackForClip(clipContent) {
    // Try to find track reference in the clip content
    const trackRefMatch = clipContent.match(/<TrackId Value="([^"]*)"/);
    if (trackRefMatch) {
      return trackRefMatch[1];
    }
    
    // Try to find any ID that might relate to a track
    const idMatch = clipContent.match(/Id="([^"]*)"/);
    if (idMatch) {
      // Check if this ID matches any track ID
      const trackId = this.tracks.find(track => track.id === idMatch[1]);
      if (trackId) return idMatch[1];
    }
    
    // Fallback: assign to tracks in sequence
    const clipIndex = this.clips.length;
    const trackIndex = clipIndex % this.tracks.length;
    return this.tracks[trackIndex]?.id || 0;
  }
  
  extractSceneName(content) {
    const nameMatch = content.match(/<Name Value="([^"]*)"/);
    return nameMatch ? nameMatch[1] : '';
  }
  
  extractSceneTempo(content) {
    const tempoMatch = content.match(/<Tempo Value="([^"]*)"/);
    return tempoMatch ? parseFloat(tempoMatch[1]) : null;
  }
  
  extractSceneTimeSignature(content) {
    const numMatch = content.match(/<TimeSignatureNumerator Value="([^"]*)"/);
    const denMatch = content.match(/<TimeSignatureDenominator Value="([^"]*)"/);
    if (numMatch && denMatch) {
      return {
        numerator: parseInt(numMatch[1]),
        denominator: parseInt(denMatch[1])
      };
    }
    return null;
  }
  
  getTrackColor(type) {
    const colors = {
      MidiTrack: "lime",
      AudioTrack: "cyan", 
      ReturnTrack: "orange",
      GroupTrack: "magenta"
    };
    return colors[type] || "gray";
  }
  
  // Calculate current beat position from audio time
  getCurrentBeat(audioTime) {
    if (!audioTime || !this.tempo || isNaN(audioTime) || isNaN(this.tempo)) {
      return 0;
    }
    const beat = (audioTime * this.tempo) / 60;
    return isNaN(beat) ? 0 : beat;
  }
  
  // Calculate the actual project duration from clips and locators
  calculateProjectDuration() {
    if (this.clips.length === 0 && this.locators.length === 0) {
      console.log("📏 No clips/locators found, using default 64 beats");
      return 64; // Default fallback
    }
    
    let maxBeat = 0;
    
    // Check clips for the furthest endpoint
    this.clips.forEach(clip => {
      // Convert to numbers and handle string numbers properly
      const rawStart = clip.currentStart || clip.time || 0;
      const clipStart = typeof rawStart === 'string' ? parseFloat(rawStart) : rawStart;
      
      let clipDuration;
      if (clip.currentEnd) {
        const rawEnd = typeof clip.currentEnd === 'string' ? parseFloat(clip.currentEnd) : clip.currentEnd;
        clipDuration = rawEnd - clipStart;
      } else {
        const rawDuration = clip.duration || 4;
        clipDuration = typeof rawDuration === 'string' ? parseFloat(rawDuration) : rawDuration;
      }
      
      // Validate inputs after conversion
      if (isNaN(clipStart) || isNaN(clipDuration) || clipStart < 0 || clipDuration <= 0) {
        console.warn(`⚠️ Invalid clip timing for "${clip.name}":`, { 
          clipStart, clipDuration, 
          currentStart: clip.currentStart, 
          currentEnd: clip.currentEnd,
          time: clip.time,
          duration: clip.duration 
        });
        return;
      }
      
      const clipEnd = clipStart + clipDuration;
      maxBeat = Math.max(maxBeat, clipEnd);
      
      // Debug first few clips (less frequently) - with validation
      if (this.clips.indexOf(clip) < 3 && Date.now() % 3000 < 100) {
        const safeClipEnd = (typeof clipEnd === 'number' && !isNaN(clipEnd)) ? clipEnd.toFixed(0).padStart(4, '0') : 'NaN';
        console.log(`📏 Clip "${clip.name}": start=${clipStart}, duration=${clipDuration}, end=${safeClipEnd}`);
      }
    });
    
    // Check locators for structure endpoints  
    this.locators.forEach(locator => {
      maxBeat = Math.max(maxBeat, locator.time || 0);
      // Only show locators occasionally to reduce spam
      if (Date.now() % 5000 < 100) {
        console.log(`📏 Locator "${locator.name}": time=${locator.time}`);
      }
    });
    
    // Add some buffer and round to sensible values
    const bufferedDuration = maxBeat * 1.1; // 10% buffer
    
    // Round to nearest 16 beats for clean display
    const roundedDuration = Math.ceil(bufferedDuration / 16) * 16;
    
    // Ensure minimum reasonable length
    const finalDuration = Math.max(32, roundedDuration);
    
    console.log(`📏 Project duration: maxBeat=${maxBeat.toFixed(1)}, buffered=${bufferedDuration.toFixed(1)}, final=${finalDuration}`);
    return finalDuration;
  }
  
  // Convert ALS time units to beats (improved conversion)
  alsTimeToBeat(alsTime) {
    // ALS uses different time units depending on context
    // This might need adjustment based on actual ALS file structure
    
    // For typical ALS files:
    // - Time values are often in quarter notes already
    // - But sometimes they're in ticks (typically 960 ticks per quarter note)
    
    if (alsTime < 1000) {
      // Likely already in quarter note beats
      return alsTime;
    } else {
      // Likely in ticks - convert (assuming 960 ticks per quarter note)
      return alsTime / 960;
    }
  }
  
  // Convert beats to actual time in seconds
  beatsToSeconds(beats) {
    if (!this.tempo) return beats * 0.5; // Fallback at 120 BPM
    return (beats * 60) / this.tempo;
  }
  
  // Get active clips at a specific beat position
  getActiveClipsAtBeat(beatPosition) {
    return this.clips.filter(clip => {
      const clipStart = clip.time;
      const clipEnd = clip.time + clip.duration;
      
      if (clip.loop && clip.loop.enabled) {
        const loopLength = clip.loop.end - clip.loop.start;
        const relativePosition = (beatPosition - clipStart) % loopLength;
        return beatPosition >= clipStart && relativePosition >= 0 && relativePosition < loopLength;
      } else {
        return beatPosition >= clipStart && beatPosition < clipEnd;
      }
    });
  }
  
  // Get active MIDI notes at a specific beat position
  getActiveNotesAtBeat(beatPosition) {
    const activeNotes = [];
    const activeClips = this.getActiveClipsAtBeat(beatPosition);
    
    activeClips.forEach(clip => {
      if (clip.type === 'MIDI' && clip.notes) {
        clip.notes.forEach(note => {
          const noteStart = clip.time + note.time;
          const noteEnd = noteStart + note.duration;
          
          if (clip.loop && clip.loop.enabled) {
            const loopLength = clip.loop.end - clip.loop.start;
            const relativePosition = (beatPosition - clip.time) % loopLength;
            const relativeNoteStart = note.time;
            const relativeNoteEnd = note.time + note.duration;
            
            if (relativePosition >= relativeNoteStart && relativePosition < relativeNoteEnd) {
              activeNotes.push({
                ...note,
                clipId: clip.id,
                trackRef: clip.trackRef,
                absoluteStart: noteStart,
                absoluteEnd: noteEnd
              });
            }
          } else {
            if (beatPosition >= noteStart && beatPosition < noteEnd) {
              activeNotes.push({
                ...note,
                clipId: clip.id,
                trackRef: clip.trackRef,
                absoluteStart: noteStart,
                absoluteEnd: noteEnd
              });
            }
          }
        });
      }
    });
    
    return activeNotes;
  }
  
  // Generate visual data for current beat position
  generateVisualizationData(beatPosition) {
    const activeClips = this.getActiveClipsAtBeat(beatPosition);
    const activeNotes = this.getActiveNotesAtBeat(beatPosition);
    const harmonicContext = {};
    
    // Calculate track activity levels with enhanced musical analysis
    const trackActivity = this.tracks.map(track => {
      const trackClips = activeClips.filter(clip => clip.trackRef === track.id);
      const trackNotes = activeNotes.filter(note => note.trackRef === track.id);
      
      let activity = 0;
      if (trackClips.length > 0) {
        activity = 0.3; // Base activity for having clips
        
        if (trackNotes.length > 0) {
          // Calculate intensity based on note velocities
          const avgVelocity = trackNotes.reduce((sum, note) => sum + note.velocity, 0) / trackNotes.length;
          activity += (avgVelocity / 127) * 0.7; // Scale velocity to 0-0.7
        }
      }
      
      // Add beat synchronization pulse for active tracks
      const beatPhase = beatPosition % 1;
      const beatPulse = trackNotes.length > 0 ? 
        Math.sin(beatPhase * Math.PI * 2) * 0.15 + 0.85 : 1;
      
      return {
        track,
        activity: Math.min(activity * beatPulse, 1.0),
        clipCount: trackClips.length,
        noteCount: trackNotes.length,
        notes: trackNotes,
        averageVelocity: trackNotes.length > 0 ? 
          trackNotes.reduce((sum, note) => sum + note.velocity, 0) / trackNotes.length : 0,
        pitchRange: this.calculatePitchRange(trackNotes),
        rhythmicDensity: this.calculateTrackRhythmicDensity(trackNotes, beatPosition)
      };
    });
    
    // Analyze harmonic context of all active notes
    const allPitches = activeNotes
      .map(note => this.derivePitchFromNote(note))
      .filter(pitch => pitch !== undefined);
    
    harmonicContext.pitches = [...new Set(allPitches)].sort((a, b) => a - b);
    harmonicContext.rootNote = this.findRootNote(harmonicContext.pitches);
    harmonicContext.scale = this.analyzeScale(harmonicContext.pitches);
    harmonicContext.chord = this.analyzeChord(harmonicContext.pitches);
    harmonicContext.tension = this.calculateHarmonicTension(harmonicContext.pitches);
    
    return {
      beatPosition,
      activeClips,
      activeNotes: activeNotes.slice(0, 16), // Limit for notation display
      trackActivity,
      harmonicContext,
      tempo: this.tempo,
      timeSignature: this.timeSignature,
      globalIntensity: this.calculateGlobalIntensity(trackActivity),
      measurePosition: beatPosition % (this.timeSignature.numerator || 4)
    };
  }
  
  // Derive MIDI pitch from note data (enhanced)
  derivePitchFromNote(note, baseOctave = 4) {
    if (note.pitch !== undefined) return note.pitch;
    
    // For ALS files, pitch often comes from KeyTrack structure
    // This is a fallback estimation
    const basePitch = 60 + (baseOctave - 4) * 12; // C in specified octave
    const variation = Math.floor(Math.random() * 25) - 12; // ± 1 octave
    return Math.max(0, Math.min(127, basePitch + variation));
  }
  
  // Calculate pitch range for a track
  calculatePitchRange(notes) {
    if (notes.length === 0) return { min: 60, max: 60, span: 0 };
    
    const pitches = notes.map(note => this.derivePitchFromNote(note));
    const min = Math.min(...pitches);
    const max = Math.max(...pitches);
    
    return { min, max, span: max - min };
  }
  
  // Calculate rhythmic density for a track
  calculateTrackRhythmicDensity(notes, beatPosition) {
    const windowSize = 1.0; // One beat window
    const windowStart = beatPosition - windowSize / 2;
    const windowEnd = beatPosition + windowSize / 2;
    
    const notesInWindow = notes.filter(note => {
      const noteStart = note.absoluteStart || note.time;
      return noteStart >= windowStart && noteStart <= windowEnd;
    });
    
    return Math.min(1, notesInWindow.length / 6); // Normalize
  }
  
  // Find harmonic root note
  findRootNote(pitches) {
    if (pitches.length === 0) return 60; // Default C4
    
    // Use lowest note as root (bass note principle)
    return Math.min(...pitches);
  }
  
  // Analyze musical scale
  analyzeScale(pitches) {
    if (pitches.length < 3) return 'Monophonic';
    
    const root = pitches[0];
    const intervals = pitches.map(p => (p - root) % 12).slice(1);
    
    // Check for major scale intervals
    const majorIntervals = [2, 4, 5, 7, 9, 11];
    const minorIntervals = [2, 3, 5, 7, 8, 10];
    const dorianIntervals = [2, 3, 5, 7, 9, 10];
    
    const majorMatch = intervals.filter(i => majorIntervals.includes(i)).length;
    const minorMatch = intervals.filter(i => minorIntervals.includes(i)).length;
    const dorianMatch = intervals.filter(i => dorianIntervals.includes(i)).length;
    
    if (majorMatch >= minorMatch && majorMatch >= dorianMatch) return 'Major';
    if (minorMatch >= dorianMatch) return 'Minor';
    if (dorianMatch > 0) return 'Modal';
    
    return 'Chromatic';
  }
  
  // Analyze chord structure
  analyzeChord(pitches) {
    if (pitches.length < 2) return 'Unison';
    if (pitches.length === 2) return 'Interval';
    
    const root = pitches[0];
    const intervals = pitches.map(p => (p - root) % 12).sort((a, b) => a - b);
    
    // Common chord types
    if (intervals.includes(4) && intervals.includes(7)) return 'Major Triad';
    if (intervals.includes(3) && intervals.includes(7)) return 'Minor Triad';
    if (intervals.includes(4) && intervals.includes(7) && intervals.includes(10)) return 'Dominant 7th';
    if (intervals.includes(3) && intervals.includes(7) && intervals.includes(10)) return 'Minor 7th';
    if (intervals.includes(4) && intervals.includes(7) && intervals.includes(11)) return 'Major 7th';
    if (intervals.includes(3) && intervals.includes(6)) return 'Diminished';
    if (intervals.includes(4) && intervals.includes(8)) return 'Augmented';
    if (intervals.includes(5) && intervals.includes(7)) return 'Sus4';
    if (intervals.includes(2) && intervals.includes(7)) return 'Sus2';
    
    return `Complex (${pitches.length} notes)`;
  }
  
  // Calculate harmonic tension
  calculateHarmonicTension(pitches) {
    if (pitches.length < 2) return 0;
    
    let tension = 0;
    const intervals = [];
    
    // Calculate all intervals between notes
    for (let i = 0; i < pitches.length; i++) {
      for (let j = i + 1; j < pitches.length; j++) {
        const interval = (pitches[j] - pitches[i]) % 12;
        intervals.push(interval);
      }
    }
    
    // Weight intervals by their dissonance
    const dissonanceWeights = {
      0: 0,    // Unison
      1: 0.9,  // Minor 2nd (very tense)
      2: 0.4,  // Major 2nd
      3: 0.2,  // Minor 3rd
      4: 0.1,  // Major 3rd (consonant)
      5: 0.3,  // Perfect 4th
      6: 0.8,  // Tritone (very tense)
      7: 0,    // Perfect 5th (consonant)
      8: 0.2,  // Minor 6th
      9: 0.1,  // Major 6th
      10: 0.4, // Minor 7th
      11: 0.6  // Major 7th
    };
    
    intervals.forEach(interval => {
      tension += dissonanceWeights[interval] || 0.5;
    });
    
    return Math.min(1, tension / intervals.length);
  }
  
  // Calculate global intensity
  calculateGlobalIntensity(trackActivity) {
    if (trackActivity.length === 0) return 0;
    
    const totalActivity = trackActivity.reduce((sum, track) => sum + track.activity, 0);
    const avgActivity = totalActivity / trackActivity.length;
    
    // Weight by number of active tracks
    const activeTrackCount = trackActivity.filter(track => track.activity > 0.1).length;
    const densityFactor = Math.min(1, activeTrackCount / 4);
    
    return Math.min(1, avgActivity * (0.7 + densityFactor * 0.3));
  }
  
  /*
  // COMMENTED OUT - Original timeline visualizations
  // Real-time activity visualization - small moving boxes showing all clips
  drawTimeBasedVisualization(ink, screen, currentTime = 0, isPlaying = false) {
    if (!currentTime || this.tracks.length === 0) return;
    
    const beatPosition = this.getCurrentBeat(currentTime);
    
    // Much larger activity display - use most of the screen
    const activityHeight = screen.height - 60; // Use almost full screen height
    const activityY = 40; // Start near top
    
    // Dark background
    ink(5, 5, 8).box(0, activityY, screen.width, activityHeight);
    
    // Minimal status line at top
    const playStatusText = isPlaying ? "▶" : "⏸";
    const currentMeasure = Math.floor(beatPosition / this.timeSignature.numerator) + 1;
    const beatInMeasure = Math.floor((beatPosition % this.timeSignature.numerator)) + 1;
    
    ink(isPlaying ? [120, 255, 120] : [255, 200, 80]).write(playStatusText, 
      { x: 10, y: 10, font: "MatrixChunky8" });
    ink(180, 180, 220).write(`${currentMeasure}.${beatInMeasure}`, 
      { x: 25, y: 10, font: "MatrixChunky8" });
    ink(160, 160, 180).write(`${this.tempo}`, 
      { x: 65, y: 10, font: "MatrixChunky8" });
    
    // SIMPLE FLOWING TIMELINE - much larger window
    const timeWindow = 64; // Massive window - 64 beats of context
    const timeOffset = beatPosition - 16; // Show 16 beats behind, 48 ahead
    
    // Get all unique tracks for stable lane assignment
    const allTracks = new Set();
    this.clips.forEach(clip => {
      const trackRef = clip.trackRef || clip.trackIndex || `track_${clip.id}`;
      allTracks.add(trackRef);
    });
    
    const trackList = Array.from(allTracks).sort();
    const minTrackHeight = 20; // Much larger minimum height
    const trackHeight = Math.max(minTrackHeight, Math.floor(activityHeight / Math.max(trackList.length, 1)));
    const actualTracksShown = Math.min(trackList.length, Math.floor(activityHeight / minTrackHeight));
    
    // Extended time grid - show much more structure
    const pixelsPerBeat = screen.width / timeWindow;
    
    for (let beat = Math.floor(timeOffset); beat <= Math.ceil(timeOffset + timeWindow); beat++) {
      const x = (beat - timeOffset) * pixelsPerBeat;
      if (x >= 0 && x <= screen.width) {
        const isDownbeat = beat % 4 === 0;
        const is16Beat = beat % 16 === 0;
        
        if (is16Beat && beat >= 0) {
          // Major grid lines every 16 beats
          ink(40, 40, 50).line(x, activityY, x, activityY + activityHeight);
          if (beat % 32 === 0) {
            ink(80, 80, 100).write(`${beat}`, 
              { x: x + 2, y: activityY - 12, font: "MatrixChunky8" });
          }
        } else if (isDownbeat) {
          // Minor grid lines every 4 beats
          ink(20, 20, 25).line(x, activityY, x, activityY + activityHeight);
        }
      }
    }
    
    // Draw all track activity with huge search window
    trackList.slice(0, actualTracksShown).forEach((trackRef, trackIndex) => {
      const trackY = activityY + trackIndex * trackHeight;
      const laneHeight = Math.max(16, trackHeight - 4); // Much larger lane height
      
      // Track lane background
      if (trackIndex % 2 === 0) {
        ink(12, 12, 16).box(0, trackY, screen.width, laneHeight);
      }
      
      // Track label with plenty of space
      const track = this.tracks.find(t => t.id === trackRef || t.trackIndex === trackRef);
      let trackName = track?.name?.substring(0, 12) || `Track ${trackIndex + 1}`;
      
      ink(100, 100, 120).write(trackName, {
        x: 5, 
        y: trackY + 2, 
        font: "MatrixChunky8"
      });
      
      // Get clips for this track with massive search window
      const trackClips = this.clips.filter(clip => {
        const clipTrackRef = clip.trackRef || clip.trackIndex || `track_${clip.id}`;
        return clipTrackRef === trackRef;
      });
      
      // Ultra-extended clip search - look WAY further ahead and behind
      const visibleTrackClips = trackClips.filter(clip => {
        const clipStart = (clip.time || 0) / 4;
        const clipEnd = clipStart + ((clip.duration || 1) / 4);
        return clipEnd >= timeOffset && clipStart <= timeOffset + timeWindow;
      });
      
      // Debug: Show total clips found for this track
      if (trackClips.length > 0) {
        ink(60, 80, 100).write(`(${trackClips.length})`, 
          { x: 5, y: trackY + 12, font: "MatrixChunky8" });
      }
      
      // Draw all clips for this track with better visibility
      visibleTrackClips.forEach(clip => {
        const clipStart = (clip.time || 0) / 4;
        const clipDuration = (clip.duration || 1) / 4;
        const clipEnd = clipStart + clipDuration;
        
        const startX = (clipStart - timeOffset) * pixelsPerBeat;
        const endX = (clipEnd - timeOffset) * pixelsPerBeat;
        const clipWidth = Math.max(3, endX - startX); // Minimum visible width
        
        // Enhanced activity detection
        const isActive = beatPosition >= clipStart && beatPosition < clipEnd;
        
        // Enhanced color coding with better contrast
        let clipColor;
        const clipName = (clip.name || '').toLowerCase();
        
        if (clipName.includes('kick')) {
          clipColor = isActive ? [255, 100, 100] : [100, 50, 50];
        } else if (clipName.includes('snare')) {
          clipColor = isActive ? [100, 255, 100] : [50, 100, 50];
        } else if (clipName.includes('hat')) {
          clipColor = isActive ? [100, 150, 255] : [50, 75, 100];
        } else if (clipName.includes('bass')) {
          clipColor = isActive ? [255, 255, 100] : [100, 100, 50];
        } else if (clip.type === 'MIDI') {
          clipColor = isActive ? [255, 200, 100] : [100, 80, 50];
        } else {
          clipColor = isActive ? [200, 150, 255] : [80, 60, 100];
        }
        
        // Draw clip with enhanced visibility
        if (startX < screen.width && endX > 0) {
          const clampedStartX = Math.max(0, startX);
          const clampedWidth = Math.min(clipWidth, screen.width - clampedStartX);
          
          // Main clip rectangle with better height
          const clipBoxHeight = Math.max(8, laneHeight - 8);
          ink(...clipColor).box(clampedStartX, trackY + 4, clampedWidth, clipBoxHeight);
          
          // Active clip gets outline and name
          if (isActive) {
            ink(255, 255, 255).rect(clampedStartX, trackY + 4, clampedWidth, clipBoxHeight);
            
            // Show clip name if there's space
            if (clipWidth > 40) {
              const shortName = (clip.name || 'CLIP').substring(0, 8);
              ink(255, 255, 255).write(shortName, 
                { x: clampedStartX + 2, y: trackY + 6, font: "MatrixChunky8" });
            }
          }
          
          // Enhanced MIDI note visualization
          if (clip.type === 'MIDI' && clip.notes && clip.notes.length > 0) {
            clip.notes.forEach(note => {
              const noteTime = clipStart + (note.time || 0) / 4;
              const noteX = (noteTime - timeOffset) * pixelsPerBeat;
              
              if (noteX >= 0 && noteX <= screen.width) {
                const noteEnd = noteTime + (note.duration || 0.1) / 4;
                const noteActive = beatPosition >= noteTime && beatPosition < noteEnd;
                
                if (noteActive) {
                  const pitch = note.pitch || 60;
                  const velocity = (note.velocity || 100) / 127;
                  const noteY = trackY + 4 + Math.floor((pitch % 24) / 24 * clipBoxHeight);
                  
                  // Larger, more visible note indicators
                  ink(255, 255 * velocity, 150).circle(noteX, noteY, 4);
                }
              }
            });
          }
        }
      });
      
      // Track activity indicator with better visibility
      const hasActiveClips = trackClips.some(clip => {
        const clipStart = (clip.time || 0) / 4;
        const clipEnd = clipStart + ((clip.duration || 1) / 4);
        return beatPosition >= clipStart && beatPosition < clipEnd;
      });
      
      if (hasActiveClips) {
        ink(255, 150, 150).circle(screen.width - 15, trackY + Math.floor(laneHeight / 2), 5);
      }
    });
    
    // Playhead with enhanced visibility
    const playheadX = (beatPosition - timeOffset) * pixelsPerBeat;
    if (playheadX >= 0 && playheadX <= screen.width) {
      ink(255, 255, 100).line(playheadX, activityY - 5, playheadX, activityY + activityHeight + 5);
      ink(255, 200, 100).circle(playheadX, activityY - 8, 3);
    }
    
    // Enhanced status info showing project coverage
    const maxBeat = this.calculateProjectDuration();
    const totalActive = this.getActiveClipsAtTime(currentTime).length;
    const totalMidiNotes = this.getActiveMidiNotesAtTime(currentTime).length;
    
    ink(255, 255, 150).write(`Beat: ${beatPosition.toFixed(2)} / ${maxBeat.toFixed(0)}`, 
      { x: 100, y: 10, font: "MatrixChunky8" });
    
    ink(150, 255, 255).write(`Window: ${timeOffset.toFixed(0)}-${(timeOffset + timeWindow).toFixed(0)}`, 
      { x: 250, y: 10, font: "MatrixChunky8" });
    
    ink(255, 150, 255).write(`Active: ${totalActive} clips, ${totalMidiNotes} notes`, 
      { x: 400, y: 10, font: "MatrixChunky8" });
    
    // Show total tracks for debugging
    ink(150, 150, 255).write(`Tracks: ${trackList.length}`, 
      { x: screen.width - 100, y: 10, font: "MatrixChunky8" });

    // Beat pulse
    const beatPhase = beatPosition % 1;
    const pulseSize = beatPhase < 0.1 ? 4 : 2;
    ink(255, 255 - beatPhase * 200, 100).circle(screen.width - 15, 15, pulseSize);
  }
  
  // Ultra-detailed activity timeline - MIDI notes, automation, and all track activity
  drawDetailedActivityTimeline(ink, screen, currentTime, startY, height) {
    const currentBeat = this.getCurrentBeat(currentTime);
    
    // Much extended timeline parameters - show way more content
    const timeWindow = 32; // Show 32 beats (was 16) - even more context
    const pixelsPerBeat = (screen.width - 100) / timeWindow; // More space for labels
    const playheadX = 80; // Move playhead right for label space
    const timeOffset = currentBeat - 8; // Show 8 beats behind, 24 ahead
    
    // Background
    ink(15, 15, 18).box(80, startY, screen.width - 100, height);
    
    // Create individual track rows - NO COMBINING!
    const allTracks = new Set();
    this.clips.forEach(clip => {
      const trackRef = clip.trackRef || clip.trackIndex || `track_${clip.id}`;
      allTracks.add(trackRef);
    });
    
    // Convert to sorted array for stable ordering
    const trackList = Array.from(allTracks).sort();
    const minTrackHeight = 12; // Larger minimum height for better visibility
    const trackHeight = Math.max(minTrackHeight, Math.floor(height / Math.max(trackList.length, 1)));
    const actualTracksShown = Math.min(trackList.length, Math.floor(height / minTrackHeight));
    
    // Extended beat grid - show more structure
    for (let beat = Math.floor(timeOffset); beat <= Math.ceil(timeOffset + timeWindow); beat++) {
      const x = 80 + (beat - timeOffset) * pixelsPerBeat;
      if (x >= 80 && x <= screen.width - 20) {
        const isDownbeat = beat % 4 === 0;
        const is16Beat = beat % 16 === 0;
        
        if (is16Beat && beat >= 0) {
          // Major grid lines every 16 beats
          ink(60, 60, 80).line(x, startY, x, startY + height);
          ink(80, 80, 120).write(`${beat}`, 
            { x: x + 1, y: startY - 10, font: "MatrixChunky8" });
        } else if (isDownbeat) {
          // Minor grid lines every 4 beats
          ink(40, 40, 50).line(x, startY, x, startY + height);
          ink(60, 60, 80).write(`${beat}`, 
            { x: x + 1, y: startY - 8, font: "MatrixChunky8" });
        }
      }
    }
    
    // Fixed playhead with beat sync validation
    ink(255, 200, 100).line(playheadX, startY - 3, playheadX, startY + height + 3);
    
    // Debug: Show current timing info for A/V sync validation
    ink(255, 255, 100).write(`Beat: ${currentBeat.toFixed(2)}`, 
      { x: 5, y: startY - 15, font: "MatrixChunky8" });
    ink(100, 255, 255).write(`Time: ${currentTime.toFixed(2)}s`, 
      { x: 5, y: startY - 5, font: "MatrixChunky8" });
    
    // Show total project info for debugging
    const maxBeat = this.calculateProjectDuration();
    ink(255, 150, 100).write(`Max: ${maxBeat.toFixed(0)}`, 
      { x: screen.width - 80, y: startY - 15, font: "MatrixChunky8" });
    
    // Draw each track on its own dedicated row with larger spacing
    trackList.slice(0, actualTracksShown).forEach((trackRef, trackIndex) => {
      const trackY = startY + trackIndex * trackHeight;
      const rowHeight = Math.max(8, trackHeight - 4); // Larger rows for better visibility
      
      // Track background
      ink(8, 8, 12).box(80, trackY, screen.width - 100, rowHeight);
      
      // Track label with better spacing - no overlap
      const track = this.tracks.find(t => t.id === trackRef || t.trackIndex === trackRef);
      let trackName = track?.name?.substring(0, 8) || `T${trackIndex + 1}`;
      
      // Ensure label doesn't overlap by using consistent spacing
      const labelY = trackY + Math.floor(rowHeight / 2) - 3;
      ink(120, 120, 140).write(trackName, {
        x: 5, 
        y: labelY, 
        font: "MatrixChunky8"
      });
      
      // Get clips for this specific track only
      const trackClips = this.clips.filter(clip => {
        const clipTrackRef = clip.trackRef || clip.trackIndex || `track_${clip.id}`;
        return clipTrackRef === trackRef;
      });
      
      // Much more extended clip search - look way further ahead
      const visibleTrackClips = trackClips.filter(clip => {
        const clipStart = (clip.time || 0) / 4;
        const clipEnd = clipStart + ((clip.duration || 1) / 4);
        return clipEnd >= timeOffset && clipStart <= timeOffset + timeWindow;
      });
      
      // Debug: Show clip count for this track
      if (trackClips.length > 0) {
        ink(60, 80, 100).write(`(${trackClips.length})`, 
          { x: 5, y: labelY + 8, font: "MatrixChunky8" });
      }
      
      // Draw clips for this track
      visibleTrackClips.forEach(clip => {
        const clipStart = (clip.time || 0) / 4;
        const clipDuration = (clip.duration || 1) / 4;
        const clipEnd = clipStart + clipDuration;
        
        const startX = 80 + (clipStart - timeOffset) * pixelsPerBeat;
        const endX = 80 + (clipEnd - timeOffset) * pixelsPerBeat;
        const clipWidth = Math.max(2, endX - startX);
        
        // A/V Sync check - validate timing
        const isActive = currentBeat >= clipStart && currentBeat < clipEnd;
        
        // Clip color based on type and content
        let clipColor;
        const clipName = (clip.name || '').toLowerCase();
        
        if (clipName.includes('kick')) {
          clipColor = isActive ? [255, 80, 80] : [120, 40, 40];
        } else if (clipName.includes('snare')) {
          clipColor = isActive ? [80, 255, 80] : [40, 120, 40];
        } else if (clipName.includes('hat')) {
          clipColor = isActive ? [80, 120, 255] : [40, 60, 120];
        } else if (clipName.includes('bass')) {
          clipColor = isActive ? [255, 255, 80] : [120, 120, 40];
        } else if (clip.type === 'MIDI') {
          clipColor = isActive ? [255, 180, 80] : [120, 90, 40];
        } else {
          clipColor = isActive ? [180, 120, 255] : [90, 60, 120];
        }
        
        // Clip box
        if (startX < screen.width - 20 && endX > 80) {
          const clampedStartX = Math.max(80, startX);
          const clampedWidth = Math.min(clipWidth, screen.width - 20 - clampedStartX);
          
          ink(...clipColor).box(clampedStartX, trackY + 2, clampedWidth, rowHeight - 4);
          
          // Show clip name if there's space and it's active (for debugging sync)
          if (isActive && clipWidth > 30) {
            const shortName = (clip.name || 'CLIP').substring(0, 6);
            ink(255, 255, 255).write(shortName, 
              { x: clampedStartX + 1, y: trackY + 3, font: "MatrixChunky8" });
          }
          
          // MIDI note visualization - show individual notes as larger dots
          if (clip.type === 'MIDI' && clip.notes && clip.notes.length > 0 && isActive) {
            clip.notes.forEach(note => {
              const noteTime = clipStart + (note.time || 0) / 4;
              const noteX = 80 + (noteTime - timeOffset) * pixelsPerBeat;
              
              if (noteX >= 80 && noteX <= screen.width - 20) {
                // Note activity - check if note is currently playing
                const noteEnd = noteTime + (note.duration || 0.1) / 4;
                const noteActive = currentBeat >= noteTime && currentBeat < noteEnd;
                
                if (noteActive) {
                  // Active note - larger bright dot
                  const pitch = note.pitch || 60;
                  const velocity = (note.velocity || 100) / 127;
                  const noteY = trackY + 2 + Math.floor((pitch % 12) / 12 * (rowHeight - 4));
                  ink(255, 255 * velocity, 100).circle(noteX, noteY, 3);
                }
              }
            });
          }
        }
      });
      
      // Track activity indicator - show if track has any active content
      const hasActiveClips = trackClips.some(clip => {
        const clipStart = (clip.time || 0) / 4;
        const clipEnd = clipStart + ((clip.duration || 1) / 4);
        return currentBeat >= clipStart && currentBeat < clipEnd;
      });
      
      if (hasActiveClips) {
        ink(255, 100, 100).circle(75, trackY + Math.floor(rowHeight / 2), 3);
      }
    });
    
    // Activity summary with sync info
    const totalActive = this.getActiveClipsAtTime(currentTime).length;
    const totalMidiNotes = this.getActiveMidiNotesAtTime(currentTime).length;
    
    ink(100, 200, 255).write(`${totalActive} clips`, 
      { x: 5, y: startY + height - 18, font: "MatrixChunky8" });
    
    if (totalMidiNotes > 0) {
      ink(255, 255, 100).write(`${totalMidiNotes} notes`, 
        { x: 5, y: startY + height - 8, font: "MatrixChunky8" });
    }
    
    // Show time window for debugging - with max project beat
    ink(80, 80, 100).write(`${timeOffset.toFixed(1)}-${(timeOffset + timeWindow).toFixed(1)} / ${maxBeat.toFixed(0)}`, 
      { x: screen.width - 120, y: startY + height - 8, font: "MatrixChunky8" });
  }
  
  // Get currently active MIDI notes
  getActiveMidiNotesAtTime(currentTime) {
    const currentBeat = this.getCurrentBeat(currentTime);
    const activeNotes = [];
    
    this.clips.forEach(clip => {
      if (clip.type === 'MIDI' && clip.notes) {
        const clipStart = (clip.time || 0) / 4;
        const clipEnd = clipStart + ((clip.duration || 1) / 4);
        
        if (currentBeat >= clipStart && currentBeat < clipEnd) {
          clip.notes.forEach(note => {
            const noteStart = clipStart + (note.time || 0) / 4;
            const noteEnd = noteStart + (note.duration || 0.1) / 4;
            
            if (currentBeat >= noteStart && currentBeat < noteEnd) {
              activeNotes.push({
                ...note,
                clip: clip,
                absoluteStart: noteStart,
                absoluteEnd: noteEnd
              });
            }
          });
        }
      }
    });
    
    return activeNotes;
  }
  
  // Show current activity with clear visual indicators - what's happening NOW
  drawCurrentActivityIndicators(ink, screen, currentTime, startY, availableHeight) {
    const activeClips = this.getActiveClipsAtTime(currentTime);
    const currentBeat = this.getCurrentBeat(currentTime);
    
    // Header
    ink(160, 160, 180).write("ACTIVE NOW:", 
      { x: 15, y: startY + 5, font: "MatrixChunky8" });
    
    if (activeClips.length === 0) {
      ink(120, 120, 120).write("(silence)", 
        { x: 120, y: startY + 5, font: "MatrixChunky8" });
      return;
    }
    
    // Group active clips by type for cleaner display
    const groupedClips = {
      kick: activeClips.filter(c => (c.name || '').toLowerCase().includes('kick')),
      snare: activeClips.filter(c => (c.name || '').toLowerCase().includes('snare')),
      hat: activeClips.filter(c => (c.name || '').toLowerCase().includes('hat')),
      bass: activeClips.filter(c => (c.name || '').toLowerCase().includes('bass')),
      other: activeClips.filter(c => {
        const name = (c.name || '').toLowerCase();
        return !name.includes('kick') && !name.includes('snare') && 
               !name.includes('hat') && !name.includes('bass');
      })
    };
    
    // Display active groups as colored activity bars
    let yPos = startY + 25;
    const barHeight = 12;
    const barSpacing = 16;
    
    Object.entries(groupedClips).forEach(([type, clips]) => {
      if (clips.length === 0) return;
      
      // Type colors
      let typeColor;
      switch(type) {
        case 'kick': typeColor = [255, 100, 100]; break;
        case 'snare': typeColor = [100, 255, 100]; break;
        case 'hat': typeColor = [100, 150, 255]; break;
        case 'bass': typeColor = [255, 255, 100]; break;
        default: typeColor = [200, 150, 255]; break;
      }
      
      // Activity intensity based on how many clips and their position in time
      const intensity = Math.min(1, clips.length / 2);
      const pulsePhase = (currentBeat * 4) % 1; // Pulse 4x per beat
      const pulseIntensity = 0.7 + Math.sin(pulsePhase * Math.PI * 2) * 0.3;
      
      const finalColor = typeColor.map(c => Math.floor(c * intensity * pulseIntensity));
      
      // Activity bar showing current intensity
      const barWidth = Math.floor(150 * intensity);
      ink(...finalColor).box(90, yPos, barWidth, barHeight);
      
      // Type label
      const typeLabel = type.toUpperCase();
      ink(220, 220, 220).write(typeLabel, 
        { x: 15, y: yPos + 2, font: "MatrixChunky8" });
      
      // Clip count
      if (clips.length > 1) {
        ink(180, 180, 180).write(`×${clips.length}`, 
          { x: 250, y: yPos + 2, font: "MatrixChunky8" });
      }
      
      yPos += barSpacing;
    });
    
    // Show start/stop events happening soon
    this.drawUpcomingEvents(ink, screen, currentTime, startY + availableHeight - 35, 30);
  }
  
  // Show upcoming start/stop events in next few beats
  drawUpcomingEvents(ink, screen, currentTime, startY, height) {
    const currentBeat = this.getCurrentBeat(currentTime);
    const lookAheadBeats = 2; // Look 2 beats ahead
    
    const upcomingEvents = [];
    
    // Find clips starting or ending soon
    this.clips.forEach(clip => {
      const clipStart = (clip.time || 0) / 4;
      const clipEnd = clipStart + ((clip.duration || 1) / 4);
      
      // Check for starts
      if (clipStart > currentBeat && clipStart <= currentBeat + lookAheadBeats) {
        upcomingEvents.push({
          type: 'start',
          beat: clipStart,
          clip: clip,
          time: clipStart - currentBeat
        });
      }
      
      // Check for ends
      if (clipEnd > currentBeat && clipEnd <= currentBeat + lookAheadBeats) {
        upcomingEvents.push({
          type: 'end',
          beat: clipEnd,
          clip: clip,
          time: clipEnd - currentBeat
        });
      }
    });
    
    if (upcomingEvents.length === 0) return;
    
    // Sort by time
    upcomingEvents.sort((a, b) => a.time - b.time);
    
    // Header
    ink(140, 140, 160).write("UPCOMING:", 
      { x: 15, y: startY + 2, font: "MatrixChunky8" });
    
    // Show first few upcoming events
    upcomingEvents.slice(0, 3).forEach((event, index) => {
      const xPos = 120 + index * 80;
      const eventColor = event.type === 'start' ? [120, 255, 120] : [255, 120, 120];
      const symbol = event.type === 'start' ? '▶' : '◼';
      const timeStr = `${event.time.toFixed(1)}b`;
      
      ink(...eventColor).write(`${symbol}${timeStr}`, 
        { x: xPos, y: startY + 2, font: "MatrixChunky8" });
    });
  }
  */
  
  // NEW MINIMAL DRUM VISUALIZATION - Focus only on snare, hihat, bass kick for time sync checking
  drawMinimalDrumVisualization(ink, screen, currentTime = 0, isPlaying = false) {
    if (!currentTime || this.tracks.length === 0) return;
    
    const currentBeat = this.getCurrentBeat(currentTime);
    
    // Simple header
    const headerY = 60;
    ink(180, 180, 200).write(`DRUM SYNC CHECK | Beat: ${currentBeat.toFixed(2)} | ${this.tempo}bpm`, 
      { x: 20, y: headerY, font: "MatrixChunky8" });
    
    // Main visualization area
    const vizY = 100;
    const vizHeight = 200;
    ink(10, 10, 15).box(20, vizY, screen.width - 40, vizHeight);
    
    // Get only kick, snare, and hihat clips
    const drumClips = this.clips.filter(clip => {
      const name = (clip.name || '').toLowerCase();
      return name.includes('kick') || name.includes('snare') || name.includes('hat');
    });
    
    // Three lanes: Kick, Snare, HiHat
    const laneHeight = Math.floor(vizHeight / 3);
    const laneWidth = screen.width - 80;
    
    // Draw lane labels
    ink(255, 100, 100).write('KICK', { x: 25, y: vizY + 15, font: "MatrixChunky8" });
    ink(100, 255, 100).write('SNARE', { x: 25, y: vizY + laneHeight + 15, font: "MatrixChunky8" });
    ink(100, 150, 255).write('HIHAT', { x: 25, y: vizY + 2 * laneHeight + 15, font: "MatrixChunky8" });
    
    // Time window - show 8 beats around current position
    const timeWindow = 8;
    const timeOffset = currentBeat - 2; // Show 2 beats behind, 6 ahead
    const pixelsPerBeat = laneWidth / timeWindow;
    const laneStartX = 80;
    
    // Draw beat grid
    for (let beat = Math.floor(timeOffset); beat <= Math.ceil(timeOffset + timeWindow); beat++) {
      const x = laneStartX + (beat - timeOffset) * pixelsPerBeat;
      if (x >= laneStartX && x <= laneStartX + laneWidth) {
        const isDownbeat = beat % 4 === 0;
        const gridAlpha = isDownbeat ? 0.6 : 0.3;
        ink(80, 80, 100, gridAlpha).line(x, vizY, x, vizY + vizHeight);
        
        if (isDownbeat && beat >= 0) {
          ink(120, 120, 140).write(beat.toString(), 
            { x: x + 2, y: vizY - 15, font: "MatrixChunky8" });
        }
      }
    }
    
    // Draw clips for each drum type
    ['kick', 'snare', 'hat'].forEach((drumType, laneIndex) => {
      const laneY = vizY + laneIndex * laneHeight;
      const typedClips = drumClips.filter(clip => 
        (clip.name || '').toLowerCase().includes(drumType));
      
      typedClips.forEach(clip => {
        const clipStart = (clip.time || 0) / 4;
        const clipDuration = (clip.duration || 1) / 4;
        const clipEnd = clipStart + clipDuration;
        
        // Only draw if visible in time window
        if (clipEnd >= timeOffset && clipStart <= timeOffset + timeWindow) {
          const startX = laneStartX + Math.max(0, (clipStart - timeOffset) * pixelsPerBeat);
          const endX = laneStartX + Math.min(laneWidth, (clipEnd - timeOffset) * pixelsPerBeat);
          const width = Math.max(2, endX - startX);
          
          const isActive = currentBeat >= clipStart && currentBeat < clipEnd;
          
          // Color coding
          let color;
          if (drumType === 'kick') color = isActive ? [255, 150, 150] : [120, 60, 60];
          else if (drumType === 'snare') color = isActive ? [150, 255, 150] : [60, 120, 60];
          else color = isActive ? [150, 180, 255] : [60, 80, 120];
          
          // Draw clip
          const clipHeight = laneHeight - 10;
          ink(...color).box(startX, laneY + 5, width, clipHeight);
          
          // Active clip effects
          if (isActive) {
            // Bright outline
            ink(255, 255, 255).rect(startX, laneY + 5, width, clipHeight);
            
            // Beat flash effect
            const beatPhase = currentBeat % 1;
            if (beatPhase < 0.1) {
              const flashIntensity = (1 - beatPhase * 10) * 0.8;
              ink(255, 255, 255, flashIntensity).box(startX - 2, laneY + 3, width + 4, clipHeight + 4);
            }
            
            // Show clip name
            if (width > 30) {
              ink(0, 0, 0).write(clip.name.substring(0, 8), 
                { x: startX + 2, y: laneY + 8, font: "MatrixChunky8" });
            }
          }
        }
      });
      
      // Draw activity indicator for this lane
      const hasActive = typedClips.some(clip => {
        const clipStart = (clip.time || 0) / 4;
        const clipEnd = clipStart + ((clip.duration || 1) / 4);
        return currentBeat >= clipStart && currentBeat < clipEnd;
      });
      
      if (hasActive) {
        // Large pulsing indicator
        const pulse = Math.sin(currentBeat * 8) * 0.3 + 0.7;
        let indicatorColor;
        if (drumType === 'kick') indicatorColor = [255, 100, 100];
        else if (drumType === 'snare') indicatorColor = [100, 255, 100];
        else indicatorColor = [100, 150, 255];
        
        const finalColor = indicatorColor.map(c => Math.floor(c * pulse));
        ink(...finalColor).circle(laneStartX - 15, laneY + laneHeight/2, 8);
      }
    });
    
    // Playhead
    const playheadX = laneStartX + (currentBeat - timeOffset) * pixelsPerBeat;
    if (playheadX >= laneStartX && playheadX <= laneStartX + laneWidth) {
      ink(255, 255, 100).line(playheadX, vizY - 10, playheadX, vizY + vizHeight + 10);
      
      // Beat pulse at playhead
      const beatPhase = currentBeat % 1;
      const pulseSize = beatPhase < 0.1 ? 6 : 3;
      ink(255, 255, 100).circle(playheadX, vizY - 15, pulseSize);
    }
    
    // Status info
    const statusY = vizY + vizHeight + 20;
    const activeDrums = ['kick', 'snare', 'hat'].filter(type => 
      drumClips.some(clip => {
        const name = (clip.name || '').toLowerCase();
        const clipStart = (clip.time || 0) / 4;
        const clipEnd = clipStart + ((clip.duration || 1) / 4);
        return name.includes(type) && currentBeat >= clipStart && currentBeat < clipEnd;
      }));
    
    ink(200, 200, 220).write(`Active drums: ${activeDrums.join(', ') || 'none'}`, 
      { x: 20, y: statusY, font: "MatrixChunky8" });
    
    ink(160, 160, 180).write(`Time: ${currentTime.toFixed(2)}s | Found ${drumClips.length} drum clips`, 
      { x: 20, y: statusY + 15, font: "MatrixChunky8" });
  }

  // Get clips that are currently active at a specific time
  getActiveClipsAtTime(currentTime) {
    const currentBeat = this.getCurrentBeat(currentTime);
    return this.clips.filter(clip => {
      const clipStart = (clip.time || 0) / 4;
      const clipDuration = (clip.duration || 1) / 4;
      const clipEnd = clipStart + clipDuration;
      return currentBeat >= clipStart && currentBeat < clipEnd;
    });
  }
  
  /*
  // COMMENTED OUT - Duplicate methods that were accidentally added
  // Simplified drum visualization showing small moving activity dots
  drawMinimalDrumVisualization(ink, screen, currentTime = 0) {
    if (!currentTime || this.tracks.length === 0) return;
    
    // Compact status bar at bottom
    const vizHeight = 40;
    const vizY = screen.height - vizHeight - 5;
    
    // Simple dark background
    ink(5, 5, 10).box(0, vizY, screen.width, vizHeight);
    
    const currentBeat = this.getCurrentBeat(currentTime);
    const activeClips = this.getActiveClipsAtTime(currentTime);
    
    // Compact info display
    ink(140, 140, 160).write(`${activeClips.length} CLIPS • ${this.tempo}BPM • ${currentTime.toFixed(1)}s`, 
      { x: 10, y: vizY + 5, font: "MatrixChunky8" });
    
    // Show small moving dots for each active clip type
    let xPos = 10;
    const dotSpacing = 20;
    
    activeClips.slice(0, 20).forEach((clip, index) => {
      const clipName = (clip.name || '').toLowerCase();
      let color;
      
      if (clipName.includes('kick')) color = [255, 100, 100];
      else if (clipName.includes('snare')) color = [100, 255, 100];
      else if (clipName.includes('hat')) color = [100, 150, 255];
      else if (clipName.includes('bass')) color = [255, 255, 100];
      else if (clip.type === 'MIDI') color = [255, 200, 100];
      else color = [200, 150, 255];
      
      // Small pulsing dot with movement
      const pulse = Math.sin(currentBeat * 6 + index) * 0.3 + 0.7;
      const finalColor = color.map(c => Math.floor(c * pulse));
      const movement = Math.sin(currentBeat * 2 + index * 0.5) * 3;
      
      ink(...finalColor).circle(xPos + movement, vizY + 25, 3);
      
      xPos += dotSpacing;
      if (xPos > screen.width - 30) return; // Stop if we run out of space
    });
  }
  
  // Display active clips as simple colored boxes
  drawActiveClipsDisplay(ink, screen, currentTime, displayY, displayHeight) {
    const activeClips = this.getActiveClipsAtTime(currentTime);
    const boxWidth = 60;
    const boxHeight = 12;
    const spacing = 5;
    
    // Background for active clips area
    ink(15, 15, 20).box(5, displayY, screen.width - 10, displayHeight);
    
    ink(120, 120, 120).write("ACTIVE CLIPS:", 
      { x: 8, y: displayY + 2, font: "MatrixChunky8" });
    
    if (activeClips.length === 0) {
      ink(80, 80, 80).write("(none)", 
        { x: 8, y: displayY + 15, font: "MatrixChunky8" });
      return;
    }
    
    // Draw active clips as colored boxes
    activeClips.slice(0, 8).forEach((clip, index) => {
      const x = 8 + (index % 4) * (boxWidth + spacing);
      const y = displayY + 15 + Math.floor(index / 4) * (boxHeight + 3);
      
      // Color based on clip type and name
      const clipName = (clip.name || '').toLowerCase();
      let clipColor;
      
      if (clipName.includes('kick') || clipName.includes('bass')) {
        clipColor = [255, 80, 80]; // Bright red
      } else if (clipName.includes('snare') || clipName.includes('clap')) {
        clipColor = [80, 255, 80]; // Bright green
      } else if (clipName.includes('hat') || clipName.includes('cymbal')) {
        clipColor = [80, 80, 255]; // Bright blue
      } else if (clip.type === 'MIDI') {
        clipColor = [255, 255, 80]; // Bright yellow
      } else {
        clipColor = [255, 140, 80]; // Bright orange
      }
      
      // Pulsing effect for active clips
      const currentBeat = this.getCurrentBeat(performance.now() / 1000);
      const pulse = Math.sin(currentBeat * 4) * 0.2 + 0.8; // Pulse with the beat
      const adjustedColor = clipColor.map(c => Math.floor(c * pulse));
      
      // Draw clip box with colored fill
      ink(...adjustedColor).box(x, y, boxWidth, boxHeight);
      
      // Add colored outline for consistency
      const outlineColor = clipColor.map(c => Math.min(255, c + 50)); // Slightly brighter outline
      ink(...outlineColor).box(x, y, boxWidth, boxHeight, "outline");
      
      // Clip name with MatrixChunky8
      const shortName = clip.name ? clip.name.substring(0, 8) : 'CLIP';
      ink(255, 255, 255).write(shortName, 
        { x: x + 1, y: y + 2, font: "MatrixChunky8" });
    });
    
    // Show total count if more clips exist
    if (activeClips.length > 8) {
      ink(200, 200, 200).write(`+${activeClips.length - 8} more`, 
        { x: screen.width - 60, y: displayY + displayHeight - 10, font: "MatrixChunky8" });
    }
  }
  
  // Simplified compact timeline with clear visual feedback
  drawCompactTimeline(ink, screen, currentTime, timelineY, timelineHeight) {
    const timelineWidth = screen.width - 10;
    
    // Simple dark background
    ink(20, 20, 20).box(5, timelineY, timelineWidth, timelineHeight);
    
    // Show 4 beats around current time for focus
    const viewWindow = 2; // seconds
    const startTime = Math.max(0, currentTime - viewWindow / 2);
    const endTime = startTime + viewWindow;
    const pixelsPerSecond = timelineWidth / viewWindow;
    
    // Simple playhead
    const playheadX = 5 + (currentTime - startTime) * pixelsPerSecond;
    ink(255, 200, 100).line(playheadX, timelineY, playheadX, timelineY + timelineHeight);
    
    // Find active clips
    const visibleClips = this.clips.filter(clip => {
      const clipStart = (clip.time || 0) / 4;
      const clipEnd = clipStart + ((clip.duration || 0) / 4);
      return clipEnd >= startTime && clipStart <= endTime;
    });
    
    // Draw clips as simple colored rectangles
    visibleClips.forEach((clip, index) => {
      const clipStart = (clip.time || 0) / 4;
      const clipDuration = (clip.duration || 0.1) / 4;
      const clipEnd = clipStart + clipDuration;
      
      const startX = Math.max(5, 5 + (clipStart - startTime) * pixelsPerSecond);
      const endX = Math.min(5 + timelineWidth, 5 + (clipEnd - startTime) * pixelsPerSecond);
      const boxWidth = Math.max(2, endX - startX);
      
      // Stack clips in rows
      const row = index % 3;
      const rowHeight = Math.floor(timelineHeight / 3);
      const boxY = timelineY + row * rowHeight + 2;
      const boxHeight = rowHeight - 4;
      
      // Check if active
      const isActive = currentTime >= clipStart && currentTime <= clipEnd;
      
      // Simple color coding
      const clipName = (clip.name || '').toLowerCase();
      let baseColor;
      
      if (clipName.includes('kick')) baseColor = [200, 100, 100];
      else if (clipName.includes('snare')) baseColor = [100, 200, 100];
      else if (clipName.includes('hat')) baseColor = [100, 100, 200];
      else baseColor = [150, 150, 100];
      
      const intensity = isActive ? 1.0 : 0.4;
      const color = baseColor.map(c => Math.floor(c * intensity));
      
      ink(...color).box(startX, boxY, boxWidth, boxHeight);
      
      // Clip label if active and enough space
      if (isActive && boxWidth > 25) {
        const shortName = (clip.name || 'CLIP').substring(0, 6);
        ink(255, 255, 255).write(shortName, 
          { x: startX + 1, y: boxY + 1, font: "MatrixChunky8" });
      }
    });
    
    // Time markers
    for (let t = Math.floor(startTime); t <= Math.ceil(endTime); t++) {
      const x = 5 + (t - startTime) * pixelsPerSecond;
      if (x >= 5 && x <= 5 + timelineWidth) {
        ink(60, 60, 60).line(x, timelineY, x, timelineY + 8);
        ink(120, 120, 120).write(`${t}s`, 
          { x: x + 1, y: timelineY + timelineHeight - 8, font: "MatrixChunky8" });
      }
    }
  }
  
  // Draw percussion events as dots on timeline
  drawPercussionEvents(ink, screen, timelineY, timelineHeight, startTime, pixelsPerSecond, currentTime) {
    // Show actual clips instead of fake patterns
    const visibleClips = this.clips.filter(clip => {
      const clipStart = (clip.time || 0) / 4; // Convert ALS time to seconds (rough conversion)
      const clipEnd = clipStart + ((clip.duration || 0) / 4);
      const endTime = startTime + (screen.width - 100) / pixelsPerSecond;
      
      return clipEnd >= startTime && clipStart <= endTime;
    });
    
    // Debug: Show what clips we found
    if (visibleClips.length > 0) {
      console.log(`🎵 Found ${visibleClips.length} clips in view:`, 
        visibleClips.map(c => ({ name: c.name, type: c.type, time: c.time, duration: c.duration })));
    }
    
    // Draw actual clips as rectangles
    visibleClips.forEach((clip, index) => {
      const clipStart = (clip.time || 0) / 4; // Convert ALS time to seconds
      const clipDuration = (clip.duration || 1) / 4;
      const clipEnd = clipStart + clipDuration;
      
      const startX = 50 + (clipStart - startTime) * pixelsPerSecond;
      const endX = 50 + (clipEnd - startTime) * pixelsPerSecond;
      const clipWidth = Math.max(1, endX - startX);
      
      // Position on timeline based on clip type/name
      let trackY = timelineY + 10; // Default
      const clipName = (clip.name || '').toLowerCase();
      const samplePath = clip.sampleRef ? (clip.sampleRef.path || '').toLowerCase() : '';
      
      // Determine track based on content
      if (clipName.includes('kick') || samplePath.includes('kick')) {
        trackY = timelineY + 10;
      } else if (clipName.includes('snare') || samplePath.includes('snare')) {
        trackY = timelineY + 25;
      } else if (clipName.includes('hihat') || samplePath.includes('hihat') || 
                 clipName.includes('hat') || samplePath.includes('hat')) {
        trackY = timelineY + 40;
      } else {
        trackY = timelineY + 15 + (index % 3) * 15; // Distribute other clips
      }
      
      // Clip color based on type
      const clipColor = clip.type === 'MIDI' ? [100, 150, 255] : [255, 150, 100];
      
      // Highlight if currently playing
      const isCurrentlyPlaying = currentTime >= clipStart && currentTime <= clipEnd;
      const intensity = isCurrentlyPlaying ? 1.0 : 0.6;
      
      if (startX < screen.width - 50 && endX > 50) {
        // Draw clip rectangle
        ink(clipColor[0] * intensity, clipColor[1] * intensity, clipColor[2] * intensity)
          .box(Math.max(50, startX), trackY - 3, 
               Math.min(clipWidth, screen.width - 50 - Math.max(50, startX)), 6);
        
        // Draw clip name if there's space
        if (clipWidth > 30) {
          ink("white").write(clip.name || `${clip.type} ${clip.id}`, 
            { x: Math.max(52, startX + 2), y: trackY - 1, font: "microtype" });
        }
      }
      
      // For MIDI clips, also show individual notes
      if (clip.type === 'MIDI' && clip.notes && clip.notes.length > 0) {
        clip.notes.forEach(note => {
          const noteStart = clipStart + (note.time || 0) / 4;
          const noteEnd = noteStart + (note.duration || 0.1) / 4;
          const noteX = 50 + (noteStart - startTime) * pixelsPerSecond;
          
          if (noteX >= 50 && noteX <= screen.width - 50) {
            const timeSinceNote = currentTime - noteStart;
            const isRecentNote = timeSinceNote >= 0 && timeSinceNote < 0.2;
            
            if (isRecentNote) {
              const intensity = 1 - (timeSinceNote / 0.2);
              ink(255 * intensity, 255 * intensity, 100).circle(noteX, trackY, 3 + intensity);
            } else {
              ink(150, 150, 200).circle(noteX, trackY, 1);
            }
          }
        });
      }
    });
    
    // Draw track labels for actual data
    ink("white").write("CLIPS", { x: 10, y: timelineY + 20, font: "microtype" });
    ink("cyan").write(`${this.clips.length} total`, { x: 10, y: timelineY + 35, font: "microtype" });
  }
  */
  
  // Compact drum status boxes
  drawCompactDrumBoxes(ink, screen, currentTime, currentBeat, boxY, boxHeight) {
    const boxWidth = 40;
    const spacing = 45;
    const startX = 10;
    
    // Only show drum types that actually have clips
    const drumTypes = [
      { name: "KICK", keywords: ['kick'], color: [255, 100, 100] },
      { name: "SNARE", keywords: ['snare'], color: [100, 255, 100] },
      { name: "HIHAT", keywords: ['hihat', 'hat'], color: [100, 100, 255] },
      { name: "BASS", keywords: ['bass'], color: [255, 255, 100] },
      { name: "PERC", keywords: ['perc'], color: [255, 100, 255] }
    ];
    
    let activeBoxCount = 0;
    
    drumTypes.forEach((drum) => {
      // Find matching clips
      const matchingClips = this.clips.filter(clip => {
        const clipName = (clip.name || '').toLowerCase();
        const samplePath = clip.sampleRef ? (clip.sampleRef.path || '').toLowerCase() : '';
        return drum.keywords.some(keyword => 
          clipName.includes(keyword) || samplePath.includes(keyword)
        );
      });
      
      // Only draw if we have clips
      if (matchingClips.length > 0) {
        const x = startX + activeBoxCount * spacing;
        
        // Check if any matching clip is currently playing
        const isActive = matchingClips.some(clip => {
          const clipStart = (clip.time || 0) / 4;
          const clipEnd = clipStart + ((clip.duration || 0) / 4);
          return currentTime >= clipStart && currentTime <= clipEnd;
        });
        
        const intensity = isActive ? 1.0 : 0.3;
        ink(drum.color[0] * intensity, drum.color[1] * intensity, drum.color[2] * intensity)
          .box(x, boxY, boxWidth, boxHeight - 5);
        
        // Label
        const textColor = isActive ? "white" : "gray";
        ink(textColor).write(drum.name, 
          { x: x + 2, y: boxY + boxHeight - 12, font: "microtype" });
        
        activeBoxCount++;
      }
    });
    
    // Show clip count
    ink("white").write(`${this.clips.length} clips`, 
      { x: screen.width - 60, y: boxY + boxHeight - 12, font: "microtype" });
  }
  
  // Simple beat grid visualization
  drawBeatGrid(ink, screen, currentBeat) {
    const gridY = screen.height - 80;
    const gridWidth = screen.width - 100;
    const beatsToShow = 8;
    const beatWidth = gridWidth / beatsToShow;
    
    for (let i = 0; i < beatsToShow; i++) {
      const beat = Math.floor(currentBeat) + i - Math.floor(beatsToShow / 2);
      const x = 50 + i * beatWidth;
      
      // Highlight current beat
      if (Math.abs(beat - currentBeat) < 0.5) {
        ink(255, 255, 0).box(x, gridY, beatWidth - 2, 20);
        ink("black").write(`${beat}`, { x: x + 5, y: gridY + 5, font: "microtype" });
      } else {
        ink(60, 60, 60).box(x, gridY, beatWidth - 2, 20);
        ink("white").write(`${beat}`, { x: x + 5, y: gridY + 5, font: "microtype" });
      }
    }
  }
  
  // Use actual clip data instead of fake patterns
  isDrumActiveAtTime(drumName, currentTime, currentBeat) {
    // Convert current time to the timing format used in ALS clips
    // ALS typically uses 4 units per beat, so we need to adjust
    const currentTimeInALS = currentTime * (this.tempo / 60) * 4; // Convert to ALS timing units
    
    // Debug timing conversion
    if (currentTime < 10) {
      console.log(`🕐 Timing conversion: ${currentTime.toFixed(2)}s -> ${currentTimeInALS.toFixed(2)} ALS units`);
    }
    
    // Find clips that might contain this drum element
    const relevantClips = this.clips.filter(clip => {
      // Check if clip name suggests it contains drums
      const clipName = (clip.name || '').toLowerCase();
      const samplePath = clip.sampleRef ? (clip.sampleRef.path || '').toLowerCase() : '';
      
      // Match drum types to clip names/paths
      switch (drumName) {
        case "KICK":
          return clipName.includes('kick') || samplePath.includes('kick') || 
                 clipName.includes('bass') || samplePath.includes('bass');
        case "SNARE":
          return clipName.includes('snare') || samplePath.includes('snare');
        case "HIHAT":
          return clipName.includes('hihat') || samplePath.includes('hihat') ||
                 clipName.includes('hat') || samplePath.includes('hat');
        case "CYMBAL":
          return clipName.includes('cymbal') || samplePath.includes('cymbal') ||
                 clipName.includes('crash') || samplePath.includes('crash');
        case "PERC":
          return clipName.includes('perc') || samplePath.includes('perc') ||
                 clipName.includes('shaker') || samplePath.includes('shaker') ||
                 clipName.includes('tom') || samplePath.includes('tom');
        default:
          return false;
      }
    });
    
    // Debug what clips we found
    if (currentTime < 5 && relevantClips.length > 0) {
      console.log(`🎯 Found ${relevantClips.length} ${drumName} clips:`, relevantClips.map(c => c.name));
    }
    
    // Check if any relevant clips are active at current time
    for (const clip of relevantClips) {
      const clipStart = clip.time || 0;
      const clipDuration = clip.duration || 1;
      const clipEnd = clipStart + clipDuration;
      
      // Debug clip timing
      if (currentTime < 5) {
        console.log(`📍 ${drumName} clip "${clip.name}": ${clipStart}-${clipEnd}, current: ${currentTimeInALS}`);
      }
      
      // Check if clip is set to loop (most ALS clips loop by default)
      // If we're past the clip end time, check if we're within a loop cycle
      let isInClipRange = false;
      
      if (currentTimeInALS >= clipStart && currentTimeInALS <= clipEnd) {
        // Direct hit - we're in the original clip time
        isInClipRange = true;
      } else if (currentTimeInALS > clipEnd && clipDuration > 0) {
        // We're past the clip end, check if it's looping
        // Calculate which loop cycle we're in
        const timeFromClipStart = currentTimeInALS - clipStart;
        const loopPosition = timeFromClipStart % clipDuration;
        
        // We're within a loop cycle
        isInClipRange = true;
        
        if (currentTime < 5) {
          console.log(`🔄 ${drumName} clip "${clip.name}" LOOPING: loop pos ${loopPosition.toFixed(2)} of ${clipDuration}`);
        }
      }
      
      if (isInClipRange) {
        // For MIDI clips, check actual note data
        if (clip.type === 'MIDI' && clip.notes) {
          for (const note of clip.notes) {
            const noteStart = note.time || 0;
            const noteEnd = noteStart + (note.duration || 0.1);
            
            // For looping clips, check note timing within the current loop cycle
            let noteTimeInLoop = noteStart;
            if (currentTimeInALS > clipEnd && clipDuration > 0) {
              const timeFromClipStart = currentTimeInALS - clipStart;
              const loopPosition = timeFromClipStart % clipDuration;
              noteTimeInLoop = noteStart;
              
              // Check if the note is active at this point in the loop
              if (loopPosition >= noteStart && loopPosition <= noteEnd) {
                console.log(`🎵 ${drumName} ACTIVE via MIDI note (looped) at ${currentTime.toFixed(2)}s!`);
                return true;
              }
            } else {
              // Original clip timing (not looped yet)
              const noteStartAbs = clipStart + noteStart;
              const noteEndAbs = noteStartAbs + (note.duration || 0.1);
              
              if (currentTimeInALS >= noteStartAbs && currentTimeInALS <= noteEndAbs) {
                console.log(`🎵 ${drumName} ACTIVE via MIDI note at ${currentTime.toFixed(2)}s!`);
                return true;
              }
            }
          }
        }
        // For Audio clips, assume active throughout the clip duration (with looping)
        else if (clip.type === 'Audio') {
          console.log(`🎵 ${drumName} ACTIVE via Audio clip (${currentTimeInALS > clipEnd ? 'looped' : 'direct'}) at ${currentTime.toFixed(2)}s!`);
          return true;
        }
      }
    }
    
    return false; // No real clip data matches, don't show anything
  }
  
  // Get the time of the last drum hit for flash effects
  getLastDrumHitTime(drumName, currentTime) {
    const currentBeat = this.getCurrentBeat(currentTime);
    
    // Look backwards in time to find the last hit
    for (let t = currentTime; t >= currentTime - 2; t -= 0.01) {
      const pastBeat = this.getCurrentBeat(t);
      if (this.isDrumActiveAtTime(drumName, t, pastBeat)) {
        return t;
      }
    }
    return currentTime - 1; // Default to 1 second ago if no hit found
  }
  
  // Determine if a drum element should be active based on ALS data
  isDrumActive(drumName, currentBeat, currentTime) {
    // Simple pattern generation based on beat timing
    // In a real implementation, this would analyze the ALS data for drum hits
    
    const beat = Math.floor(currentBeat) % 4;
    const subBeat = currentBeat % 1;
    
    switch (drumName) {
      case "KICK":
        return beat === 0 || beat === 2; // On 1 and 3
      case "SNARE":
        return beat === 1 || beat === 3; // On 2 and 4
      case "HIHAT":
        return subBeat < 0.1 || (subBeat > 0.4 && subBeat < 0.6); // 8th note pattern
      case "BASS":
        return beat === 0 || (beat === 2 && subBeat > 0.5); // Syncopated bass
      case "PERC":
        return subBeat > 0.7; // Off-beat percussion
      case "CYMBAL":
        return beat === 0 && Math.floor(currentBeat / 4) % 4 === 0; // Every 4 bars
      default:
        return false;
    }
  }
  
  // Draw live musical notation
  drawLiveNotation(ink, screen, beatPosition, vizData) {
    const notationY = 50;
    const notationHeight = 120;
    
    // Staff lines (5-line musical staff)
    const staffSpacing = 8;
    ink(80, 80, 100);
    for (let i = 0; i < 5; i++) {
      const lineY = notationY + i * staffSpacing;
      ink().line(20, lineY, screen.width - 20, lineY);
    }
    
    // Treble clef symbol (simplified)
    ink("white").write("𝄞", { x: 25, y: notationY - 8, font: "microtype" });
    
    // Draw active notes as notation
    const notesPerScreen = 16;
    const noteSpacing = (screen.width - 80) / notesPerScreen;
    
    vizData.activeNotes.forEach((note, index) => {
      const noteX = 60 + (index * noteSpacing);
      this.drawMusicalNote(ink, noteX, notationY, note, staffSpacing);
    });
    
    // Draw measure lines
    const measuresOnScreen = 4;
    const measureWidth = (screen.width - 80) / measuresOnScreen;
    ink(60, 60, 80);
    for (let m = 0; m <= measuresOnScreen; m++) {
      const measureX = 60 + m * measureWidth;
      ink().line(measureX, notationY - 10, measureX, notationY + staffSpacing * 4 + 10);
    }
    
    // Current beat indicator on staff
    const beatX = 60 + ((beatPosition % 4) / 4) * measureWidth;
    ink("red").line(beatX, notationY - 15, beatX, notationY + staffSpacing * 4 + 15);
  }
  
  // Draw individual musical notes
  drawMusicalNote(ink, x, staffY, note, staffSpacing) {
    // Map velocity to note intensity
    const intensity = note.velocity / 127;
    
    // Map MIDI note to staff position (simplified)
    const midiNote = note.pitch || 60; // Default to middle C
    const staffPosition = this.midiToStaffPosition(midiNote);
    const noteY = staffY + staffPosition * (staffSpacing / 2);
    
    // Note head
    ink(255 * intensity, 255 * intensity, 100).circle(x, noteY, 3);
    
    // Note stem
    if (staffPosition < 4) {
      // Stem up
      ink().line(x + 3, noteY, x + 3, noteY - 20);
    } else {
      // Stem down
      ink().line(x - 3, noteY, x - 3, noteY + 20);
    }
    
    // Velocity indicator (note size variation)
    const noteSize = 2 + intensity * 2;
    ink(255 * intensity, 200, 100, intensity).circle(x, noteY, noteSize);
  }
  
  // Convert MIDI note number to staff position
  midiToStaffPosition(midiNote) {
    // C4 (middle C) = MIDI 60, maps to position 8 (below staff)
    // Each semitone = 0.5 staff position
    const middleC = 60;
    const staffMiddle = 8; // Below the staff
    return staffMiddle - ((midiNote - middleC) * 0.3);
  }
  
  // Draw track notation with rhythmic patterns
  drawTrackNotation(ink, screen, trackData, y, trackHeight, startBeat, pixelsPerBeat, currentBeat, trackIndex = 0) {
    const { track, activity, notes } = trackData;
    
    // Track background with activity pulse
    const brightness = 30 + (activity * 50);
    const beatPulse = Math.sin(currentBeat * Math.PI * 2) * 10 + 10;
    const color = track.color;
    
    ink(color.r * brightness / 100, color.g * brightness / 100, color.b * brightness / 100)
      .box(20, y, screen.width - 40, trackHeight - 2);
    
    // Track name with instrument symbol
    const instrumentSymbol = this.getInstrumentSymbol(track.type);
    ink("white").write(`${instrumentSymbol} ${track.name}`, 
      { x: 25, y: y + 3, font: "microtype" });
    
    // Draw clips on the timeline as boxes
    const trackClips = this.clips.filter(clip => clip.trackRef === track.id || clip.trackRef === trackData.track.id);
    
    // Debug: Log clip information
    if (trackIndex === 0) { // Only log for first track to avoid spam
      console.log(`🎵 Timeline Debug - Total clips: ${this.clips.length}, Track clips: ${trackClips.length}`);
      if (this.clips.length > 0) {
        console.log(`🎵 First clip example:`, this.clips[0]);
        console.log(`🎵 Current track ID: ${track.id}, Track ref examples:`, this.clips.slice(0, 3).map(c => c.trackRef));
      }
    }
    
    trackClips.forEach(clip => {
      const clipStartBeat = clip.time;
      const clipEndBeat = clip.time + clip.duration;
      
      // Debug for first clip
      if (trackIndex === 0 && trackClips.indexOf(clip) === 0) {
        console.log(`🎵 Drawing clip: Start=${clipStartBeat}, End=${clipEndBeat}, StartBeat=${startBeat}, Duration=${clip.duration}`);
      }
      
      // Only draw clips that are visible on the current timeline view (expanded range)
      if (clipEndBeat >= startBeat && clipStartBeat <= startBeat + 16) { // Use 16 instead of 8 to match beatsPerScreen
        const clipX = 20 + Math.max(0, clipStartBeat - startBeat) * pixelsPerBeat;
        const clipEndX = 20 + Math.min(16, clipEndBeat - startBeat) * pixelsPerBeat; // Use 16 instead of 8
        const clipWidth = clipEndX - clipX;
        
        if (clipWidth > 1) { // Only draw if clip is wide enough to see
          const clipHeight = trackHeight - 6;
          const clipY = y + 3;
          
          // Different colors for MIDI vs Audio clips
          if (clip.type === 'MIDI') {
            // MIDI clips: brighter colors with note activity
            const noteIntensity = notes.length > 0 ? 0.8 : 0.4;
            ink(100 + noteIntensity * 100, 255 * noteIntensity, 150, 0.7)
              .box(clipX, clipY, clipWidth, clipHeight);
          } else {
            // Audio clips: more muted colors
            ink(150, 100, 200, 0.6)
              .box(clipX, clipY, clipWidth, clipHeight);
          }
          
          // Clip border
          ink(255, 255, 255, 0.8).line(clipX, clipY, clipX + clipWidth, clipY); // Top
          ink(255, 255, 255, 0.8).line(clipX, clipY, clipX, clipY + clipHeight); // Left
          
          // Clip name (if it fits)
          if (clipWidth > 30 && clip.name && clip.name.length > 0) {
            const displayName = clip.name.length > 8 ? clip.name.substring(0, 8) + "..." : clip.name;
            ink("white").write(displayName, { 
              x: clipX + 2, 
              y: clipY + 2, 
              font: "microtype" 
            });
          }
          
          // Loop indicator
          if (clip.loop && clip.loop.enabled) {
            ink("yellow").write("⟲", { 
              x: clipX + clipWidth - 12, 
              y: clipY + 2, 
              font: "microtype" 
            });
          }
        }
      }
    });
    
    // Draw rhythmic notation for active notes (on top of clips)
    notes.forEach(note => {
      const noteStartBeat = this.timeToBeats(note.absoluteStart);
      const noteDuration = this.timeToBeats(note.duration);
      
      if (noteStartBeat >= startBeat && noteStartBeat <= startBeat + 8) {
        const noteX = 20 + (noteStartBeat - startBeat) * pixelsPerBeat;
        const noteWidth = Math.max(2, noteDuration * pixelsPerBeat);
        
        // Note rectangle with velocity-based height (smaller, overlay style)
        const velocity = note.velocity / 127;
        const noteHeight = Math.min(4, (trackHeight - 8) * velocity * 0.5);
        const noteY = y + trackHeight - 4 - noteHeight;
        
        // Bright note indicators on top of clips
        ink(255, 255 * velocity, 100, velocity * 0.9)
          .box(noteX, noteY, noteWidth, noteHeight);
      }
    });
    
    // Track activity meter
    const meterWidth = 30;
    const meterHeight = trackHeight - 6;
    const meterX = screen.width - 50;
    
    // Meter background
    ink(20, 20, 30).box(meterX, y + 2, meterWidth, meterHeight);
    
    // Activity level
    const activityHeight = activity * meterHeight;
    ink(color.r, color.g, color.b, activity).box(meterX, y + 2 + meterHeight - activityHeight, meterWidth, activityHeight);
    
    // Peak indicator
    if (activity > 0.8) {
      ink("white").write("●", { x: meterX + meterWidth + 2, y: y + 3, font: "microtype" });
    }
  }
  
  // Get instrument symbol for track type
  getInstrumentSymbol(trackType) {
    const symbols = {
      MidiTrack: "🎹",
      AudioTrack: "🔊", 
      ReturnTrack: "↩️",
      GroupTrack: "📁"
    };
    return symbols[trackType] || "🎵";
  }
  
  // Draw rhythmic notation symbols
  drawRhythmicSymbol(ink, x, y, duration, velocity) {
    const intensity = velocity;
    
    if (duration >= 2.0) {
      // Whole note
      ink(255 * intensity, 255 * intensity, 255 * intensity).write("𝅝", { x, y, font: "microtype" });
    } else if (duration >= 1.0) {
      // Half note
      ink(255 * intensity, 255 * intensity, 255 * intensity).write("𝅗𝅥", { x, y, font: "microtype" });
    } else if (duration >= 0.5) {
      // Quarter note
      ink(255 * intensity, 255 * intensity, 255 * intensity).write("♩", { x, y, font: "microtype" });
    } else if (duration >= 0.25) {
      // Eighth note
      ink(255 * intensity, 255 * intensity, 255 * intensity).write("♪", { x, y, font: "microtype" });
    } else {
      // Sixteenth note
      ink(255 * intensity, 255 * intensity, 255 * intensity).write("♬", { x, y, font: "microtype" });
    }
  }
  
  // Convert time to beats for notation
  timeToBeats(timeValue) {
    return timeValue; // Already in beats for ALS format
  }
  
  // Enhanced mini diagram with beat synchronization
  drawMiniDiagram(ink, x, y, beatTime = 0) {
    if (this.tracks.length === 0) return;
    
    const beatPosition = this.getCurrentBeat(beatTime);
    const vizData = this.generateVisualizationData(beatPosition);
    
    // Compact track visualization
    const trackWidth = 80;
    const trackHeight = 8;
    const spacing = 2;
    
    vizData.trackActivity.forEach((trackData, index) => {
      const trackY = y + index * (trackHeight + spacing);
      const { track, activity } = trackData;
      
      // Track bar with activity-based brightness
      const brightness = 0.3 + (activity * 0.7);
      ink(track.color.r * brightness, track.color.g * brightness, track.color.b * brightness)
        .box(x, trackY, trackWidth, trackHeight);
      
      // Track name (abbreviated)
      const name = track.name.substring(0, 8);
      ink("white").write(name, { x: x + 2, y: trackY + 1, font: "microtype" });
      
      // Activity indicator
      if (activity > 0.5) {
        ink("white").write("●", { x: x + trackWidth - 8, y: trackY + 1, font: "microtype" });
      }
    });
    
    // Beat position indicator
    const beatPhase = (beatPosition % 4) / 4;
    const indicatorX = x + beatPhase * trackWidth;
    ink("red").line(indicatorX, y - 2, indicatorX, y + (trackHeight + spacing) * this.tracks.length);
  }
  
  // Draw harmonic analysis information
  drawHarmonicAnalysis(ink, screen, harmonicContext, timelineY) {
    const analysisY = 30;
    const analysisHeight = 60;
    
    // Background for harmonic analysis
    ink(15, 15, 25, 0.8).box(screen.width - 200, analysisY, 180, analysisHeight);
    
    // Scale information
    ink("lightblue").write(`Scale: ${harmonicContext.scale}`, 
      { x: screen.width - 195, y: analysisY + 5, font: "microtype" });
    
    // Chord information
    ink("lightgreen").write(`Chord: ${harmonicContext.chord}`, 
      { x: screen.width - 195, y: analysisY + 17, font: "microtype" });
    
    // Root note (convert MIDI to note name)
    const rootNoteName = this.midiToNoteName(harmonicContext.rootNote);
    ink("orange").write(`Root: ${rootNoteName}`, 
      { x: screen.width - 195, y: analysisY + 29, font: "microtype" });
    
    // Harmonic tension meter
    const tensionWidth = 60;
    const tensionHeight = 8;
    const tensionX = screen.width - 195;
    const tensionY = analysisY + 42;
    
    // Tension background
    ink(40, 40, 40).box(tensionX, tensionY, tensionWidth, tensionHeight);
    
    // Tension level
    const tensionLevel = harmonicContext.tension || 0;
    const tensionColor = tensionLevel > 0.6 ? [255, 100, 100] : 
                        tensionLevel > 0.3 ? [255, 200, 100] : [100, 255, 100];
    
    ink(...tensionColor).box(tensionX, tensionY, tensionWidth * tensionLevel, tensionHeight);
    ink("white").write(`Tension: ${(tensionLevel * 100).toFixed(0)}%`, 
      { x: tensionX + tensionWidth + 5, y: tensionY + 1, font: "microtype" });
    
    // Active pitches visualization
    if (harmonicContext.pitches && harmonicContext.pitches.length > 0) {
      const pitchDisplayY = analysisY + 55;
      ink("gray").write("Active Notes:", { x: screen.width - 195, y: pitchDisplayY, font: "microtype" });
      
      harmonicContext.pitches.slice(0, 8).forEach((pitch, index) => {
        const noteName = this.midiToNoteName(pitch);
        const noteX = screen.width - 135 + (index * 15);
        
        // Color code by pitch class
        const pitchClass = pitch % 12;
        const noteColor = this.getPitchClassColor(pitchClass);
        
        ink(...noteColor).write(noteName, { x: noteX, y: pitchDisplayY, font: "microtype" });
      });
    }
  }
  
  // Convert MIDI note number to note name
  midiToNoteName(midiNote) {
    const noteNames = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'];
    const octave = Math.floor(midiNote / 12) - 1;
    const noteClass = midiNote % 12;
    return `${noteNames[noteClass]}${octave}`;
  }
  
  // Get color for pitch class (chromatic circle colors)
  getPitchClassColor(pitchClass) {
    const colors = [
      [255, 100, 100], // C - Red
      [255, 150, 100], // C# - Orange-Red
      [255, 200, 100], // D - Orange
      [255, 255, 100], // D# - Yellow
      [200, 255, 100], // E - Yellow-Green
      [100, 255, 100], // F - Green
      [100, 255, 200], // F# - Green-Cyan
      [100, 200, 255], // G - Cyan
      [100, 150, 255], // G# - Blue
      [150, 100, 255], // A - Purple
      [200, 100, 255], // A# - Violet
      [255, 100, 200]  // B - Magenta
    ];
    return colors[pitchClass] || [128, 128, 128];
  }
  
  // Simple clip statistics for debugging
  getClipStats() {
    const midiClips = this.clips.filter(clip => clip.type === 'midi').length;
    const audioClips = this.clips.filter(clip => clip.type === 'audio').length;
    return `Clips: ${this.clips.length} (${midiClips}M, ${audioClips}A)`;
  }
  
  // Get drum elements active at specific beat (for visualization)
  getActiveElementsAtTime(currentBeat) {
    const activeElements = {
      kick: false,
      snare: false,
      hihat: false,
      bass: false,
      activeClips: []
    };
    
    // Find all clips that should be playing at this beat
    this.clips.forEach(clip => {
      const clipStartBeat = clip.time || 0;
      const clipEndBeat = clipStartBeat + (clip.duration || 1);
      
      // Check if this clip is active at the current beat
      if (currentBeat >= clipStartBeat && currentBeat < clipEndBeat) {
        activeElements.activeClips.push(clip);
        
        // Find the track this clip belongs to
        const track = this.tracks.find(t => t.id === clip.trackRef);
        const trackName = track?.name?.toLowerCase() || '';
        const clipName = (clip.name || '').toLowerCase();
        
        // Debug output for drum detection
        if (this.debugMode) {
          console.log(`🔍 Checking clip "${clip.name}" on track "${track?.name || 'unknown'}" for drums`);
        }
        
        // Check for drum elements by track/clip name (expanded patterns)
        if (trackName.includes('kick') || clipName.includes('kick') || trackName.includes('# kick')) {
          activeElements.kick = true;
          console.log(`🥁 KICK detected from ${trackName || clipName}`);
        }
        if (trackName.includes('snare') || clipName.includes('snare') || trackName.includes('# snare') || 
            clipName.includes('clap') || trackName.includes('clap')) {
          activeElements.snare = true;
          console.log(`🥁 SNARE detected from ${trackName || clipName}`);
        }
        if (trackName.includes('hat') || clipName.includes('hat') || trackName.includes('# hat')) {
          activeElements.hihat = true;
          console.log(`🥁 HIHAT detected from ${trackName || clipName}`);
        }
        if (trackName.includes('bass') || clipName.includes('bass') || trackName.includes('# bass')) {
          activeElements.bass = true;
          console.log(`🥁 BASS detected from ${trackName || clipName}`);
        }
        
        // Additional fallback: detect drums by clip names we see in the logs
        if (clipName.includes('drum')) {
          activeElements.snare = true; // "EXC!TE Snare Drum" -> snare
          console.log(`🥁 DRUM detected from clip name: ${clipName}`);
        }
        
        // Check for MIDI notes at this specific time within the clip
        if (clip.notes && clip.notes.length > 0) {
          const relativeTime = currentBeat - clipStartBeat;
          const activeNotes = clip.notes.filter(note => {
            const noteStart = note.time || 0;
            const noteEnd = noteStart + (note.duration || 0.1);
            return relativeTime >= noteStart && relativeTime < noteEnd;
          });
          
          if (activeNotes.length > 0) {
            // Debug: show active notes (only if debug mode enabled)
            if (this.debugMode) {
              console.log(`🎵 Active notes at beat ${currentBeat.toFixed(2)}: ${activeNotes.length} in clip "${clip.name}"`);
            }
            
            // Analyze note pitches to determine drum type (typical drum mapping)
            activeNotes.forEach(note => {
              if (note.pitch >= 35 && note.pitch <= 36) activeElements.kick = true;    // C1-C#1
              if (note.pitch >= 38 && note.pitch <= 40) activeElements.snare = true;  // D1-E1  
              if (note.pitch >= 42 && note.pitch <= 46) activeElements.hihat = true;  // F#1-A#1
            });
          }
        }
      }
    });
    
    return activeElements;
  }
  
  // Comprehensive project structure analysis
  analyzeProjectStructure() {
    console.log("\n🎵 ===== PROJECT STRUCTURE ANALYSIS =====");
    console.log(`📊 Project: "${this.projectName}" | ${this.tracks.length} tracks | ${this.clips.length} clips | ${this.scenes.length} scenes`);
    console.log(`🎵 Tempo: ${this.tempo} BPM | Time Sig: ${this.timeSignature.numerator}/${this.timeSignature.denominator}`);
    console.log(`🎚️ Creator: ${this.creator} | Version: ${this.version}`);
    
    // Analyze tracks by type
    const tracksByType = {};
    this.tracks.forEach(track => {
      if (!tracksByType[track.type]) tracksByType[track.type] = [];
      tracksByType[track.type].push(track);
    });
    
    console.log("\n🎯 TRACKS BY TYPE:");
    Object.keys(tracksByType).forEach(type => {
      console.log(`  ${type}: ${tracksByType[type].length} tracks`);
      tracksByType[type].forEach(track => {
        const trackClips = this.clips.filter(clip => clip.trackRef === track.id);
        console.log(`    - "${track.name}" (ID: ${track.id}) - ${trackClips.length} clips`);
      });
    });
    
    // Analyze clip timing patterns
    const clipsByTime = {};
    const clipsByType = { MIDI: 0, Audio: 0 };
    
    this.clips.forEach(clip => {
      const startBeat = Math.floor(clip.time || 0);
      if (!clipsByTime[startBeat]) clipsByTime[startBeat] = [];
      clipsByTime[startBeat].push(clip);
      clipsByType[clip.type] = (clipsByType[clip.type] || 0) + 1;
    });
    
    console.log(`\n⏱️ CLIP DISTRIBUTION: ${clipsByType.MIDI || 0} MIDI clips, ${clipsByType.Audio || 0} Audio clips`);
    
    const timeKeys = Object.keys(clipsByTime).sort((a, b) => parseInt(a) - parseInt(b));
    console.log(`📍 CLIPS BY START TIME (first 8 beats):`);
    timeKeys.slice(0, 8).forEach(beat => {
      const clips = clipsByTime[beat];
      console.log(`  Beat ${beat}: ${clips.length} clips - ${clips.map(c => c.name || 'unnamed').slice(0, 5).join(', ')}`);
    });
    
    // Find drum-related content
    const drumTracks = this.tracks.filter(track => {
      const name = track.name.toLowerCase();
      return name.includes('kick') || name.includes('snare') || name.includes('hat') || 
             name.includes('clap') || name.includes('drum') || name.includes('perc');
    });
    
    console.log(`\n🥁 DRUM ELEMENTS (${drumTracks.length} drum tracks found):`);
    drumTracks.forEach(track => {
      const trackClips = this.clips.filter(clip => 
        clip.trackRef === track.id || clip.name?.toLowerCase().includes(track.name.toLowerCase())
      );
      console.log(`  "${track.name}": ${trackClips.length} clips`);
      
      // Show timing pattern for first few clips
      trackClips.slice(0, 4).forEach(clip => {
        console.log(`    - Beat ${clip.time?.toFixed(1) || '?'}: "${clip.name}" (${clip.notes?.length || 0} notes)`);
      });
    });
    
    // Show beat pattern for first 4 beats
    console.log(`\n🎵 BEAT PATTERN ANALYSIS (first 4 beats @ ${this.tempo} BPM):`);
    for (let beat = 0; beat < 4; beat++) {
      const beatClips = this.clips.filter(clip => {
        const clipBeat = Math.floor(clip.time || 0);
        return clipBeat === beat;
      });
      console.log(`  Beat ${beat}: ${beatClips.length} active clips`);
      beatClips.forEach(clip => {
        console.log(`    - "${clip.name}" (${clip.type}, ${clip.notes?.length || 0} notes)`);
      });
    }
    
    console.log("🎵 ===== END ANALYSIS =====\n");
    
    return {
      tracksByType,
      clipsByTime,
      drumTracks,
      summary: {
        totalTracks: this.tracks.length,
        totalClips: this.clips.length,
        midiClips: clipsByType.MIDI || 0,
        audioClips: clipsByType.Audio || 0,
        drumTracks: drumTracks.length,
        tempo: this.tempo
      }
    };
  }
}

// State
let alsProject = null;
let wavFile = null;
let preloadedAudio = null; // Store preloaded audio data
let isPlaying = false;
let playingSfx = null;
let playStartTime = 0;
let audioStartTime = 0; // Track when audio actually started
let lastAutoStartAttempt = 0; // Throttle auto-start attempts
let message = "Drop .als + .wav files";
let netAPI = null; // Store net API for audio preloading
let soundDebugLogged = false; // Track if sound debug info has been logged
let useSimpleTimeline = true; // Default to simplified real-time activity view

// Add boot function to access net API for audio preloading
export const boot = ({ net }) => {
  console.log("🎵 ABLETON.MJS: Boot function called with net API");
  netAPI = net; // Store net API for audio preloading
  console.log("🎵 ABLETON.MJS: Net API stored:", !!netAPI);
};

// Initialize
console.log("🎵 ABLETON.MJS: Enhanced visualizer loaded and ready!");

function paint({ wipe, ink, screen, sound, clock, write, box, line }) {
  wipe("black");
  
  if (!alsProject) {
    ink("green").write("Drop ALS + WAV files to begin", { center: "xy", size: 1.5 });
    ink("cyan", 0.7).write("Real-time activity visualizer", { center: "x", y: screen.height/2 + 40 });
    ink("white", 0.6).write("[Space] or touch to play/pause • [T] toggle view modes", { center: "x", y: screen.height/2 + 60 });
    
    // Show debug info about what we're waiting for
    ink("yellow", 0.5).write("Waiting for BIOS events...", { center: "x", y: screen.height/2 + 100 });
    if (message !== "Drop .als + .wav files") {
      ink("orange", 0.6).write(message, { center: "x", y: screen.height/2 + 120 });
    }
    return;
  }

  // Auto-start audio if playing is true but no audio is actually playing
  // Add throttling to prevent rapid start/stop cycles
  if (isPlaying && wavFile && (preloadedAudio || wavFile.id) && !playingSfx) {
    // Only try auto-start if we haven't tried recently
    const now = performance.now();
    if (!lastAutoStartAttempt || (now - lastAutoStartAttempt) > 1000) { // 1 second throttle
      lastAutoStartAttempt = now;
      console.log("🔄 Auto-starting audio to sync with visualization...");
      try {
        // Use preloaded audio if available, otherwise fallback to ID
        const audioToPlay = preloadedAudio || wavFile.id;
        console.log("🔊 Auto-playing audio:", typeof audioToPlay, !!preloadedAudio ? "preloaded" : "by ID");
        
        playingSfx = sound.play(audioToPlay);
        if (playingSfx) {
          audioStartTime = sound.time;
          console.log("✅ Auto-started audio playback");
        }
      } catch (error) {
        console.error("❌ Auto-start audio error:", error);
      }
    }
  }

  // More stable condition - avoid flickering when sound.time is unreliable
  if (!isPlaying || (!playingSfx && !wavFile)) {
    ink("yellow").write(`"${alsProject.projectName || 'Untitled'}" - ${alsProject.tracks.length} tracks @ ${alsProject.tempo}bpm`, 
      { center: "xy", size: 1 });
    ink("white", 0.7).write("Press space to play audio", { center: "x", y: screen.height/2 + 30, size: 0.8 });
    
    // Enhanced project statistics with duration info
    const calculatedDuration = alsProject.calculateProjectDuration();
    ink("cyan", 0.6).write(`📊 ${alsProject.clips.length} clips • ${alsProject.locators.length} locators • Duration: ${calculatedDuration} beats`, 
      { center: "x", y: screen.height/2 + 60, size: 0.8 });
    
    // Show song structure via locators
    if (alsProject.locators.length > 0) {
      ink("orange", 0.8).write("Song Structure:", { x: 50, y: screen.height/2 + 90, size: 0.9 });
      alsProject.locators.slice(0, 6).forEach((locator, i) => {
        ink("orange", 0.6).write(`• ${locator.name} @ ${locator.time.toFixed(1)}`, 
          { x: 70, y: screen.height/2 + 110 + i * 18, size: 0.7 });
      });
    }
    
    // DRAW STATIC TIMELINE PREVIEW EVEN WHEN NOT PLAYING
    const previewY = screen.height/2 + 200;
    const previewHeight = 80;
    const previewWidth = screen.width - 100;
    const maxBeats = calculatedDuration;
    const beatsPerPixel = maxBeats / previewWidth;
    
    // Preview background
    ink("gray", 0.3).box(50, previewY, previewWidth, previewHeight);
    ink("white", 0.6).write("Timeline Preview (press space to play):", { x: 50, y: previewY - 20 });
    
    // Draw clips in preview
    alsProject.clips.forEach((clip, i) => {
      if (clip.source !== 'arrangement') return;
      
      const clipStart = clip.currentStart || clip.time || 0;
      const clipEnd = clip.currentEnd || (clipStart + (clip.duration || 1));
      
      const x = 50 + clipStart / beatsPerPixel;
      const width = Math.max(1, (clipEnd - clipStart) / beatsPerPixel);
      const y = previewY + 10 + (i % 8) * 8; // Stack clips
      
      // Simple color coding
      let color = [100, 100, 150];
      const name = (clip.name || '').toLowerCase();
      if (name.includes('kick')) color = [200, 100, 100];
      else if (name.includes('snare')) color = [100, 200, 100];
      else if (name.includes('hat')) color = [100, 150, 200];
      
      ink(...color, 0.7).box(x, y, width, 6);
    });
    
    // Draw locators in preview  
    alsProject.locators.forEach(locator => {
      const x = 50 + locator.time / beatsPerPixel;
      ink("yellow", 0.8).line(x, previewY, x, previewY + previewHeight);
      
      // Locator labels
      ink("yellow", 0.6).write(locator.name.substring(0, 6), { 
        x: x + 2, y: previewY - 10, size: 0.6 
      });
    });
    
    return;
  }

  // Enhanced audio synchronization with real-time analysis
  let audioTime = 0;
  let currentBeat = 0;
  let audioAmplitude = 0;
  let audioWaveform = [];
  
  
  if (isPlaying && playingSfx && !playingSfx.killed) {
    // Use actual audio time for perfect sync - validate calculations
    const soundTime = sound.time || 0;
    const startTime = audioStartTime || 0;
    
    if (isNaN(soundTime) || isNaN(startTime)) {
      console.warn("⚠️ Invalid audio timing:", { soundTime, startTime });
      audioTime = 0;
    } else {
      audioTime = soundTime - startTime;
    }
    
    currentBeat = alsProject.getCurrentBeat(audioTime);
    
    // Get real-time audio analysis
    if (sound.speaker && sound.speaker.waveforms) {
      audioWaveform = sound.speaker.waveforms.left || [];
      if (audioWaveform.length > 0) {
        // Calculate RMS amplitude for audio reactivity
        const sum = audioWaveform.reduce((acc, val) => acc + val * val, 0);
        audioAmplitude = Math.sqrt(sum / audioWaveform.length);
      }
    }

    // Reduced console logging to prevent performance issues
    if (Math.floor(audioTime) % 5 === 0 && Math.floor(audioTime * 10) % 10 === 0) {
      console.log(`🎵 SYNC: audioTime=${audioTime.toFixed(2)}s, beat=${currentBeat.toFixed(2)}, tempo=${alsProject.tempo}, amp=${(audioAmplitude*100).toFixed(1)}%`);
    }
  } else if (isPlaying && !wavFile) {
    // ALS-only mode - use performance timing with validation
    const now = performance.now() / 1000;
    const startTime = playStartTime || 0;
    
    if (isNaN(now) || isNaN(startTime)) {
      console.warn("⚠️ Invalid visual timing:", { now, startTime });
      audioTime = 0;
    } else {
      const visualTime = now - startTime;
      audioTime = Math.max(0, visualTime); // Ensure non-negative
    }
    currentBeat = alsProject.getCurrentBeat(audioTime);
  } else if (isPlaying && playingSfx && playingSfx.killed) {
    // Audio was killed but we're still in playing state - clean up
    console.log("🔄 Audio was killed, cleaning up state...");
    playingSfx = null;
    // Don't return here, allow visualization to continue in ALS-only mode
    const now = performance.now() / 1000;
    const startTime = playStartTime || 0;
    
    if (isNaN(now) || isNaN(startTime)) {
      console.warn("⚠️ Invalid fallback timing:", { now, startTime });
      audioTime = 0;
    } else {
      const visualTime = now - startTime;
      audioTime = Math.max(0, visualTime);
    }
    currentBeat = alsProject.getCurrentBeat(audioTime);
  }  // Beat-based visual effects
  const beatPhase = currentBeat % 1;
  const beatFlash = beatPhase < 0.1 ? (1 - beatPhase * 10) * 0.5 : 0;
  const barPhase = (currentBeat % 4) / 4;
  const barFlash = barPhase < 0.025 ? (1 - barPhase * 40) * 0.3 : 0;
  
  // Audio-reactive background pulse
  const pulseIntensity = audioAmplitude * 0.3 + beatFlash + barFlash;
  if (pulseIntensity > 0) {
    ink("white", pulseIntensity * 0.1).box(0, 0, screen.width, screen.height);
  }
  
  // Find current song section from locators with enhanced detection
  const currentSection = alsProject.locators.reduce((prev, curr) => {
    return (curr.time <= currentBeat && curr.time > (prev?.time || -1)) ? curr : prev;
  }, null);
  
  // Get all clips active at current time with timing info
  const activeClips = alsProject.clips.filter(clip => {
    const clipStart = clip.currentStart || clip.time || 0;
    const clipEnd = clip.currentEnd || (clipStart + (clip.duration || 1));
    return currentBeat >= clipStart && currentBeat < clipEnd;
  });
  
  // Enhanced project info with audio sync status
  const syncStatus = wavFile ? `🔊 SYNCED` : `📊 VISUAL`;
  ink("white").write(`${alsProject.projectName} | ${syncStatus} | ${alsProject.tracks.length} tracks @ ${alsProject.tempo}bpm`, 
    { x: 20, y: 20, size: 1 });
  
  // Track Progress Bar - Clear visual indicator of song position
  const trackDuration = alsProject.calculateProjectDuration(); // Use calculated duration
  const progressBarWidth = screen.width - 100;
  const progressBarHeight = 8;
  const progressBarY = 40;
  const currentProgress = Math.min(1, currentBeat / trackDuration);
  
  // Progress bar background
  ink("gray", 0.4).box(20, progressBarY, progressBarWidth, progressBarHeight);
  
  // Filled progress
  const filledWidth = currentProgress * progressBarWidth;
  ink("lime", 0.8 + audioAmplitude * 0.2).box(20, progressBarY, filledWidth, progressBarHeight);
  
  // Progress markers every 16 beats
  for (let beat = 0; beat <= trackDuration; beat += 16) {
    const markerX = 20 + (beat / trackDuration) * progressBarWidth;
    ink("white", 0.6).box(markerX, progressBarY - 2, 1, progressBarHeight + 4);
    ink("white", 0.5).write(beat.toString(), { x: markerX - 8, y: progressBarY - 15 });
  }
  
  // Current position indicator
  const positionX = 20 + currentProgress * progressBarWidth;
  ink("yellow", 0.9 + Math.sin(Date.now() * 0.01) * 0.1).box(positionX - 2, progressBarY - 3, 4, progressBarHeight + 6);
  
  // Progress text
  const progressPercent = Math.round(currentProgress * 100);
  const timeText = `${currentBeat.toFixed(1)}/${trackDuration} beats (${progressPercent}%)`;
  ink("cyan", 0.8).write(timeText, { x: screen.width - 200, y: progressBarY + 2 });
  
  // Beat counter with visual pulse  
  const beatPulseSize = 0.9 + beatFlash * 0.3 + audioAmplitude * 0.5;
  const sectionText = currentSection ? ` | 📍 ${currentSection.name}` : '';
  ink("cyan", 0.8 + beatFlash).write(`♪ ${currentBeat.toFixed(2)}${sectionText} | 🎵 ${activeClips.length} active`, 
    { x: 20, y: 60, size: beatPulseSize });
  
  // Real-time audio level meter
  if (audioAmplitude > 0) {
    const levelWidth = Math.min(300, audioAmplitude * 2000);
    const levelHeight = 12;
    const levelY = 78;
    
    // Background meter
    ink("gray", 0.3).box(20, levelY, 300, levelHeight);
    
    // Active level with color coding
    let levelColor = "green";
    if (audioAmplitude > 0.3) levelColor = "yellow";
    if (audioAmplitude > 0.6) levelColor = "red";
    
    ink(levelColor, 0.8).box(20, levelY, levelWidth, levelHeight);
    
    // Peak indicators
    if (audioAmplitude > 0.5) {
      ink("white", audioAmplitude).write("PEAK", { x: levelWidth + 25, y: levelY + 2 });
    }
  }
  
  // Waveform visualization (small but responsive)
  if (audioWaveform.length > 0) {
    const waveY = 95;
    const waveHeight = 30;
    const waveWidth = Math.min(400, screen.width - 40);
    const step = Math.floor(audioWaveform.length / waveWidth);
    
    ink("cyan", 0.3).box(20, waveY, waveWidth, waveHeight);
    
    for (let i = 0; i < waveWidth; i += 2) {
      const sampleIndex = i * step;
      if (sampleIndex < audioWaveform.length) {
        const amplitude = audioWaveform[sampleIndex];
        const barHeight = Math.abs(amplitude) * waveHeight * 0.8;
        const y = waveY + waveHeight/2 - barHeight/2;
        
        ink("cyan", 0.6 + Math.abs(amplitude) * 0.4).box(20 + i, y, 1, barHeight);
      }
    }
  }
  
  // Real-time activity visualization - focus on what's happening NOW
  if (useSimpleTimeline) {
    // Use new minimal drum visualization to check time sync
    alsProject.drawMinimalDrumVisualization(ink, screen, audioTime, isPlaying);
  } else {
    // Use complex timeline with all the bells and whistles (for advanced users)
    // Enhanced timeline visualization with audio-reactive elements
    const complexTimelineY = 130;
    const complexTimelineHeight = screen.height - 250;
  const complexBarHeight = 32; // Increased from 20 to 32 for even taller tracks with better MIDI visibility
  
  // DEBUG: Timeline drawing status (reduced logging)
  if (Math.floor(currentBeat) % 8 === 0 && Math.floor(currentBeat * 4) % 4 === 0) {
    console.log(`🎨 Drawing timeline: Y=${complexTimelineY}, height=${complexTimelineHeight}, beat=${currentBeat.toFixed(2)}, playing=${isPlaying}`);
  }
  
  // Calculate smart view window (improved scrolling behavior)
  const viewBeats = 32; // Show more context
  const maxBeat = alsProject.calculateProjectDuration(); // Use calculated duration
  
  // Improved scrolling: start immediately when playing, no delay
  let viewStart;
  
  // Validate currentBeat before using it in calculations
  if (isNaN(currentBeat)) {
    console.warn("⚠️ currentBeat is NaN, resetting to 0");
    currentBeat = 0;
  }
  
  if (currentBeat < 2) {
    // Stay at the beginning for just the first 2 beats
    viewStart = 0;
  } else {
    // Start following immediately, keep playhead at 25% from left edge for context
    viewStart = Math.max(0, currentBeat - viewBeats * 0.25);
  }
  
  const viewEnd = Math.min(maxBeat, viewStart + viewBeats);
  const beatWidth = (screen.width - 140) / viewBeats; // Account for wider track labels (140 instead of 60)
  
  // Validate calculations before proceeding
  if (isNaN(viewStart) || isNaN(viewEnd) || isNaN(beatWidth) || isNaN(maxBeat)) {
    console.warn("⚠️ Invalid timeline calculations:", { 
      viewStart, viewEnd, beatWidth, maxBeat, viewBeats, 
      screenWidth: screen.width, currentBeat 
    });
    return; // Skip timeline drawing if calculations are invalid
  }
  
  // Background timeline
  ink("gray", 0.2).box(20, complexTimelineY, screen.width - 40, complexTimelineHeight);
  
  // Add vertical grid lines for better timing reference
  for (let beat = Math.floor(viewStart); beat <= Math.ceil(viewEnd); beat += 1) {
    if (beat >= viewStart && beat <= viewEnd) {
      const gridX = 20 + (beat - viewStart) * beatWidth;
      
      // Validate coordinates before drawing
      if (isNaN(gridX) || isNaN(complexTimelineY) || isNaN(complexTimelineHeight)) {
        console.warn("⚠️ Invalid grid line coordinates:", { gridX, complexTimelineY, complexTimelineHeight });
        continue;
      }
      
      // Different line styles for different beat intervals
      if (beat % 16 === 0) {
        // Every 16 beats - prominent lines with labels
        ink("white", 0.4).line(gridX, complexTimelineY, gridX, complexTimelineY + complexTimelineHeight);
        ink("white", 0.6).write(beat.toString(), { 
          x: gridX + 1, y: complexTimelineY - 12, size: 0.6
        });
      } else if (beat % 4 === 0) {
        // Every 4 beats - medium lines with smaller labels
        ink("white", 0.25).line(gridX, complexTimelineY, gridX, complexTimelineY + complexTimelineHeight);
        ink("white", 0.3).write(beat.toString(), { 
          x: gridX + 1, y: complexTimelineY - 8, size: 0.5
        });
      } else {
        // Every beat - subtle lines
        ink("white", 0.1).line(gridX, complexTimelineY, gridX, complexTimelineY + complexTimelineHeight);
      }
    }
  }
  
  // Draw locator sections as duration boxes (behind clips)
  const sortedLocators = [...alsProject.locators].sort((a, b) => a.time - b.time);
  
  for (let i = 0; i < sortedLocators.length; i++) {
    const locator = sortedLocators[i];
    const nextLocator = sortedLocators[i + 1];
    
    // Calculate section duration with validation
    const sectionStart = locator.time || 0;
    const sectionEnd = nextLocator ? (nextLocator.time || 0) : Math.min(maxBeat, sectionStart + 32); // Default 32 beat sections
    
    // Validate section values
    if (isNaN(sectionStart) || isNaN(sectionEnd)) {
      console.warn("⚠️ Invalid section timing:", { sectionStart, sectionEnd, locator: locator.name });
      continue;
    }
    
    // Only draw if section is visible in current view
    if (sectionEnd >= viewStart && sectionStart <= viewEnd) {
      const sectionX = 20 + Math.max(0, (sectionStart - viewStart) * beatWidth);
      const sectionEndX = 20 + Math.min(viewBeats * beatWidth, (sectionEnd - viewStart) * beatWidth);
      const sectionWidth = sectionEndX - sectionX;
      
      // Validate section coordinates
      if (isNaN(sectionX) || isNaN(sectionEndX) || isNaN(sectionWidth)) {
        console.warn("⚠️ Invalid section coordinates:", { sectionX, sectionEndX, sectionWidth });
        continue;
      }
      
      if (sectionWidth > 0) {
        // Determine section color and style
        const isCurrentSection = locator === currentSection;
        const sectionName = locator.name.toLowerCase();
        
        // Color coding for different section types
        let sectionColor = [60, 60, 80]; // Default blue-gray
        let alpha = 0.15;
        
        if (sectionName.includes('act')) {
          sectionColor = [80, 60, 120]; // Purple for acts
          alpha = 0.2;
        } else if (sectionName.includes('pause')) {
          sectionColor = [80, 80, 60]; // Yellow-gray for pauses
          alpha = 0.1;
        } else if (sectionName.includes('surprise')) {
          sectionColor = [120, 60, 60]; // Red for surprises
          alpha = 0.25;
        } else if (sectionName.includes('end')) {
          sectionColor = [60, 80, 60]; // Green for endings
          alpha = 0.2;
        }
        
        // Highlight current section
        if (isCurrentSection) {
          sectionColor = sectionColor.map(c => c * 1.5);
          alpha *= 2;
        }
        
        // Draw section background box
        ink(...sectionColor, alpha).box(sectionX, complexTimelineY, sectionWidth, complexTimelineHeight);
        
        // Draw section border with validation
        const borderAlpha = isCurrentSection ? 0.6 : 0.3;
        if (!isNaN(sectionX) && !isNaN(complexTimelineY) && !isNaN(complexTimelineHeight)) {
          ink(...sectionColor, borderAlpha).line(sectionX, complexTimelineY, sectionX, complexTimelineY + complexTimelineHeight); // Left border
        } else {
          console.warn("⚠️ Invalid section border coordinates:", { sectionX, complexTimelineY, complexTimelineHeight });
        }
        
        // Section label with duration
        const duration = (sectionEnd - sectionStart).toFixed(1);
        const labelText = `${locator.name} (${duration}b)`;
        
        // Only show label if there's enough space
        if (sectionWidth > 60) {
          const textColor = isCurrentSection ? "yellow" : "white";
          const textAlpha = isCurrentSection ? 0.9 : 0.6;
          
          ink(textColor, textAlpha).write(labelText, { 
            x: sectionX + 4, 
            y: complexTimelineY - 15,
            size: 0.8
          });
          
          // Show section progress bar if it's the current section
          if (isCurrentSection && currentBeat >= sectionStart && currentBeat < sectionEnd) {
            const sectionProgress = (currentBeat - sectionStart) / (sectionEnd - sectionStart);
            const progressWidth = sectionWidth * sectionProgress;
            
            ink("yellow", 0.3).box(sectionX, complexTimelineY + complexTimelineHeight - 4, progressWidth, 4);
          }
        }
      }
    }
  }
  
  // Define timeline variables for clip rendering (needed when useSimpleTimeline is true)
  const timelineY = useSimpleTimeline ? (screen.height - 150 - 10) : 130;
  const timelineHeight = useSimpleTimeline ? 150 : (screen.height - 250);
  const barHeight = 32;
  
  // Simple, stable track assignment - create a consistent mapping
  const clipTrackMap = new Map(); // Stable track assignment for each clip
  const trackNames = new Map(); // Track names
  let nextTrack = 0;
  
  // Sort clips by their original order to ensure consistent assignment
  const sortedClips = [...alsProject.clips]
    .filter(clip => clip.source === 'arrangement')
    .sort((a, b) => {
      // Sort by track reference first, then by start time, then by index
      const aTrack = a.trackRef || a.trackIndex || 0;
      const bTrack = b.trackRef || b.trackIndex || 0;
      if (aTrack !== bTrack) return aTrack.toString().localeCompare(bTrack.toString());
      
      const aStart = a.currentStart || a.time || 0;
      const bStart = b.currentStart || b.time || 0;
      if (aStart !== bStart) return aStart - bStart;
      
      return (a.id || 0) - (b.id || 0);
    });
  
  // Assign each unique track reference to a visual track with proper names
  sortedClips.forEach(clip => {
    const trackRef = clip.trackRef || clip.trackIndex || 'default';
    
    if (!clipTrackMap.has(trackRef)) {
      clipTrackMap.set(trackRef, nextTrack);
      
      // Get the actual track name from the project
      let trackName = `Track ${nextTrack + 1}`;
      const actualTrack = alsProject.tracks.find(t => t.id === trackRef || t.trackIndex === trackRef);
      
      if (actualTrack && actualTrack.name) {
        trackName = actualTrack.name.substring(0, 12); // Longer names for better identification
      } else {
        // Fallback: try to infer from clip names in this track
        const trackClips = alsProject.clips.filter(c => 
          (c.trackRef || c.trackIndex) === trackRef && c.name);
        
        if (trackClips.length > 0) {
          const firstClipName = trackClips[0].name.toLowerCase();
          if (firstClipName.includes('kick') || firstClipName.includes('bd')) trackName = 'Kick';
          else if (firstClipName.includes('snare') || firstClipName.includes('drum')) trackName = 'Snare';
          else if (firstClipName.includes('hat') || firstClipName.includes('hi')) trackName = 'Hi-Hats';
          else if (firstClipName.includes('bass')) trackName = 'Bass';
          else if (firstClipName.includes('synth') || firstClipName.includes('lead')) trackName = 'Synth';
          else if (firstClipName.includes('vocal') || firstClipName.includes('vox')) trackName = 'Vocals';
          else if (firstClipName.includes('piano')) trackName = 'Piano';
          else if (firstClipName.includes('guitar')) trackName = 'Guitar';
          else trackName = `Track ${nextTrack + 1}`;
        }
      }
      
      trackNames.set(nextTrack, trackName);
      console.log(`🎵 Assigned track ${nextTrack}: "${trackName}" (ref: ${trackRef})`);
      nextTrack++;
    }
  });
  
  alsProject.clips.forEach((clip, i) => {
    if (clip.source !== 'arrangement') return;
    
    const clipStart = clip.currentStart || clip.time || 0;
    const clipEnd = clip.currentEnd || (clipStart + (clip.duration || 1));
    
    // Only draw clips that intersect with the current view window
    if (clipEnd < viewStart || clipStart > viewEnd) return;

    // Calculate visible portion of the clip
    const visibleStart = Math.max(clipStart, viewStart);
    const visibleEnd = Math.min(clipEnd, viewEnd);
    
    const x = 90 + (visibleStart - viewStart) * beatWidth; // Start after track labels (90px)
    const width = Math.max(1, (visibleEnd - visibleStart) * beatWidth);
    
    // Debug timing for first few clips to verify units
    if (i < 3) {
      console.log(`🎵 Clip ${i} "${clip.name}": start=${clipStart}, end=${clipEnd}, visibleStart=${visibleStart}, visibleEnd=${visibleEnd}, x=${x.toFixed(1)}, width=${width.toFixed(1)}`);
    }
    
    // Get stable track assignment
    const trackRef = clip.trackRef || clip.trackIndex || 'default';
    const visualTrackIndex = clipTrackMap.get(trackRef) || 0;
    
    // Ensure we don't exceed visual space with proper spacing
    const trackSpacing = 6; // Increased spacing for taller tracks
    const maxVisualTracks = Math.floor((timelineHeight - 40) / (barHeight + trackSpacing));
    const finalTrackIndex = visualTrackIndex % maxVisualTracks;
    
    const y = timelineY + 20 + finalTrackIndex * (barHeight + trackSpacing);
    
    // Debug track assignment for first few clips
    if (i < 3) {
      console.log(`🎯 Clip ${i} track: ref=${trackRef}, visual=${visualTrackIndex}, final=${finalTrackIndex}, y=${y}`);
    }
    
    // Check if this clip is currently playing/active
    const isActive = currentBeat >= clipStart && currentBeat < clipEnd;
    
    // Enhanced color coding based on musical content with vibrant named colors
    let inkColor = "gray"; // Default color
    const name = (clip.name || '').toLowerCase();
    
    if (clip.notes && clip.notes.length > 0) {
      // MIDI clips - color by note density and pitch range
      const avgPitch = clip.notes.reduce((sum, note) => sum + note.pitch, 0) / clip.notes.length;
      
      if (avgPitch < 50) inkColor = "red"; // Low = red (bass/kick)
      else if (avgPitch < 70) inkColor = "lime"; // Mid = lime (snare/mid)
      else inkColor = "cyan"; // High = cyan (hats/leads)
    } else {
      // Audio clips - enhanced color by name patterns
      if (name.includes('kick') || name.includes('bd')) inkColor = "red"; // Kick = red
      else if (name.includes('snare') || name.includes('drum')) inkColor = "lime"; // Snare = lime
      else if (name.includes('hat') || name.includes('hi')) inkColor = "cyan"; // Hats = cyan
      else if (name.includes('bass')) inkColor = "yellow"; // Bass = yellow
      else if (name.includes('lead') || name.includes('synth')) inkColor = "magenta"; // Synths = magenta
      else if (name.includes('vocal') || name.includes('vox')) inkColor = "green"; // Vocals = green
      else inkColor = "blue"; // Generic audio = blue
    }
    
    // Adjust brightness for active clips with pulsing effects
    let clipAlpha = 0.6;
    let finalColor = inkColor;
    
    if (isActive) {
      // Create pulsing effect for active clips
      const pulse = Math.sin(Date.now() * 0.01) * 0.3 + 0.7; // Slow pulse
      const beatPulse = beatPhase < 0.1 ? (1 - beatPhase * 10) : 0; // Quick flash on beats
      
      clipAlpha = 0.7 + pulse * 0.3 + beatPulse * 0.2;
      
      // Flash yellow on beat start
      if (beatPulse > 0.5) {
        finalColor = "yellow";
        clipAlpha = 1.0;
      }
      
      // Add red blink for clip starts (when clip just became active)
      const timeSinceClipStart = currentBeat - clipStart;
      if (timeSinceClipStart < 0.5) {
        const blinkIntensity = Math.sin(timeSinceClipStart * 20) * 0.5 + 0.5;
        if (blinkIntensity > 0.7) {
          finalColor = "red";
          clipAlpha = 1.0;
        }
      }
      
      // Add glow effect around active clips using the same color
      ink(finalColor, 0.3).box(x - 1, y - 1, width + 2, barHeight + 2); // Glow
      
      // Show individual MIDI notes firing (for MIDI clips with notes)
      if (clip.notes && clip.notes.length > 0) {
        clip.notes.forEach(note => {
          const noteTime = (note.time || 0) + clipStart;
          const noteDuration = note.duration || 0.25;
          
          // Check if this note should be playing now
          if (currentBeat >= noteTime && currentBeat <= noteTime + noteDuration) {
            const noteX = x + ((noteTime - clipStart) / (clipEnd - clipStart)) * width;
            const notePitch = note.pitch || 60;
            
            // Visual note flash based on pitch
            let noteColor = "white";
            if (notePitch < 40) noteColor = "red"; // Low notes = red
            else if (notePitch < 60) noteColor = "lime"; // Mid notes = lime
            else if (notePitch < 80) noteColor = "cyan"; // High notes = cyan
            else noteColor = "yellow"; // Very high = yellow
            
            // Note flash intensity based on velocity and timing
            const noteProgress = (currentBeat - noteTime) / noteDuration;
            const noteIntensity = Math.max(0.3, 1 - noteProgress) * (note.velocity || 100) / 127;
            
            // Draw firing note indicator
            ink(noteColor, noteIntensity).box(
              Math.max(x, noteX - 1), y - 2, 
              Math.min(3, width - (noteX - x)), barHeight + 4
            );
            
            // Add note spark effect for recently triggered notes
            const timeSinceStart = currentBeat - noteTime;
            if (timeSinceStart < 0.25) {
              const sparkSize = (1 - timeSinceStart * 4) * 3;
              ink(noteColor, 0.8).box(noteX - sparkSize/2, y - sparkSize, sparkSize, sparkSize);
            }
          }
        });
      }
      
      // Show audio event flashes (for audio clips)
      if (!clip.notes || clip.notes.length === 0) {
        // Detect audio events based on clip name patterns and beat timing
        const beatInClip = (currentBeat - clipStart) % 1;
        
        // Hi-hat flashes (every beat or half beat for hats)
        if (name.includes('hat') || name.includes('hi')) {
          if (beatInClip < 0.1 || (beatInClip > 0.4 && beatInClip < 0.6)) {
            const flashIntensity = beatInClip < 0.1 ? (1 - beatInClip * 10) : (1 - (beatInClip - 0.5) * 20);
            ink("cyan", flashIntensity * 0.8).box(x, y - 2, width, barHeight + 4);
          }
        }
        
        // Kick flashes (strong on beat 1)
        else if (name.includes('kick') || name.includes('bd')) {
          const beatPosition = Math.floor(currentBeat - clipStart) % 4;
          if (beatInClip < 0.2 && (beatPosition === 0 || beatPosition === 2)) {
            const flashIntensity = (1 - beatInClip * 5) * audioAmplitude * 2;
            ink("red", flashIntensity).box(x - 2, y - 3, width + 4, barHeight + 6);
          }
        }
        
        // Snare flashes (beat 2 and 4)
        else if (name.includes('snare') || name.includes('sd')) {
          const beatPosition = Math.floor(currentBeat - clipStart) % 4;
          if (beatInClip < 0.15 && (beatPosition === 1 || beatPosition === 3)) {
            const flashIntensity = (1 - beatInClip * 6.67) * 0.8;
            ink("lime", flashIntensity).box(x, y - 2, width, barHeight + 4);
          }
        }
        
        // Bass flashes (longer, sustained)
        else if (name.includes('bass')) {
          if (beatInClip < 0.5) {
            const flashIntensity = Math.sin(beatInClip * Math.PI) * 0.6;
            ink("yellow", flashIntensity).box(x, y - 1, width, barHeight + 2);
          }
        }
        
        // Generic audio response to amplitude
        else if (audioAmplitude > 0.2) {
          ink("blue", audioAmplitude * 0.4).box(x, y, width, barHeight);
        }
      }
    }
    
    // Draw main clip box with enhanced border and active highlighting
    ink(finalColor, clipAlpha).box(x, y, width, barHeight);
    
    // MIDI note grid visualization for clips with notes - ENHANCED CONTRAST
    if (clip.notes && clip.notes.length > 0 && width > 10) {
      const noteGridHeight = barHeight - 6; // Leave more room for borders
      const noteGridY = y + 3;
      
      // Draw dark background for MIDI note area for better contrast
      ink(10, 10, 20, 0.8).box(x, noteGridY, width, noteGridHeight);
      
      // Determine pitch range for this clip
      const pitches = clip.notes.map(note => note.pitch || 60);
      const minPitch = Math.min(...pitches);
      const maxPitch = Math.max(...pitches);
      const pitchRange = Math.max(24, maxPitch - minPitch); // At least two octaves for better spread
      
      // Draw MIDI notes as prominent rectangles
      clip.notes.forEach(note => {
        const noteTime = (note.time || 0) + clipStart;
        const noteDuration = note.duration || 0.25;
        const notePitch = note.pitch || 60;
        const noteVelocity = note.velocity || 100;
        
        // Check if note is within visible clip portion
        if (noteTime >= visibleStart && noteTime <= visibleEnd) {
          // Calculate note position within the visible clip
          const noteStartInClip = noteTime - visibleStart;
          const noteX = x + (noteStartInClip / (visibleEnd - visibleStart)) * width;
          const noteWidth = Math.max(2, (noteDuration / (visibleEnd - visibleStart)) * width);
          
          // Calculate note Y position based on pitch (higher pitch = higher on screen)
          const pitchNormalized = (notePitch - minPitch) / pitchRange;
          const noteY = noteGridY + (1 - pitchNormalized) * noteGridHeight;
          const noteHeight = Math.max(3, noteGridHeight / 16); // Taller notes for visibility
          
          // Note color and alpha based on velocity and current playback
          const isNotePlaying = currentBeat >= noteTime && currentBeat <= noteTime + noteDuration;
          let noteAlpha = Math.max(0.6, (noteVelocity / 127) * 1.0); // Higher base opacity
          let noteColor = "white";
          
          if (isNotePlaying) {
            noteAlpha = 1.0;
            noteColor = "yellow"; // Bright yellow for currently playing notes
            
            // Add bright sparkle effect for playing notes
            const sparkleSize = 3;
            ink("white", 1.0).box(noteX - sparkleSize, noteY - sparkleSize, 
                                  noteWidth + sparkleSize * 2, noteHeight + sparkleSize * 2);
          } else {
            // High contrast colors based on pitch range
            if (notePitch < 40) noteColor = "red";      // Bass notes - red
            else if (notePitch < 55) noteColor = "orange";  // Low-mid - orange  
            else if (notePitch < 70) noteColor = "lime";    // Mid - lime
            else if (notePitch < 85) noteColor = "cyan";    // High-mid - cyan
            else noteColor = "magenta";                     // High notes - magenta
          }
          
          // Draw the note rectangle with high contrast
          ink(noteColor, noteAlpha).box(Math.floor(noteX), Math.floor(noteY), 
                                      Math.max(2, Math.floor(noteWidth)), Math.max(3, Math.floor(noteHeight)));
          
          // Add bright outline for even better visibility
          if (noteWidth >= 3 && noteHeight >= 4) {
            ink("white", 0.6).box(Math.floor(noteX), Math.floor(noteY), 
                                Math.max(2, Math.floor(noteWidth)), Math.max(3, Math.floor(noteHeight)), "outline");
          }
        }
      });
      
      // Draw bright octave grid lines for reference
      for (let octave = 0; octave <= pitchRange / 12; octave++) {
        const gridPitch = minPitch + octave * 12;
        const gridY = noteGridY + (1 - (gridPitch - minPitch) / pitchRange) * noteGridHeight;
        if (gridY >= noteGridY && gridY <= noteGridY + noteGridHeight && !isNaN(gridY)) {
          ink("white", 0.4).line(x, gridY, x + width, gridY);
        }
      }
    }
    
    // DEBUG: Show clip drawing details for first few clips
    if (i < 3) {
      console.log(`🎨 Drawing clip ${i}: x=${x.toFixed(1)}, y=${y}, w=${width.toFixed(1)}, h=${barHeight}, color=${finalColor}, alpha=${clipAlpha.toFixed(2)}, notes=${clip.notes?.length || 0}`);
    }
    
    // Add complete border for better definition (brighter for active clips)
    const borderAlpha = isActive ? 0.8 : 0.5;
    
    // Validate coordinates before drawing borders
    if (!isNaN(x) && !isNaN(y) && !isNaN(width) && !isNaN(barHeight)) {
      // Draw full border - top, bottom, left, right
      ink("white", borderAlpha).line(x, y, x + width, y); // Top border
      ink("white", borderAlpha).line(x, y + barHeight, x + width, y + barHeight); // Bottom border  
      ink("white", borderAlpha * 0.9).line(x, y, x, y + barHeight); // Left border
      ink("white", borderAlpha * 0.75).line(x + width, y, x + width, y + barHeight); // Right border
    } else {
      console.warn("⚠️ Invalid clip border coordinates:", { x, y, width, barHeight });
    }
    
    // Show MIDI note density as small indicators along the clip
    if (clip.notes && clip.notes.length > 0 && width > 4) {
      const noteIndicatorHeight = 2;
      const noteY = y + barHeight - noteIndicatorHeight;
      
      // Group notes by position within the clip for visualization
      const notesPerBeat = {};
      clip.notes.forEach(note => {
        const notePositionInClip = (note.time || 0);
        const beatPosition = Math.floor(notePositionInClip);
        if (!notesPerBeat[beatPosition]) notesPerBeat[beatPosition] = 0;
        notesPerBeat[beatPosition]++;
      });
      
      // Draw note density indicators
      Object.keys(notesPerBeat).forEach(beatPos => {
        const beat = parseInt(beatPos);
        const noteCount = notesPerBeat[beat];
        const indicatorX = x + (beat / (clipEnd - clipStart)) * width;
        
        if (indicatorX >= x && indicatorX <= x + width) {
          const intensity = Math.min(1, noteCount / 5); // Scale by note count
          ink(255, 255, 100, intensity * 0.8).box(indicatorX, noteY, 2, noteIndicatorHeight);
        }
      });
    }
    
    // Show clip name for longer clips  
    if (width > 30 && barHeight >= 6) {
      ink("white", 0.8).write(clip.name || `C${i}`, { 
        x: x + 2, y: y + 1
      });
    }
  });
  
  // Draw scrolling track labels that move with the clips
  const visibleTracks = new Set();
  
  // Determine which tracks are actually visible in current view
  alsProject.clips.forEach((clip) => {
    if (clip.source !== 'arrangement') return;
    
    const clipStart = clip.currentStart || clip.time || 0;
    const clipEnd = clip.currentEnd || (clipStart + (clip.duration || 1));
    if (clipEnd < viewStart || clipStart > viewEnd) return;
    
    const trackRef = clip.trackRef || clip.trackIndex || 'default';
    const visualTrackIndex = clipTrackMap.get(trackRef) || 0;
    const trackSpacing = 6; // Match the spacing used in clip drawing
    const maxVisualTracks = Math.floor((timelineHeight - 40) / (barHeight + trackSpacing));
    const finalTrackIndex = visualTrackIndex % maxVisualTracks;
    
    visibleTracks.add(finalTrackIndex);
  });
  
  // Draw track labels only for visible tracks - positioned on the left
  visibleTracks.forEach((trackIndex) => {
    const trackSpacing = 6; // Match the spacing used in clip drawing
    const y = timelineY + 20 + trackIndex * (barHeight + trackSpacing);
    const trackName = trackNames.get(trackIndex) || `Track ${trackIndex + 1}`;
    
    // Draw fixed track labels on the left side (not scrolling)
    const labelX = 2;
    const labelWidth = 80; // Wider labels for longer track names
    
    // Draw label background with better contrast
    ink(30, 30, 40, 0.95).box(labelX, y - 2, labelWidth, barHeight + 4);
    
    // Draw label border
    ink(100, 100, 120, 0.8).box(labelX, y - 2, labelWidth, barHeight + 4, "outline");
    
    // Draw label text with high contrast
    ink(255, 255, 255, 1.0).write(trackName, { 
      x: labelX + 4, y: y + barHeight/2 - 4, size: 0.8 
    });
  });
  
  // PROMINENT CURRENT PLAYHEAD - Make it very visible!
  const playheadX = 20 + (currentBeat - viewStart) * beatWidth;
  
  // DEBUG: Playhead position
  console.log(`� Playhead: x=${playheadX.toFixed(1)}, currentBeat=${currentBeat.toFixed(2)}, viewStart=${viewStart.toFixed(1)}, beatWidth=${beatWidth.toFixed(2)}`);
  
  // Only draw playhead if it's within the visible timeline
  if (playheadX >= 20 && playheadX <= screen.width - 40) {
    const flashIntensity = beatPhase < 0.1 ? (1 - beatPhase * 10) : 0;
    
    // Add audio amplitude to playhead intensity
    const audioIntensity = audioAmplitude * 0.5;
    const totalIntensity = Math.min(1, flashIntensity + audioIntensity);
    
    // Main playhead line - make it thicker and more prominent
    const lineThickness = 3;
    for (let i = 0; i < lineThickness; i++) {
      ink(255, 200 + totalIntensity * 55, 100, 0.9 + totalIntensity * 0.1).line(
        playheadX - 1 + i, timelineY - 15, 
        playheadX - 1 + i, timelineY + timelineHeight + 15
      );
    }
    
    // Audio-reactive playhead glow - make it wider
    if (audioAmplitude > 0.05) {
      const glowWidth = 6 + audioAmplitude * 10;
      ink(255, 255, 100, audioAmplitude * 0.2).box(
        playheadX - glowWidth/2, timelineY - 15, 
        glowWidth, timelineHeight + 30
      );
    }
    
    // Beat flash effect
    if (beatPhase < 0.2) {
      const flashSize = (1 - beatPhase * 5) * 8;
      ink(255, 255, 255, flashIntensity * 0.6).box(
        playheadX - flashSize/2, timelineY - 5, 
        flashSize, timelineHeight + 10
      );
    }
    
    // Playhead position indicator (small triangle at top)
    ink("yellow", 0.9).box(playheadX - 3, timelineY - 18, 6, 3);
  }
  
  // Enhanced beat grid markers (more subtle since we have section boxes)
  for (let beat = Math.floor(viewStart/4)*4; beat <= viewEnd; beat += 4) {
    if (beat >= viewStart && beat <= viewEnd) {
      const x = 20 + (beat - viewStart) * beatWidth;
      
      // Simple beat markers every 4 beats
      const isAtSectionStart = alsProject.locators.some(loc => Math.abs(loc.time - beat) < 0.5);
      
      if (isAtSectionStart) {
        // Section boundary markers - more prominent
        ink("cyan", 0.5 + audioAmplitude * 0.2).line(x, timelineY - 5, x, timelineY + timelineHeight + 5);
      } else {
        // Regular beat markers - subtle
        ink("white", 0.2).line(x, timelineY, x, timelineY + timelineHeight);
      }
      
      // Beat numbers (every 16 beats to avoid clutter)
      if (beat % 16 === 0) {
        ink("white", 0.4).write(beat.toString(), { 
          x: x + 2, y: timelineY + timelineHeight + 5,
          size: 0.7
        });
      }
    }
  }
  
  // Active clips info with enhanced details
  const infoY = screen.height - 120;
  ink("white").write("Active Now:", { x: 20, y: infoY, size: 1 });
  
  activeClips.slice(0, 6).forEach((clip, i) => {
    const x = 20;
    const y = infoY + 20 + i * 15;
    const name = clip.name || `Clip ${clip.id}`;
    const noteInfo = clip.notes ? ` (${clip.notes.length} notes)` : '';
    const timeInfo = ` [${clip.currentStart?.toFixed(1) || clip.time?.toFixed(1) || '?'}-${clip.currentEnd?.toFixed(1) || '?'}]`;
    
    ink("yellow", 0.8).write(`• ${name}${noteInfo}${timeInfo}`, { x, y });
  });
  
  // Timeline position indicator with musical structure awareness
  const currentBar = Math.floor(currentBeat / 4) + 1;
  const currentBeatInBar = (currentBeat % 4) + 1;
  const nextSection = alsProject.locators.find(loc => loc.time > currentBeat);
  const nextSectionDistance = nextSection ? (nextSection.time - currentBeat).toFixed(1) : "∞";
  
  // Show timeline mode (static vs following)
  const isFollowing = currentBeat >= 8; // Match the new scrolling threshold
  const modeText = isFollowing ? "📍 FOLLOWING" : "📌 FIXED";
  
  ink("cyan", 0.6).write(`View: ${viewStart.toFixed(1)}-${viewEnd.toFixed(1)} beats | ${modeText}`, 
    { x: screen.width - 250, y: screen.height - 50 });
  
  ink("yellow", 0.8).write(`Bar ${currentBar}, Beat ${currentBeatInBar.toFixed(1)}`, 
    { x: screen.width - 250, y: screen.height - 35 });
  
  if (nextSection) {
    ink("cyan", 0.7 + audioAmplitude * 0.2).write(`Next: ${nextSection.name} in ${nextSectionDistance}b`, 
      { x: screen.width - 250, y: screen.height - 20 });
  }
  } // End of complex timeline else block
  
  // Show timeline mode indicator
  if (useSimpleTimeline) {
    ink("lime", 0.8).write("SIMPLE MODE (T to toggle)", 
      { x: 20, y: screen.height - 40, font: "MatrixChunky8" });
  } else {
    ink("orange", 0.8).write("COMPLEX MODE (T to toggle)", 
      { x: 20, y: screen.height - 40, font: "MatrixChunky8" });
  }
}

function act({ event: e, sound, pen, clock, send }) {
  // Debug all events to see what we're getting
  if (e.name !== "move" && e.name !== "draw") {
    console.log("🔍 Act event:", e.name || e.type || "unknown", Object.keys(e));
    
    // ENHANCED: Look for any event that might contain ALS data
    if (JSON.stringify(e).toLowerCase().includes('als')) {
      console.log("🎵 ABLETON.MJS: Event contains 'als' keyword:", e);
    }
    
    // Check if this event has any content that looks like file data
    if (e.content || e.data || e.xmlData) {
      console.log("🎵 ABLETON.MJS: Event has content/data:", Object.keys(e));
    }
  }
  
  // ENHANCED: File handling is now done through the receive() function
  // which properly processes dropped ALS and WAV files
  
  // ENHANCED: Look for any ALS-related events
  if (e.type === "dropped:als" || e.name === "dropped:als" || (e.name && e.name.includes("als"))) {
    console.log("🎵 ABLETON.MJS: ALS event detected in act:", e);
    
    if (e.content && e.content.xmlData) {
      console.log("🎵 ABLETON.MJS: Found xmlData in act event, creating project...");
      alsProject = new ALSProject(e.content.xmlData);
      alsProject.projectName = e.content.name;
      message = `ALS loaded: ${alsProject.tracks.length} tracks @ ${alsProject.tempo}bpm`;
      console.log("✅ ABLETON.MJS: ALS Project loaded via act:", alsProject);
      return;
    }
  }
  
  // ENHANCED: Look for any WAV-related events
  if (e.type === "dropped:wav" || e.name === "dropped:wav" || (e.name && e.name.includes("wav"))) {
    console.log("🔊 ABLETON.MJS: WAV event detected in act:", e);
    
    if (e.content) {
      wavFile = {
        name: e.content.name,
        size: e.content.size || 0,
        id: e.content.id
      };
      message = `WAV ready: ${wavFile.name}`;
      console.log("✅ ABLETON.MJS: WAV loaded via act");
      return;
    }
  }
  
  // Enhanced: Check for dropped:als events that might come through act
  if (e.type === "dropped:als" || (e.name && e.name.includes("als"))) {
    console.log("🎵 ALS event detected in act:", e);
    
    if (e.content && e.content.xmlData) {
      console.log("🎵 Found xmlData in act event, creating project...");
      alsProject = new ALSProject(e.content.xmlData);
      alsProject.projectName = e.content.name;
      message = `ALS loaded: ${alsProject.tracks.length} tracks @ ${alsProject.tempo}bpm`;
      console.log("✅ ALS Project loaded via act:", alsProject);
      return;
    }
  }
  
  // Handle ALS file data that comes through act instead of receive
  if (e.xmlData && e.name && !alsProject) {
    console.log("🎵 ALS file received via act:", e.name, "XML length:", e.xmlData.length);
    alsProject = new ALSProject(e.xmlData);
    alsProject.projectName = e.name; // Set project name for tempo extraction
    message = `ALS loaded: ${alsProject.tracks.length} tracks @ ${alsProject.tempo}bpm`;
    console.log("✅ ALS Project loaded via act:", alsProject);
    return;
  }
  
  // Simple click/touch to play/pause
  if (e.is && e.is("touch")) {
    console.log("🎵 Touch detected, toggling playback. Current state:", isPlaying);
    
    if (isPlaying) {
      // Pause
      console.log("⏸️ Pausing playback");
      if (playingSfx) {
        try {
          // Use the proper kill method without passing the object itself
          if (playingSfx.kill) {
            playingSfx.kill();
          }
          playingSfx = null;
        } catch (error) {
          console.error("Error stopping audio:", error);
        }
      }
      isPlaying = false;
      message = "Paused - touch to resume";
      console.log("✅ Audio paused");
    } else {
      // Play
      if (wavFile && wavFile.id) {
        console.log("▶️ Starting playback with WAV file:", wavFile.name);
        console.log("🔊 WAV file ID:", wavFile.id);
        console.log("🔊 Sound object available:", !!sound, "sound.play function:", typeof sound.play);
        
        try {
          // Use the WAV file ID directly (already decoded by BIOS)
          console.log("🔊 Playing WAV file ID:", wavFile.id);
          const playResult = sound.play(wavFile.id);
          
          console.log("🔊 sound.play() returned:", playResult, "type:", typeof playResult);
          
          if (playResult) {
            playingSfx = playResult;
            isPlaying = true;
            playStartTime = performance.now() / 1000;
            audioStartTime = sound.time;
            message = "Playing - touch to pause";
            console.log("✅ Audio playback started - Timestamp:", playStartTime, "Audio time:", audioStartTime);
          } else {
            console.error("❌ Failed to start audio playback - sound.play() returned null/undefined");
            console.log("🔍 Attempting to diagnose audio issue...");
            
            // Try to get sample data to verify the file exists
            if (sound.getSampleData) {
              sound.getSampleData(wavFile.id)
                .then((data) => {
                  console.log("🔍 Sample data found:", !!data);
                  if (!data) {
                    message = `Audio file ${wavFile.name} not ready - try again`;
                  }
                })
                .catch((error) => {
                  console.error("🔍 Sample data error:", error);
                  message = `Audio file ${wavFile.name} failed to load`;
                });
            }
            
            message = "Failed to play audio - file not ready?";
          }
        } catch (error) {
          console.error("❌ Audio playback error:", error);
          console.error("❌ Error details:", error.message, error.stack);
          message = "Failed to play audio - " + error.message;
        }
      } else if (alsProject) {
        // Start ALS visualization without audio
        console.log("🔄 Starting ALS visualizer without audio");
        isPlaying = true;
        playStartTime = performance.now() / 1000; // Convert to seconds
        message = "Playing ALS visualizer - DROP .WAV FILE FOR AUDIO";
      } else {
        message = "No audio or ALS file loaded - drop .als + .wav files to start";
      }
    }
    return; // Important: return early to prevent other event handling
  }
  
  // 🎹 Keyboard shortcut to toggle timeline view (T key)
  if (e.is && e.is("keyboard:down:t")) {
    useSimpleTimeline = !useSimpleTimeline;
    message = useSimpleTimeline ? "Activity View" : "Complex Timeline";
    console.log("🎨 View toggled:", useSimpleTimeline ? "Activity View" : "Complex Timeline");
    return;
  }
  
  // 🎹 Keyboard controls - Space to play/pause
  if (e.is && e.is("keyboard:down:space")) {
    console.log("🎵 Space key detected, toggling playback. Current state:", isPlaying);
    
    if (isPlaying) {
      // Pause
      console.log("⏸️ Pausing playback");
      if (playingSfx) {
        try {
          if (playingSfx.kill) {
            playingSfx.kill();
          }
          playingSfx = null;
        } catch (error) {
          console.error("Error stopping audio:", error);
        }
      }
      isPlaying = false;
      message = "Paused - [Space] or touch to resume";
      console.log("✅ Audio paused via keyboard");
    } else if (wavFile && wavFile.id) {
      // Play with audio
      console.log("▶️ Starting playback with WAV file:", wavFile.name);
      console.log("🔊 WAV file ID:", wavFile.id);
      console.log("🔊 Sound object available:", !!sound, "sound.play function:", typeof sound.play);
      
      try {
        // Use the WAV file ID directly (already decoded by BIOS)
        console.log("🔊 Playing WAV file ID:", wavFile.id);
        const playResult = sound.play(wavFile.id);
        
        console.log("🔊 sound.play() returned:", playResult, "type:", typeof playResult);
        
        if (playResult) {
          playingSfx = playResult;
          isPlaying = true;
          playStartTime = performance.now() / 1000;
          audioStartTime = sound.time;
          message = "Playing - [Space] or touch to pause";
          console.log("✅ Audio playback started via keyboard - Timestamp:", playStartTime, "Audio time:", audioStartTime);
        } else {
          console.error("❌ Failed to start audio playback - sound.play() returned null/undefined");
          console.log("🔍 Checking if file exists in sound system...");
          message = "Failed to play audio - file not found?";
        }
      } catch (error) {
        console.error("❌ Audio playback error:", error);
        console.error("❌ Error details:", error.message, error.stack);
        message = "Failed to play audio - " + error.message;
      }
    } else {
      console.log("🔄 No WAV file loaded yet - starting ALS visualizer without audio");
      if (alsProject) {
        isPlaying = true;
        playStartTime = performance.now() / 1000; // Convert to seconds
        message = "Playing ALS visualizer - DROP .WAV FILE FOR AUDIO";
      } else {
        message = "No audio or ALS file loaded - drop .als + .wav files to start";
      }
    }
    return; // Important: return early to prevent other event handling
  }
  
  // Handle WAV events that come via act with file properties directly on event
  if (e.name && e.size && e.id && !e.is("touch") && !e.is("keyboard:down:space")) {
    console.log("🔊 WAV file received in act:", e.name, "Size:", e.size, "ID:", e.id);
    console.log("🔊 Full WAV event details:", e);
    
    wavFile = {
      name: e.name,
      originalName: e.originalName || e.name,
      size: e.size,
      id: e.id
    };
    
    console.log("🔊 WAV file stored from act:", wavFile);
    
    isPlaying = false;
    if (playingSfx) {
      try {
        if (playingSfx.kill) {
          playingSfx.kill();
        }
        playingSfx = null;
      } catch (error) {
        console.error("Error stopping audio during WAV load:", error);
        playingSfx = null;
      }
    }
    message = `WAV ready: ${wavFile.name} (ID: ${wavFile.id})`;
    console.log("🔊 WAV file ready for playback");
    
    // If ALS is already loaded, start playing
    if (alsProject) {
      console.log("🎵 ALS already loaded, ready to play both!");
      message = `Ready: ${alsProject.projectName} + ${wavFile.name} - Press [Space] or touch to play`;
    }
    
    return;
  }
  
  // Handle WAV events that come via act with special properties
  if (e.is && e.is("ableton:wav:loaded")) {
    console.log("🔊 WAV loaded via act event");
    wavFile = e.file;
    message = `WAV loaded: ${wavFile.name}`;
  }
  
  // Listen for direct WAV events that might come through act instead of receive
  if (e.type && e.type.includes("wav")) {
    console.log("🔊 WAV-related act event:", e.type, e);
  }
}

// ⌨ Receive (Handle special system events)
function receive({ type, content }) {
  console.log("🎯 ABLETON.MJS: receive() called with type:", type, "content:", content);
  console.log("🎯 ABLETON.MJS: receive() full event details:", { type, content });
  
  // Handle dropped ALS files
  if (type === "dropped:als") {
    console.log("🎵 ABLETON.MJS: Received ALS file:", content?.name);
    console.log("🔍 ABLETON.MJS: Content structure:", content);
    
    // Check different possible data structures
    if (content.xmlData) {
      console.log("🎵 Found xmlData in content, creating project...");
      alsProject = new ALSProject(content.xmlData);
      alsProject.projectName = content.name;
      message = `ALS loaded: ${alsProject.tracks.length} tracks @ ${alsProject.tempo}bpm`;
      console.log("✅ ALS Project loaded via receive:", alsProject);
      
      // 🎵 Auto-start playback when ALS loads
      if (!isPlaying && alsProject) {
        isPlaying = true;
        playStartTime = performance.now();
        console.log("🎵 ABLETON.MJS: Auto-starting playback");
      }
    } else if (content.data) {
      console.log("🎵 Found data in content, creating project...");
      alsProject = new ALSProject(content.data);
      alsProject.projectName = content.name;
      message = `ALS loaded: ${alsProject.tracks.length} tracks @ ${alsProject.tempo}bpm`;
      console.log("✅ ALS Project loaded via receive:", alsProject);
      
      // 🎵 Auto-start playback when ALS loads
      if (!isPlaying && alsProject) {
        isPlaying = true;
        playStartTime = performance.now();
        console.log("🎵 ABLETON.MJS: Auto-starting playback");
      }
    } else if (content.project && content.project.tracks) {
      console.log("🎵 Found parsed project data, using BIOS-parsed tracks...");
      // Create a basic project from BIOS-parsed data
      alsProject = new ALSProject(null); // Create empty project
      alsProject.projectName = content.name;
      alsProject.tracks = content.project.tracks;
      alsProject.tempo = content.project.tempo || 120;
      message = `ALS loaded: ${alsProject.tracks.length} tracks @ ${alsProject.tempo}bpm (BIOS-parsed)`;
      console.log("✅ ALS Project loaded from BIOS data:", alsProject);
      
      // 🎵 Auto-start playback when ALS loads
      if (!isPlaying && alsProject) {
        isPlaying = true;
        playStartTime = performance.now();
        console.log("🎵 ABLETON.MJS: Auto-starting playback");
      }
    } else if (typeof content === 'string') {
      console.log("🎵 Content is string, using as XML data...");
      alsProject = new ALSProject(content);
      message = `ALS loaded: ${alsProject.tracks.length} tracks @ ${alsProject.tempo}bpm`;
      console.log("✅ ALS Project loaded via receive:", alsProject);
    } else {
      console.error("❌ ALS content structure not recognized:", content ? Object.keys(content) : "no content");
      console.log("🔍 Full content:", content);
      message = "ALS file structure not recognized";
    }
  }
  
  // Handle dropped WAV files
  if (type === "dropped:wav") {
    console.log("🔊 ABLETON.MJS: Received WAV file via receive:", content?.name, "ID:", content?.id);
    console.log("🔊 ABLETON.MJS: Full WAV content:", content);
    
    if (content && content.id) {
      wavFile = {
        name: content.name,
        size: content.size || 0,
        id: content.id // Use the file ID as audio reference
      };
      
      console.log("🔊 ABLETON.MJS: WAV file stored:", wavFile);
      
      // Stop any current playback before setting up new file
      if (playingSfx) {
        try {
          if (playingSfx.kill) {
            playingSfx.kill();
          }
          playingSfx = null;
        } catch (error) {
          console.error("Error stopping audio in receive:", error);
          playingSfx = null;
        }
      }
      
      // For dropped WAV files, the audio is already decoded by BIOS
      // so we don't need to preload - we can use the ID directly
      console.log("🔊 ABLETON.MJS: WAV file ready for direct playback (already decoded by BIOS)");
      message = `WAV ready: ${wavFile.name} - touch or [Space] to play`;
      
      // If ALS is already loaded and was auto-playing, start audio too
      if (isPlaying && alsProject) {
        console.log("🎵 ALS already playing, auto-starting WAV audio...");
        message = `Playing: ${alsProject.projectName} with ${wavFile.name}`;
      }
    } else {
      console.error("🔊 ABLETON.MJS: WAV content missing ID or invalid:", content);
      message = "WAV file could not be loaded";
    }
  }
}

// Simple auto-stop when audio ends
function sim() {
  if (isPlaying && playingSfx && playingSfx.killed) {
    isPlaying = false;
    message = `Finished: ${wavFile.name}`;
  }
}

export { paint, act, sim, receive };
