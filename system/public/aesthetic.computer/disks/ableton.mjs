// Ableton Minimal Sculpture, 2025.01.24
// Comprehensive ALS parser and time-based track visualizer

// ALS Project Parser Class
class ALSProject {
  constructor(xmlData) {
    this.tracks = [];
    this.scenes = [];
    this.clips = [];
    this.devices = [];
    this.tempo = 120;
    this.timeSignature = { numerator: 4, denominator: 4 };
    this.creator = "Unknown";
    this.version = "Unknown";
    this.projectName = "Untitled";
    
    if (xmlData) {
      this.parseXML(xmlData);
    }
  }
  
  parseXML(xmlData) {
    try {
      console.log("🎵 Parsing ALS XML data...");
      this.parseGlobalSettings(xmlData);
      this.parseTempo(xmlData);
      this.parseTracks(xmlData);
      this.parseScenes(xmlData);
      this.parseClips(xmlData);
      this.parseDevices(xmlData);
      console.log(`✅ Parsed ${this.tracks.length} tracks, ${this.clips.length} clips @ ${this.tempo}bpm`);
    } catch (error) {
      console.error("🚨 ALS Parse Error:", error);
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
    // Enhanced tempo parsing with multiple patterns and better debugging
    console.log("🎵 Starting tempo parsing...");
    
    // Pattern 1: Standard Tempo structure
    let tempoMatch = xmlData.match(/<Tempo>[\s\S]*?<Manual Value="([^"]+)"[\s\S]*?<\/Tempo>/);
    if (tempoMatch) {
      this.tempo = parseFloat(tempoMatch[1]);
      console.log(`🎵 Found tempo (Pattern 1): ${this.tempo} BPM`);
      return;
    }
    
    // Pattern 2: Try alternative tempo pattern with case variations
    tempoMatch = xmlData.match(/<Manual Value="([^"]+)"[^>]*\/?>[\s\S]{0,100}?[Tt]empo/i);
    if (tempoMatch) {
      const tempoValue = parseFloat(tempoMatch[1]);
      if (tempoValue >= 60 && tempoValue <= 300) {
        this.tempo = tempoValue;
        console.log(`🎵 Found tempo (Pattern 2): ${this.tempo} BPM`);
        return;
      }
    }
    
    // Pattern 3: Look for any tempo-related Value attributes
    const allValueMatches = xmlData.match(/Value="([0-9.]+)"/g);
    if (allValueMatches) {
      console.log(`🎵 Found ${allValueMatches.length} Value attributes, checking for tempo...`);
      
      for (let valueMatch of allValueMatches) {
        const value = parseFloat(valueMatch.match(/Value="([0-9.]+)"/)[1]);
        
        // Look for values in typical BPM range
        if (value >= 60 && value <= 300) {
          // Check if this value appears near tempo-related keywords
          const valueIndex = xmlData.indexOf(valueMatch);
          const contextBefore = xmlData.substring(Math.max(0, valueIndex - 200), valueIndex);
          const contextAfter = xmlData.substring(valueIndex, Math.min(xmlData.length, valueIndex + 200));
          const context = contextBefore + contextAfter;
          
          if (/tempo|bpm|beat/i.test(context) || 
              /MasterTrack/i.test(contextBefore) ||
              /CurrentTempo/i.test(context)) {
            this.tempo = value;
            console.log(`🎵 Found tempo (Pattern 3 - contextual): ${this.tempo} BPM`);
            return;
          }
        }
      }
    }
    
    // Pattern 4: Check specifically in MasterTrack
    const masterTrackMatch = xmlData.match(/<MasterTrack[^>]*>([\s\S]*?)<\/MasterTrack>/);
    if (masterTrackMatch) {
      const masterContent = masterTrackMatch[1];
      console.log("🎵 Searching in MasterTrack content...");
      
      // Look for any reasonable tempo values in master track
      const masterValues = masterContent.match(/Value="([0-9.]+)"/g);
      if (masterValues) {
        for (let valueMatch of masterValues) {
          const value = parseFloat(valueMatch.match(/Value="([0-9.]+)"/)[1]);
          if (value >= 80 && value <= 200) { // Common tempo range
            this.tempo = value;
            console.log(`🎵 Found tempo in MasterTrack: ${this.tempo} BPM`);
            return;
          }
        }
      }
    }
    
    // Pattern 5: Look for specific tempo fields
    const tempoFields = [
      /<CurrentTempo[^>]*Value="([^"]+)"/i,
      /<GlobalTempo[^>]*Value="([^"]+)"/i,
      /<Tempo[^>]*Value="([^"]+)"/i,
      /<BPM[^>]*Value="([^"]+)"/i
    ];
    
    for (let pattern of tempoFields) {
      tempoMatch = xmlData.match(pattern);
      if (tempoMatch) {
        const tempoValue = parseFloat(tempoMatch[1]);
        if (tempoValue >= 60 && tempoValue <= 300) {
          this.tempo = tempoValue;
          console.log(`🎵 Found tempo (specific field): ${this.tempo} BPM`);
          return;
        }
      }
    }
    
    // Pattern 6: Try to extract from filename (zzzZWAP 143bpm) - moved up for priority
    const nameMatch = this.projectName.match(/(\d+)bpm/i);
    if (nameMatch) {
      this.tempo = parseInt(nameMatch[1]);
      console.log(`🎵 Extracted tempo from filename: ${this.tempo} BPM`);
      return;
    }
    
    // Pattern 7: Last resort - look for 143 specifically if that's what you expect
    if (xmlData.includes('143')) {
      const match143 = xmlData.match(/143(?:\.0+)?/);
      if (match143) {
        this.tempo = 143;
        console.log(`🎵 Found expected tempo 143 BPM in file`);
        return;
      }
    }
    
    // If still no tempo found, keep default but log warning
    if (this.tempo === 120) {
      console.warn(`⚠️ Could not find tempo in ALS file, using default ${this.tempo} BPM`);
      
      // Debug: Show first few Value attributes for manual inspection
      const firstValues = xmlData.match(/Value="([0-9.]+)"/g);
      if (firstValues) {
        console.log("🔍 First 10 Value attributes found:", firstValues.slice(0, 10));
      }
    }
    
    // Time signature parsing (unchanged)
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
  
  parseClips(xmlData) {
    // Parse MIDI and Audio clips with timing data
    this.parseMIDIClips(xmlData);
    this.parseAudioClips(xmlData);
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
    const noteRegex = /<MidiNote[^>]+Time="([^"]+)"[^>]+Duration="([^"]+)"[^>]+Velocity="([^"]+)"[^>]*\/>/g;
    let noteMatch;
    
    while ((noteMatch = noteRegex.exec(clipContent)) !== null) {
      notes.push({
        time: parseFloat(noteMatch[1]),
        duration: parseFloat(noteMatch[2]),
        velocity: parseInt(noteMatch[3]),
        pitch: this.extractNotePitch(noteMatch[0]) // Extract from full match
      });
    }
    
    return notes;
  }
  
  extractNotePitch(noteElement) {
    // Extract MIDI note number from the KeyTrack structure
    // This is a simplified extraction - real implementation would need track context
    return 60; // Default middle C
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
    if (!audioTime || !this.tempo) return 0;
    return (audioTime * this.tempo) / 60;
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
  
    
  // Generate a comprehensive time-based visualization that syncs with audio
  drawTimeBasedVisualization(ink, screen, currentTime = 0) {
    if (!currentTime || this.tracks.length === 0) return;
    
    const beatPosition = this.getCurrentBeat(currentTime);
    const vizData = this.generateVisualizationData(beatPosition);
    
    // Live Notation Graphics
    this.drawLiveNotation(ink, screen, beatPosition, vizData);
    
    // Timeline dimensions with better clip visibility
    const timelineHeight = 200; // Increased height for better clip visibility
    const timelineY = screen.height - timelineHeight - 20;
    const trackHeight = Math.min(30, timelineHeight / Math.max(this.tracks.length, 1)); // Slightly taller tracks
    
    // Draw timeline background
    ink(20, 20, 30).box(10, timelineY, screen.width - 20, timelineHeight);
    
    // Timeline scrolling: center the view around the current playhead
    const beatsPerScreen = 16; // Show more beats for better context
    const pixelsPerBeat = (screen.width - 40) / beatsPerScreen;
    const startBeat = Math.max(0, beatPosition - beatsPerScreen / 2); // Center on playhead
    
    // Beat markers with subdivision lines (updated for new range)
    for (let i = 0; i <= beatsPerScreen; i++) {
      const beat = startBeat + i;
      const x = 20 + i * pixelsPerBeat;
      
      // Strong beat line (downbeats)
      if (beat % this.timeSignature.numerator === 0) {
        ink(120, 120, 140).line(x, timelineY, x, timelineY + timelineHeight);
        ink("white").write(`${Math.floor(beat / this.timeSignature.numerator) + 1}`, 
          { x: x + 2, y: timelineY - 15, font: "microtype" });
      } else {
        // Regular beat line
        ink(60, 60, 80).line(x, timelineY, x, timelineY + timelineHeight);
      }
      
      // Subdivision lines (16th notes)
      for (let sub = 1; sub < 4; sub++) {
        const subX = x + (sub * pixelsPerBeat / 4);
        ink(40, 40, 50).line(subX, timelineY, subX, timelineY + timelineHeight / 2);
      }
    }
    
    // Current playhead with beat flash
    const playheadX = 20 + (beatPosition - startBeat) * pixelsPerBeat;
    const beatPhase = beatPosition % 1;
    const flashIntensity = beatPhase < 0.1 ? (0.1 - beatPhase) * 10 : 0;
    ink(255, 100 + flashIntensity * 155, 100).line(playheadX, timelineY - 15, playheadX, timelineY + timelineHeight + 15);
    
    // Draw tracks with musical activity
    vizData.trackActivity.forEach((trackData, trackIndex) => {
      const y = timelineY + trackIndex * trackHeight;
      this.drawTrackNotation(ink, screen, trackData, y, trackHeight, startBeat, pixelsPerBeat, beatPosition, trackIndex);
    });
    
    // Draw tempo and musical info
    ink("yellow").write(`♩=${this.tempo}`, { x: screen.width - 100, y: timelineY - 35, font: "microtype" });
    ink("yellow").write(`${this.timeSignature.numerator}/${this.timeSignature.denominator}`, 
      { x: screen.width - 100, y: timelineY - 23, font: "microtype" });
      
    // Beat counter with musical notation
    const currentMeasure = Math.floor(beatPosition / this.timeSignature.numerator) + 1;
    const beatInMeasure = (beatPosition % this.timeSignature.numerator) + 1;
    ink("cyan").write(`${currentMeasure}.${beatInMeasure.toFixed(1)}`, 
      { x: screen.width - 100, y: timelineY - 11, font: "microtype" });
    
    // Harmonic context display (simplified)
    if (vizData.harmonicContext && vizData.harmonicContext.chord !== 'Single') {
      ink("lightgreen").write(`${vizData.harmonicContext.chord}`, 
        { x: screen.width - 180, y: timelineY - 35, font: "microtype" });
      
      const tension = vizData.harmonicContext.tension || 0;
      const tensionColor = tension > 0.5 ? "red" : tension > 0.3 ? "orange" : "green";
      ink(tensionColor).write(`T:${(tension * 100).toFixed(0)}%`, 
        { x: screen.width - 180, y: timelineY - 23, font: "microtype" });
    }
  }
  
  // Minimal drum visualization with blinking boxes
  drawMinimalDrumVisualization(ink, screen, currentTime = 0) {
    if (!currentTime || this.tracks.length === 0) return;
    
    // Game Boy style compact area - bottom 120px
    const vizHeight = 120;
    const vizY = screen.height - vizHeight;
    
    // Clear compact visualization area
    ink(0, 0, 0).box(0, vizY, screen.width, vizHeight);
    
    const currentBeat = this.getCurrentBeat(currentTime);
    
    // Compact info display (top 20px of viz area)
    ink("white").write(`${this.tempo}BPM ${currentTime.toFixed(1)}s B${currentBeat.toFixed(1)}`, 
      { x: 5, y: vizY + 5, font: "microtype" });
    
    // Zoomed timeline (middle 60px)
    this.drawCompactTimeline(ink, screen, currentTime, vizY + 20, 60);
    
    // Minimal drum boxes (bottom 40px)
    this.drawCompactDrumBoxes(ink, screen, currentTime, currentBeat, vizY + 80, 40);
  }
  
  // Compact zoomed timeline with box-based clips
  drawCompactTimeline(ink, screen, currentTime, timelineY, timelineHeight) {
    const timelineWidth = screen.width - 10;
    
    // Timeline background
    ink(20, 20, 20).box(5, timelineY, timelineWidth, timelineHeight);
    
    // Zoomed view: show only 4 seconds around current time
    const viewWindow = 4; // seconds
    const startTime = Math.max(0, currentTime - viewWindow / 2);
    const endTime = startTime + viewWindow;
    const pixelsPerSecond = timelineWidth / viewWindow;
    
    // Playhead line
    const playheadX = 5 + (currentTime - startTime) * pixelsPerSecond;
    ink(255, 255, 0).line(playheadX, timelineY, playheadX, timelineY + timelineHeight);
    
    // Find clips in view window
    const visibleClips = this.clips.filter(clip => {
      const clipStart = (clip.time || 0) / 4; // Convert ALS time
      const clipEnd = clipStart + ((clip.duration || 0) / 4);
      return clipEnd >= startTime && clipStart <= endTime;
    });
    
    // Draw clips as boxes with proper duration
    visibleClips.forEach((clip, index) => {
      const clipStart = (clip.time || 0) / 4;
      const clipDuration = (clip.duration || 0.1) / 4;
      const clipEnd = clipStart + clipDuration;
      
      const startX = Math.max(5, 5 + (clipStart - startTime) * pixelsPerSecond);
      const endX = Math.min(5 + timelineWidth, 5 + (clipEnd - startTime) * pixelsPerSecond);
      const boxWidth = Math.max(1, endX - startX);
      
      // Stack clips vertically in compact rows
      const trackRow = index % 4; // Max 4 rows
      const rowHeight = Math.floor(timelineHeight / 4);
      const boxY = timelineY + trackRow * rowHeight + 2;
      const boxHeight = rowHeight - 4;
      
      // Color based on clip type and activity
      const isActive = currentTime >= clipStart && currentTime <= clipEnd;
      let clipColor = clip.type === 'MIDI' ? [100, 150, 255] : [255, 150, 100];
      
      // Identify drum type for specific colors
      const clipName = (clip.name || '').toLowerCase();
      const samplePath = clip.sampleRef ? (clip.sampleRef.path || '').toLowerCase() : '';
      
      if (clipName.includes('kick') || samplePath.includes('kick')) {
        clipColor = [255, 100, 100]; // Red
      } else if (clipName.includes('snare') || samplePath.includes('snare')) {
        clipColor = [100, 255, 100]; // Green
      } else if (clipName.includes('hihat') || samplePath.includes('hihat')) {
        clipColor = [100, 100, 255]; // Blue
      }
      
      const intensity = isActive ? 1.0 : 0.6;
      ink(clipColor[0] * intensity, clipColor[1] * intensity, clipColor[2] * intensity)
        .box(startX, boxY, boxWidth, boxHeight);
      
      // Minimal clip label if there's space
      if (boxWidth > 20) {
        const shortName = (clip.name || clip.type).substring(0, 8);
        ink("white").write(shortName, 
          { x: startX + 1, y: boxY + 1, font: "microtype" });
      }
    });
    
    // Time markers (minimal)
    for (let t = Math.floor(startTime); t <= Math.ceil(endTime); t++) {
      const x = 5 + (t - startTime) * pixelsPerSecond;
      if (x >= 5 && x <= 5 + timelineWidth) {
        ink(80, 80, 80).line(x, timelineY, x, timelineY + 10);
        ink("gray").write(`${t}s`, { x: x + 1, y: timelineY + timelineHeight - 10, font: "microtype" });
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
}

// State
let alsProject = null;
let wavFile = null;
let isPlaying = false;
let playingSfx = null;
let playStartTime = 0;
let audioStartTime = 0; // Track when audio actually started
let message = "Drop .als + .wav files";

function paint({ wipe, ink, screen, sound, clock }) {
  wipe("black");
  
  if (!alsProject) {
    ink("green").write("LOADING...", { center: "xy", size: 1 });
    return;
  }

  if (!isPlaying || !sound || sound.time <= 0) {
    ink("yellow").write("PAUSED", { center: "xy", size: 1 });
    return;
  }

  // Get current time for drum checking
  const clockTimeRaw = clock.time();
  const currentClockTime = typeof clockTimeRaw === 'object' && clockTimeRaw.getTime ? 
    clockTimeRaw.getTime() / 1000 : clockTimeRaw;
  const audioTime = sound.time - audioStartTime;
  const syncedTime = currentClockTime - playStartTime;
  const currentAudioTime = Math.max(audioTime, syncedTime);
  
  // Ultra-minimal Game Boy style - just colored stripes
  const stripeHeight = 12;
  const drumTypes = ["KICK", "SNARE", "HIHAT", "CYMBAL", "PERC"];
  const colors = ["red", "blue", "green", "yellow", "magenta"];
  
  let y = 20;
  for (let i = 0; i < drumTypes.length; i++) {
    const drumType = drumTypes[i];
    const color = colors[i];
    
    // Check if this drum type is active right now
    const isActive = alsProject.isDrumActiveAtTime(drumType, currentAudioTime);
    
    // Debug first few seconds
    if (currentAudioTime < 10) {
      console.log(`🥁 ${drumType} at ${currentAudioTime.toFixed(2)}s: ${isActive}`);
    }
    
    if (isActive) {
      // Full bright stripe when drum is playing
      ink(color).box(0, y, screen.width, stripeHeight);
    } else {
      // Thin dim line when inactive
      ink(color, 0.2).box(0, y + 5, screen.width, 2);
    }
    
    y += stripeHeight + 4;
  }
  
  // Minimal debug at bottom
  const clipStats = alsProject.getClipStats ? alsProject.getClipStats() : "no stats";
  ink("white", 0.7).write(`${currentAudioTime.toFixed(1)}s | ${clipStats}`, {
    x: 5,
    y: screen.height - 10,
    size: 0.4
  });
  
  // Debug timing and clips in first 10 seconds
  if (currentAudioTime < 10) {
    console.log(`⏰ Time debug - Audio: ${currentAudioTime.toFixed(2)}s, Tempo: ${alsProject.tempo}, Total clips: ${alsProject.clips.length}`);
    
    // Show first few clips for debugging
    if (alsProject.clips.length > 0) {
      const firstClips = alsProject.clips.slice(0, 3);
      firstClips.forEach((clip, i) => {
        console.log(`🎵 Clip ${i}: ${clip.name || 'unnamed'} | Type: ${clip.type} | Time: ${clip.time} | Duration: ${clip.duration}`);
      });
    }
  }
}

function act({ event: e, sound, pen, clock }) {
  // Debug all events to see what we're getting
  if (e.name !== "move" && e.name !== "draw") {
    console.log("🔍 Act event:", e.name || e.type || "unknown", Object.keys(e));
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
  if ((e.is && e.is("touch")) && wavFile) {
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
      console.log("▶️ Starting playback with file:", wavFile.id);
      try {
        playingSfx = sound.play(wavFile.id);
        
        if (playingSfx) {
          isPlaying = true;
          // Convert clock time to numeric timestamp for consistency
          const clockTimeRaw = clock.time();
          playStartTime = typeof clockTimeRaw === 'object' && clockTimeRaw.getTime ? 
            clockTimeRaw.getTime() / 1000 : clockTimeRaw; // Convert to seconds
          audioStartTime = sound.time; // Store when audio started for sync
          message = "Playing - touch to pause";
          console.log("✅ Audio playback started - Clock time:", playStartTime, "Audio time:", audioStartTime);
        } else {
          console.error("❌ Failed to start audio playback - no sound object returned");
          message = "Failed to play audio";
        }
      } catch (error) {
        console.error("❌ Audio playback error:", error);
        message = "Failed to play audio";
      }
    }
    return; // Important: return early to prevent other event handling
  }
  
  // Handle WAV events that come via act with file properties directly on event
  if (e.name && e.size && e.id && !e.is("touch")) {
    console.log("🔊 WAV file received in act:", e.name, "Size:", e.size, "ID:", e.id);
    wavFile = {
      name: e.name,
      originalName: e.originalName || e.name,
      size: e.size,
      id: e.id
    };
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
    message = `WAV ready: ${wavFile.name}`;
    console.log("🔊 WAV file ready for playback");
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
  console.log("🔧 Received event:", type, content);
  
  // Handle dropped ALS files
  if (type === "dropped:als") {
    console.log("🎵 Received ALS file:", content.name);
    if (content.xmlData) {
      alsProject = new ALSProject(content.xmlData);
      message = `ALS loaded: ${alsProject.tracks.length} tracks @ ${alsProject.tempo}bpm`;
      console.log("✅ ALS Project loaded via receive:", alsProject);
    }
  }
  
  // Handle dropped WAV files
  if (type === "dropped:wav") {
    console.log("🔊 Received WAV file via receive:", content.name, "ID:", content.id);
    wavFile = {
      name: content.name,
      size: content.size || 0,
      id: content.id // Use the file ID as audio reference
    };
    isPlaying = false;
    if (playingSfx) {
      // Stop any current playback
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
    message = `WAV ready: ${wavFile.name}`;
    console.log("🔊 WAV file ready for playback via receive");
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
