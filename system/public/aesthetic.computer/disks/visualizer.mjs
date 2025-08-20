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
          
          console.log(`🎯 LOCATOR PASSED: "${locator.name}" 
            Expected: ${locator.seconds.toFixed(2)}s (no scaling)
            Actual: ${currentTimeSeconds.toFixed(2)}s
            Audio Time: ${unscaledAudioTime.toFixed(2)}s
            Audio Beats: ${beatsBasedOnAudio.toFixed(2)} vs Project Beats: ${locator.time.toFixed(2)}
            Beat Offset: ${beatOffset.toFixed(2)} beats (${(beatOffset * 60 / this.tempo).toFixed(2)}s)
            Drift: ${timeDiff >= 0 ? '+' : ''}${timeDiff.toFixed(2)}s 
            Beat: ${locator.time.toFixed(2)} @ ${this.tempo}BPM`);
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

function paint({ wipe, ink, screen, sound, clock, write, box, line }) {
  // Global TV color tracking
  let globalCurrentColor = null;
  
  // Clear screen
  wipe(0, 0, 0);
  
  // If files aren't loaded yet, show loading message
  if (!alsProject || !preloadedAudio) {
    ink(255, 255, 255);
    write(message, 20, screen.height / 2);
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
    
    // Detailed timing log every 5 seconds
    if (Math.floor(currentTime) % 5 === 0 && Math.floor(currentTime * 10) % 10 === 0) {
      const formatNum = (num, decimals = 2) => (num != null && !isNaN(num)) ? num.toFixed(decimals) : 'N/A';
      
      console.log(`🕐 TIMING ANALYSIS @ ${currentTime.toFixed(2)}s:
        Performance elapsed: ${performanceElapsed.toFixed(2)}s
        Audio context time: ${formatNum(audioContextTime)}s  
        Audio progress: ${formatNum(audioProgress, 3)} (${formatNum(audioProgress ? audioProgress * actualDuration : null)}s)
        Using: ${currentTime.toFixed(2)}s (${(progress * 100).toFixed(1)}%) via ${timingMethod}
        Audio duration: ${actualDuration.toFixed(1)}s`);
      
      // Check for specific drift patterns between methods
      if (audioProgress != null && !isNaN(audioProgress)) {
        const perfAudioDrift = Math.abs(performanceElapsed - (audioProgress * actualDuration));
        console.log(`        Drift: Performance vs Audio = ${perfAudioDrift.toFixed(2)}s`);
        // Only warn for very large drift (increased threshold since we expect some difference)
        if (perfAudioDrift > 1.0) {
          console.warn(`⚠️ VERY LARGE DRIFT DETECTED: ${perfAudioDrift.toFixed(2)}s between timing methods`);
        }
      }
    }
  }
  
  // Calculate current time in seconds based on project timeline, not audio duration
  let currentTimeSeconds;
  if (isPlaying && progress && alsProject) {
    // Use the actual audio timeline as the master - visuals follow audio completely
    // No offset needed since we're using locator times directly
    currentTimeSeconds = progress * actualDuration;
    
    // Debug timing every 10 seconds
    if (Math.floor(currentTimeSeconds) % 10 === 0 && Math.floor(currentTimeSeconds * 10) % 10 === 0) {
      console.log(`Audio Timeline: ${currentTimeSeconds.toFixed(1)}s / ${actualDuration.toFixed(1)}s (${(progress * 100).toFixed(1)}%)`);
    }
  } else {
    currentTimeSeconds = 0;
  }
  
  // SCROLLING TIMELINE VISUALIZATION - ZOOMED IN FOR FAST MOVEMENT
  const centerX = screen.width / 2;
  const centerY = screen.height / 2;
  const timelineY = centerY;
  const timelineHeight = 120;
  
  // Timeline background - full width
  ink(15, 15, 25);
  box(0, timelineY - timelineHeight/2, screen.width, timelineHeight);
  
  // Timeline scale: ZOOMED IN - show only 20 seconds of timeline (10 seconds before/after current time)
  const timelineWindowSeconds = 20; // Much smaller for fast movement
  const pixelsPerSecond = screen.width / timelineWindowSeconds;
  
  // Calculate time range to display
  const startTime = Math.max(0, currentTimeSeconds - timelineWindowSeconds/2);
  const endTime = startTime + timelineWindowSeconds;
  
  // Draw time grid lines every 2 seconds (for zoomed view)
  ink(30, 30, 40);
  for (let t = Math.floor(startTime/2) * 2; t <= endTime; t += 2) {
    const x = centerX + (t - currentTimeSeconds) * pixelsPerSecond;
    if (x >= 0 && x <= screen.width) {
      line(x, timelineY - timelineHeight/2, x, timelineY + timelineHeight/2);
      
      // Time labels
      ink(60, 60, 80);
      const timeLabel = `${Math.floor(t/60)}:${(t%60).toString().padStart(2, '0')}`;
      write(timeLabel, x - timeLabel.length * 3, timelineY + timelineHeight/2 + 15);
    }
  }
  
  // Draw LOCATOR SEGMENTS as stretched boxes showing time duration between locators
  // Use locator times directly since audio is at correct 143 BPM
  
  for (let i = 0; i < alsProject.locators.length; i++) {
    const locator = alsProject.locators[i];
    const nextLocator = alsProject.locators[i + 1];
    
    // Use locator times directly - no scaling needed
    const segmentStart = locator.seconds;
    const segmentEnd = nextLocator ? nextLocator.seconds : actualDuration;
    const segmentDuration = segmentEnd - segmentStart;
    
    // Calculate screen positions
    const segmentStartX = centerX + (segmentStart - currentTimeSeconds) * pixelsPerSecond;
    const segmentEndX = centerX + (segmentEnd - currentTimeSeconds) * pixelsPerSecond;
    const segmentWidth = segmentEndX - segmentStartX;
    
    // Only draw if any part is visible on screen
    if (segmentEndX >= 0 && segmentStartX <= screen.width) {
      // Determine segment color based on current position
      const isCurrentSegment = currentTimeSeconds >= segmentStart && currentTimeSeconds < segmentEnd;
      const distanceToSegment = isCurrentSegment ? 0 : Math.min(
        Math.abs(segmentStart - currentTimeSeconds),
        Math.abs(segmentEnd - currentTimeSeconds)
      );
      
      // Color coding for segments
      let r, g, b, alpha = 180;
      if (isCurrentSegment) {
        // Current segment - bright green with pulsing effect
        const pulse = Math.sin(performance.now() * 0.01) * 0.3 + 0.7;
        r = 0; g = Math.floor(255 * pulse); b = Math.floor(100 * pulse);
        alpha = 220;
      } else if (distanceToSegment < 5) {
        // Nearby segments - yellow/orange
        r = 255; g = 200; b = 0;
        alpha = 160;
      } else if (distanceToSegment < 10) {
        // Visible segments - blue
        r = 100; g = 150; b = 255;
        alpha = 120;
      } else {
        // Distant segments - purple/gray
        r = 80; g = 60; b = 120;
        alpha = 80;
      }
      
      // Draw the segment box (clipped to screen bounds)
      const clippedStartX = Math.max(0, segmentStartX);
      const clippedEndX = Math.min(screen.width, segmentEndX);
      const clippedWidth = clippedEndX - clippedStartX;
      
      if (clippedWidth > 0) {
        ink(r, g, b);
        box(clippedStartX, timelineY - timelineHeight/2 + 10, clippedWidth, timelineHeight - 20);
        
        // Add border for current segment
        if (isCurrentSegment) {
          ink(255, 255, 255);
          // Top border
          box(clippedStartX, timelineY - timelineHeight/2 + 10, clippedWidth, 2);
          // Bottom border
          box(clippedStartX, timelineY + timelineHeight/2 - 12, clippedWidth, 2);
        }
        
        // REAL-TIME DATA VISUALIZATION ON TOP OF THE SEGMENT
        // (Draw AFTER the colored box so it appears on top)
        if (isCurrentSegment) {
          const activeNotes = alsProject.getActiveNotes(currentTimeSeconds);
          
          // Draw active notes as small indicators at the top of the segment
          if (activeNotes.length > 0) {
            const noteIndicatorHeight = 6;
            const noteIndicatorY = timelineY - timelineHeight/2 + 12;
            const noteIndicatorSpacing = 1;
            
            activeNotes.slice(0, 12).forEach((note, index) => { // Limit to 12 notes to fit
              const noteIndicatorX = clippedStartX + 10 + index * (noteIndicatorSpacing + 8);
              const noteIndicatorWidth = 6;
              
              // Don't draw if it would exceed the segment bounds
              if (noteIndicatorX + noteIndicatorWidth < clippedEndX) {
                // Color based on pitch
                const pitch = note.key || 60;
                const hue = (pitch * 5) % 360;
                const r = Math.floor(255 * (0.5 + 0.5 * Math.sin(hue * Math.PI / 180)));
                const g = Math.floor(255 * (0.5 + 0.5 * Math.sin((hue + 120) * Math.PI / 180)));
                const b = Math.floor(255 * (0.5 + 0.5 * Math.sin((hue + 240) * Math.PI / 180)));
                
                ink(r, g, b);
                box(noteIndicatorX, noteIndicatorY, noteIndicatorWidth, noteIndicatorHeight);
                
                // Add white border for emphasis
                ink(255, 255, 255);
                box(noteIndicatorX, noteIndicatorY, noteIndicatorWidth, 1); // Top border
                box(noteIndicatorX, noteIndicatorY + noteIndicatorHeight - 1, noteIndicatorWidth, 1); // Bottom border
              }
            });
          }
        }
        
        // SIMPLIFIED: SHOW ONLY MIDI NOTES WITHIN THIS SEGMENT
        // Get all MIDI notes that fall within this segment timespan
        const segmentNotes = [];
        
        // Debug: Check what clips we have
        if (isCurrentSegment) {
          console.log(`🎵 Checking clips for MIDI notes in segment ${locator.name}:`);
          console.log(`  Total clips: ${alsProject.clips.length}`);
          alsProject.clips.forEach((clip, idx) => {
            if (clip.type === 'midiclip') {
              console.log(`  Clip ${idx}: "${clip.name}" type=${clip.type}, notes=${clip.notes ? clip.notes.length : 'none'}, startSeconds=${clip.startSeconds}`);
            }
          });
        }
        
        alsProject.clips.forEach(clip => {
          if (clip.type === 'midiclip' && clip.notes && clip.notes.length > 0) {
            clip.notes.forEach(note => {
              // Calculate note timing in seconds
              const noteStartTime = clip.startSeconds + alsProject.beatsToSeconds(alsProject.alsTimeToBeat(note.time || 0));
              const noteEndTime = noteStartTime + alsProject.beatsToSeconds(alsProject.alsTimeToBeat(note.duration || 0.25));
              
              // Check if note falls within this segment
              if (noteStartTime >= segmentStart && noteStartTime < segmentEnd) {
                segmentNotes.push({
                  ...note,
                  startTime: noteStartTime,
                  endTime: noteEndTime,
                  clipName: clip.name,
                  trackIndex: clip.trackIndex,
                  trackName: alsProject.tracks[clip.trackIndex]?.name || `Track ${clip.trackIndex + 1}`
                });
              }
            });
          }
        });
        
        // Debug info for current segment
        if (isCurrentSegment) {
          console.log(`🎵 Current segment: ${locator.name} (${segmentStart.toFixed(2)}s - ${segmentEnd.toFixed(2)}s)`);
          console.log(`🎵 Found ${segmentNotes.length} MIDI notes in this segment`);
          if (segmentNotes.length > 0) {
            segmentNotes.slice(0, 5).forEach((note, idx) => {
              console.log(`  ${idx}: Pitch ${note.key || 60}, Time ${note.startTime.toFixed(2)}s, Track: ${note.trackName}`);
            });
          } else {
            console.log(`  🚫 No MIDI notes found in this segment`);
          }
        }
        
        if (segmentNotes.length > 0) {
          // DEBUG: Draw a bright rectangle to show we're in the rendering section
          if (isCurrentSegment) {
            console.log(`🎵 DEBUG: Drawing debug rectangle at (${clippedStartX + 5}, ${timelineY - timelineHeight/2 + 5})`);
            console.log(`🎵 DEBUG: Timeline area - X: ${clippedStartX} to ${clippedEndX}, Y: ${timelineY - timelineHeight/2} to ${timelineY + timelineHeight/2}`);
            ink(255, 0, 255); // Bright magenta debug rectangle
            box(clippedStartX + 5, timelineY - timelineHeight/2 + 5, 20, 10);
          }
          
          // TV Screen area at top - shows active/last passed colors
          const tvHeight = 25;
          const tvY = timelineY - timelineHeight/2 - tvHeight - 10;
          
          // Colored squares area (restored from before)
          const notesAreaHeight = Math.min(30, timelineHeight - 20);
          const notesAreaStartY = timelineY - timelineHeight/2 + 10;
          const noteSize = 3;
          const verticalSpacing = 6;
          const maxRows = Math.floor(notesAreaHeight / verticalSpacing);
          
          console.log(`🎵 SQUARES: squares area ${notesAreaHeight}px, ${maxRows} rows`);
          
          // Store current playing color for global TV bar (only from current segment)
          if (isCurrentSegment) {
            const playingNotes = segmentNotes.filter(note => {
              const noteEnd = note.endTime || (note.startTime + 0.25);
              return currentTimeSeconds >= note.startTime && currentTimeSeconds <= noteEnd;
            });
            
            if (playingNotes.length > 0) {
              const note = playingNotes[0];
              const velocity = note.velocity || 100;
              const pitch = note.key || 60;
              const noteProgress = (note.startTime - segmentStart) / segmentDuration;
              const row = segmentNotes.indexOf(note) % maxRows;
              
              const hue = (pitch * 7 + row * 60 + (noteProgress * 180)) % 360;
              const saturation = 0.9;
              const lightness = 0.4 + (velocity / 127) * 0.4;
              
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
              
              r = Math.min(255, r + 80);
              g = Math.min(255, g + 80);
              b = Math.min(255, b + 80);
              
              globalCurrentColor = {r, g, b, pitch, velocity};
              console.log(`🎵 STORING GLOBAL COLOR: pitch ${pitch}, RGB(${r}, ${g}, ${b})`);
            }
          }
          
          // Draw colored squares (restored original approach)
          segmentNotes.forEach((note, noteIndex) => {
            const noteProgress = (note.startTime - segmentStart) / segmentDuration;
            const noteX = clippedStartX + noteProgress * clippedWidth;
            
            const row = noteIndex % maxRows;
            const noteY = notesAreaStartY + row * verticalSpacing;
            
            if (noteX >= clippedStartX && noteX < clippedEndX) {
              const velocity = note.velocity || 100;
              const pitch = note.key || 60;
              
              // More varied color logic - mix pitch, time position, and row for diversity
              const hue = (pitch * 7 + row * 60 + (noteProgress * 180)) % 360; // More variation
              const saturation = 0.9;
              const lightness = 0.4 + (velocity / 127) * 0.4;
              
              // Better HSL to RGB conversion
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
              
              // Check if currently playing
              const noteEnd = note.endTime || (note.startTime + 0.25);
              const isPlaying = currentTimeSeconds >= note.startTime && currentTimeSeconds <= noteEnd;
              if (isPlaying) {
                r = Math.min(255, r + 80);
                g = Math.min(255, g + 80);
                b = Math.min(255, b + 80);
              }
              
              ink(r, g, b);
              box(noteX, noteY, noteSize, noteSize);
            }
          });
        }
        
        // Draw locator name if segment is wide enough and close enough
        if (segmentWidth > 60 && distanceToSegment < 15) {
          ink(255, 255, 255);
          const nameX = Math.max(10, Math.min(screen.width - locator.name.length * 6 - 10, segmentStartX + 10));
          const nameY = timelineY - 10;
          write(locator.name, nameX, nameY);
          
          // Show duration
          ink(200, 200, 200);
          const durationText = `${segmentDuration.toFixed(1)}s`;
          const durationX = Math.max(10, Math.min(screen.width - durationText.length * 6 - 10, segmentStartX + 10));
          write(durationText, durationX, nameY + 15);
        }
      }
      
      // Draw segment boundary line
      if (segmentStartX >= 0 && segmentStartX <= screen.width) {
        ink(isCurrentSegment ? 255 : 150, 255, 255);
        line(segmentStartX, timelineY - timelineHeight/2, segmentStartX, timelineY + timelineHeight/2);
      }
    }
  }
  
  // FIXED CENTRAL NEEDLE - the playhead that stays in center
  ink(255, 50, 50); // Bright red
  const needleWidth = 6;
  const needleHeight = timelineHeight + 60;
  box(centerX - needleWidth/2, timelineY - needleHeight/2, needleWidth, needleHeight);
  
  // Needle pointer at top and bottom
  ink(255, 100, 100);
  // Top arrow
  line(centerX - 15, timelineY - needleHeight/2 - 8, centerX, timelineY - needleHeight/2);
  line(centerX + 15, timelineY - needleHeight/2 - 8, centerX, timelineY - needleHeight/2);
  // Bottom arrow
  line(centerX - 15, timelineY + needleHeight/2 + 8, centerX, timelineY + needleHeight/2);
  line(centerX + 15, timelineY + needleHeight/2 + 8, centerX, timelineY + needleHeight/2);
  
  // Current time display at top
  ink(255, 255, 255);
  const currentTimeText = `${Math.floor(currentTimeSeconds/60)}:${(currentTimeSeconds%60).toFixed(1).padStart(4, '0')}`;
  write(currentTimeText, centerX - currentTimeText.length * 4, 30);
  
  // Total duration
  if (actualDuration) {
    ink(150, 150, 150);
    const totalTimeText = `/ ${Math.floor(actualDuration/60)}:${(actualDuration%60).toFixed(0).padStart(2, '0')}`;
    write(totalTimeText, centerX - totalTimeText.length * 3, 50);
  }
  
  // Current locator info in large text at bottom
  const { current: currentLocator, next: nextLocator } = alsProject.getCurrentLocator(currentTimeSeconds, actualDuration);
  
  if (currentLocator) {
    ink(0, 255, 100);
    const currentText = `NOW: ${currentLocator.name}`;
    write(currentText, centerX - currentText.length * 6, screen.height - 100);
    
    // Show progress within current segment (using direct times - no scaling)
    if (nextLocator) {
      const segmentProgress = (currentTimeSeconds - currentLocator.seconds) / (nextLocator.seconds - currentLocator.seconds);
      const progressText = `${(segmentProgress * 100).toFixed(1)}% through segment`;
      ink(100, 255, 150);
      write(progressText, centerX - progressText.length * 3, screen.height - 80);
    }
  }
  
  if (nextLocator) {
    ink(255, 255, 0);
    const nextText = `NEXT: ${nextLocator.name}`;
    const timeUntil = nextLocator.seconds - currentTimeSeconds;
    const countdownText = ` (in ${timeUntil.toFixed(1)}s)`;
    write(nextText + countdownText, centerX - (nextText + countdownText).length * 4, screen.height - 60);
  }
  
  // Progress indicator at very bottom
  const progressBarY = screen.height - 20;
  const progressBarWidth = screen.width - 40;
  
  // Progress bar background
  ink(30, 30, 30);
  box(20, progressBarY, progressBarWidth, 10);
  
  // Progress bar fill
  if (actualDuration && actualDuration > 0) {
    const fillWidth = progressBarWidth * progress;
    ink(0, 120, 255);
    box(20, progressBarY, fillWidth, 10);
  }
  
  // Show play/pause state
  if (!isPlaying && preloadedAudio) {
    ink(100, 100, 100);
    write("TAP TO PLAY", centerX - 40, screen.height - 140);
  } else if (isPlaying) {
    ink(100, 100, 100);
    write("TAP TO PAUSE", centerX - 45, screen.height - 140);
  }
  
  // BPM display in top right
  if (alsProject && alsProject.tempo) {
    ink(200, 200, 200);
    const bpmText = `${alsProject.tempo} BPM`;
    write(bpmText, screen.width - bpmText.length * 6 - 20, 30);
  }
  
  // Speed indicator
  ink(150, 150, 150);
  const speedText = `Timeline: ${timelineWindowSeconds}s window, ${pixelsPerSecond.toFixed(1)} px/s`;
  write(speedText, 20, screen.height - 40);
  
  // Compensation and locator count
  const locatorCountText = `${alsProject.locators.length} segments`;
  write(locatorCountText, screen.width - locatorCountText.length * 6 - 20, screen.height - 40);
  
  // === GLOBAL STATIC TV BAR === (completely separate from timeline segments)
  const tvHeight = 25;
  const tvY = 20; // Fixed position at top of screen
  
  if (globalCurrentColor) {
    ink(globalCurrentColor.r, globalCurrentColor.g, globalCurrentColor.b);
    box(20, tvY, screen.width - 40, tvHeight); // Full width, static position
    console.log(`🎵 GLOBAL TV: Static bar showing pitch ${globalCurrentColor.pitch}, RGB(${globalCurrentColor.r}, ${globalCurrentColor.g}, ${globalCurrentColor.b})`);
  } else {
    // Gray when no color is active
    ink(80, 80, 80);
    box(20, tvY, screen.width - 40, tvHeight);
    console.log(`🎵 GLOBAL TV: Static bar showing gray (no active notes)`);
  }
}

function act({ event: e, sound }) {
  // Simple play/pause control
  if (e.is("keyboard:p")) {
    timelineOffset += 0.5; // Look further ahead
    console.log(`👀 Timeline offset: +${timelineOffset.toFixed(1)}s (looking further ahead)`);
  }
  
  if (e.is("keyboard:minus")) {
    timelineOffset = Math.max(0, timelineOffset - 0.5); // Look less ahead (minimum 0)
    console.log(`👀 Timeline offset: +${timelineOffset.toFixed(1)}s (looking closer)`);
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
        // Debug log occasionally
        if (Math.floor(p.progress * 1000) % 500 === 0) {
          console.log(`🎵 Audio progress resolved: ${p.progress.toFixed(3)} (${(p.progress * (actualDuration || 184)).toFixed(1)}s)`);
        }
      }
    }).catch(err => {
      // Silent catch - don't spam console
    });
  }
  
  // Poll speaker for real-time analysis (if needed in future)
  sound.speaker?.poll();
}

export { paint, act, sim };
