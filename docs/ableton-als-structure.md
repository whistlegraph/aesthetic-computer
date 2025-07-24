# Ableton Live Set (.als) XML Structure Documentation

*Generated 2025.01.24*

This document provides a comprehensive analysis of Ableton Live Set (.als) file format and structure to enable semantic parsing and animated notation of ALS projects within the Aesthetic Computer environment.

## Table of Contents

1. [File Format Overview](#file-format-overview)
2. [XML Structure Analysis](#xml-structure-analysis)
3. [Core Elements](#core-elements)
4. [Track Types and Properties](#track-types-and-properties)
5. [MIDI and Timing Data](#midi-and-timing-data)
6. [Device and Effects Structure](#device-and-effects-structure)
7. [Arrangement and Session Data](#arrangement-and-session-data)
8. [Implementation Notes](#implementation-notes)
9. [Aesthetic Computer Integration](#aesthetic-computer-integration)

## File Format Overview

### Physical Structure
- **Extension**: `.als`
- **Format**: Compressed XML data (GZIP)
- **Version**: Created by Ableton Live 8.0+
- **Encoding**: UTF-8 XML

### Basic Extraction Process
```javascript
// As implemented in bios.mjs
1. File is dropped as .als
2. Decompressed using pako.ungzip()
3. XML content extracted as string
4. Sent to ableton.mjs via "dropped:als" event
```

## XML Structure Analysis

### Root Element
```xml
<Ableton MajorVersion="5" MinorVersion="12.1_12115" SchemaChangeCount="3" Creator="Ableton Live 12.1.5 Suite" Revision="">
  <LiveSet>
    <!-- All project data contained here -->
  </LiveSet>
</Ableton>
```

### Primary Sections

#### 1. Global Project Settings
```xml
<LiveSet>
  <LockedScripts/>
  <MidiControllers/>
  <OverwriteProtectionNumber Value="1024"/>
  <SceneNameManager>
    <Name Value="1"/>
  </SceneNameManager>
  <TimeSelection>
    <AnchorTime Value="0"/>
    <EndTime Value="0"/>
  </TimeSelection>
  <MasterTrack Id="0">
    <!-- Master track configuration -->
  </MasterTrack>
</LiveSet>
```

#### 2. Tempo and Time Signature
```xml
<MasterTrack Id="0">
  <DeviceChain>
    <DeviceChain>
      <AutomationEnvelopes/>
      <Mixer>
        <Tempo>
          <Manual Value="143"/>  <!-- BPM VALUE HERE -->
        </Tempo>
        <TimeSignature>
          <TimeSignatureNumerator Value="4"/>
          <TimeSignatureDenominator Value="4"/>
        </TimeSignature>
      </Mixer>
    </DeviceChain>
  </DeviceChain>
</MasterTrack>
```

## Core Elements

### 1. Tracks Structure

#### MIDI Track Example
```xml
<MidiTrack Id="1">
  <LomId Value="0"/>
  <LomIdView Value="0"/>
  <IsGrouped Value="false"/>
  <TrackGroupId Value="-1"/>
  <Name>
    <EffectiveName Value="BASS"/>
    <UserName Value="BASS"/>
    <Annotation Value=""/>
  </Name>
  <Color Value="2315382"/>
  <TrackSizeState Value="0"/>
  <SizeState Value="1"/>
  <AutomationVisible Value="false"/>
  <DeviceChain>
    <!-- Device chain contains instruments and effects -->
  </DeviceChain>
</MidiTrack>
```

#### Audio Track Example
```xml
<AudioTrack Id="2">
  <LomId Value="0"/>
  <Name>
    <EffectiveName Value="KICK"/>
    <UserName Value="KICK"/>
  </Name>
  <Color Value="16777215"/>
  <!-- Audio-specific properties -->
  <DeviceChain>
    <!-- Audio effects chain -->
  </DeviceChain>
</AudioTrack>
```

### 2. Track Types Identification

Based on XML elements:
- **MidiTrack**: MIDI instrument tracks
- **AudioTrack**: Audio tracks with recorded or imported audio
- **ReturnTrack**: Send/return effects tracks  
- **GroupTrack**: Folder tracks grouping other tracks
- **MasterTrack**: Main output bus

### 3. Track Colors and Visual Properties

```xml
<Color Value="2315382"/>  <!-- RGB color as integer -->
```

Color conversion:
```javascript
// Convert integer to RGB
function intToRgb(colorInt) {
  return {
    r: (colorInt >> 16) & 255,
    g: (colorInt >> 8) & 255,
    b: colorInt & 255
  };
}
```

## Track Types and Properties

### MIDI Track Properties
- **Instruments**: Wavetable, Operator, Impulse, etc.
- **MIDI Effects**: Arpeggiator, Scale, Note Length, etc.
- **Clips**: MIDI note data, automation
- **Routing**: Input/output, sends, groups

### Audio Track Properties  
- **Audio Effects**: EQ Eight, Compressor, Reverb, etc.
- **Clips**: Audio file references, warping data
- **Recording**: Input settings, monitoring
- **Processing**: Freeze, flatten options

### Return Track Properties
- **Effects**: Typically reverb, delay, modulation
- **Routing**: Receives sends from other tracks
- **Control**: Send amounts, pre/post fader

## MIDI and Timing Data

### MIDI Clip Structure
```xml
<MidiClip Id="0">
  <LomId Value="0"/>
  <Time Value="0.0"/>
  <Duration Value="4.0"/>
  <Loop>
    <LoopStart Value="0.0"/>
    <LoopEnd Value="4.0"/>
    <StartRelative Value="0.0"/>
    <LoopOn Value="true"/>
    <OutMarker Value="4.0"/>
    <HiddenLoopStart Value="0.0"/>
    <HiddenLoopEnd Value="4.0"/>
  </Loop>
  <Name Value=""/>
  <ColorIndex Value="0"/>
  <HasLegato Value="false"/>
  <MidiKey>
    <Notes>
      <!-- Individual MIDI notes -->
      <KeyTrack Id="0">
        <Notes>
          <MidiNote Time="0.0" Duration="0.25" Velocity="100" OffVelocity="64" IsEnabled="true"/>
          <MidiNote Time="0.5" Duration="0.25" Velocity="100" OffVelocity="64" IsEnabled="true"/>
          <!-- More notes... -->
        </Notes>
      </KeyTrack>
    </Notes>
  </MidiKey>
</MidiClip>
```

### Key MIDI Properties
- **Time**: Note start position in beats
- **Duration**: Note length in beats  
- **Velocity**: Note velocity (0-127)
- **Pitch**: MIDI note number (implicit from KeyTrack position)

### Audio Clip Structure
```xml
<AudioClip Id="0">
  <LomId Value="0"/>
  <Time Value="0.0"/>
  <Duration Value="4.0"/>
  <Loop>
    <!-- Loop settings -->
  </Loop>
  <SampleRef>
    <FileRef>
      <Name Value="KICK_SAMPLE.wav"/>
      <Type Value="1"/>
      <Data>
        <RelativePathElement Dir="Samples"/>
        <RelativePathElement Dir="Processed"/>
        <RelativePathElement Dir="Crop"/>
        <RelativePathElement Dir="KICK_SAMPLE.wav"/>
      </Data>
    </FileRef>
  </SampleRef>
  <WarpMarkers>
    <!-- Warping/timing data -->
  </WarpMarkers>
</AudioClip>
```

## Device and Effects Structure

### Instrument Device Example
```xml
<InstrumentBranch>
  <Items>
    <Wavetable Id="0">
      <LomId Value="0"/>
      <LomIdView Value="0"/>
      <IsExpanded Value="true"/>
      <On>
        <LomId Value="0"/>
        <Manual Value="true"/>
      </On>
      <ModulationSourceCount Value="0"/>
      <ParametersListWrapper>
        <!-- Device parameters -->
      </ParametersListWrapper>
    </Wavetable>
  </Items>
</InstrumentBranch>
```

### Audio Effect Example
```xml
<AudioEffectBranch>
  <Items>
    <Eq8 Id="0">
      <LomId Value="0"/>
      <LomIdView Value="0"/>
      <IsExpanded Value="true"/>
      <On>
        <LomId Value="0"/>
        <Manual Value="true"/>
      </On>
      <ParametersListWrapper>
        <!-- EQ parameters -->
        <FilterBands>
          <HighQualityOn Value="false"/>
          <Mode Value="0"/>
          <Bands>
            <!-- 8 EQ bands -->
          </Bands>
        </FilterBands>
      </ParametersListWrapper>
    </Eq8>
  </Items>
</AudioEffectBranch>
```

## Arrangement and Session Data

### Session View Clips
```xml
<Scenes>
  <Scene Id="0">
    <LomId Value="0"/>
    <LomIdView Value="0"/>
    <Name Value=""/>
    <ColorIndex Value="0"/>
    <TempoEnabled Value="false"/>
    <Tempo Value="120"/>
    <TimeSignatureEnabled Value="false"/>
    <TimeSignature>
      <TimeSignatureNumerator Value="4"/>
      <TimeSignatureDenominator Value="4"/>
    </TimeSignature>
  </Scene>
</Scenes>
```

### Arrangement View Data
```xml
<ArrangerAutomation>
  <Events>
    <!-- Automation events with timing -->
  </Events>
</ArrangerAutomation>
```

## Implementation Notes

### Current ALSProject Class Enhancement

Based on the zzzZWAP project mentioned in the notebook:
- **BPM**: 143 (found in `<Manual Value="143"/>`)
- **Tracks**: BASS (MIDI), KICK (audio), HATS (MIDI), snare (MIDI)
- **Structure**: Each track has specific MIDI data and timing

### Enhanced Parsing Strategy

```javascript
class ALSProject {
  constructor(xmlData) {
    this.tracks = [];
    this.scenes = [];
    this.tempo = 120;
    this.timeSignature = { numerator: 4, denominator: 4 };
    this.creator = "Unknown";
    this.version = "Unknown";
    this.clips = [];
    this.devices = [];
    
    if (xmlData) {
      this.parseXML(xmlData);
    }
  }
  
  parseXML(xmlData) {
    // Enhanced parsing for comprehensive data extraction
    this.parseGlobalSettings(xmlData);
    this.parseTempo(xmlData);
    this.parseTracks(xmlData);
    this.parseScenes(xmlData);
    this.parseClips(xmlData);
    this.parseDevices(xmlData);
  }
  
  parseGlobalSettings(xmlData) {
    // Extract project-level metadata
    const creatorMatch = xmlData.match(/Creator="([^"]+)"/);
    if (creatorMatch) {
      this.creator = creatorMatch[1];
      const versionMatch = creatorMatch[1].match(/(\d+\.\d+)/);
      if (versionMatch) this.version = versionMatch[1];
    }
  }
  
  parseTempo(xmlData) {
    // Enhanced tempo parsing
    const tempoMatch = xmlData.match(/<Tempo>[\s\S]*?<Manual Value="([^"]+)"[\s\S]*?<\/Tempo>/);
    if (tempoMatch) {
      this.tempo = parseFloat(tempoMatch[1]);
    }
    
    // Time signature
    const sigNumMatch = xmlData.match(/<TimeSignatureNumerator Value="([^"]+)"/);
    const sigDenMatch = xmlData.match(/<TimeSignatureDenominator Value="([^"]+)"/);
    if (sigNumMatch && sigDenMatch) {
      this.timeSignature = {
        numerator: parseInt(sigNumMatch[1]),
        denominator: parseInt(sigDenMatch[1])
      };
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
          clips: this.extractTrackClips(trackContent),
          devices: this.extractTrackDevices(trackContent)
        };
        
        this.tracks.push(track);
      }
    });
  }
  
  parseScenes(xmlData) {
    // Extract scene information
    const sceneRegex = /<Scene[^>]*Id="([^"]*)"[^>]*>([\s\S]*?)<\/Scene>/g;
    let match;
    
    while ((match = sceneRegex.exec(xmlData)) !== null) {
      const sceneId = match[1];
      const sceneContent = match[2];
      
      const scene = {
        id: sceneId,
        name: this.extractSceneName(sceneContent),
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
        name: this.extractClipName(clipContent),
        notes: this.extractMIDINotes(clipContent),
        loop: this.extractLoopData(clipContent)
      };
      
      this.clips.push(clip);
    }
  }
  
  extractMIDINotes(clipContent) {
    const notes = [];
    const noteRegex = /<MidiNote[^>]+Time="([^"]+)"[^>]+Duration="([^"]+)"[^>]+Velocity="([^"]+)"[^>]*\/>/g;
    let noteMatch;
    
    while ((noteMatch = noteRegex.exec(clipContent)) !== null) {
      notes.push({
        time: parseFloat(noteMatch[1]),
        duration: parseFloat(noteMatch[2]),
        velocity: parseInt(noteMatch[3])
      });
    }
    
    return notes;
  }
  
  // Additional extraction methods...
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
        b: colorInt & 255
      };
    }
    return { r: 128, g: 128, b: 128 };
  }
  
  // Generate semantic map for visualization
  generateSemanticMap() {
    return {
      project: {
        name: this.extractProjectName(),
        tempo: this.tempo,
        timeSignature: this.timeSignature,
        creator: this.creator,
        version: this.version
      },
      tracks: this.tracks.map(track => ({
        ...track,
        visualPosition: this.calculateTrackPosition(track),
        timeline: this.generateTrackTimeline(track)
      })),
      timeline: this.generateGlobalTimeline(),
      structure: this.analyzeProjectStructure()
    };
  }
  
  // Beat-synchronized visualization data
  generateBeatMap(currentBeat) {
    const activeElements = [];
    
    this.clips.forEach(clip => {
      if (this.isClipActiveAtBeat(clip, currentBeat)) {
        activeElements.push({
          type: 'clip',
          clip: clip,
          intensity: this.calculateClipIntensity(clip, currentBeat),
          visualEffect: this.getClipVisualEffect(clip)
        });
      }
    });
    
    return {
      beat: currentBeat,
      tempo: this.tempo,
      activeElements: activeElements,
      visualCues: this.generateVisualCues(currentBeat)
    };
  }
}
```

## Aesthetic Computer Integration

### Visualization Strategy

1. **Minimal Track Display**
   - Each track as colored bar/strip
   - Height represents activity/volume
   - Color from ALS track colors

2. **Beat Synchronization**
   - Use tempo for animation timing
   - Flash/pulse on note triggers
   - Smooth transitions between beats

3. **MIDI Note Visualization**
   - Vertical position = pitch
   - Width = note duration
   - Opacity = velocity

4. **Real-time Animation**
   ```javascript
   function drawMiniDiagram(api, beatPosition) {
     const semanticMap = alsProject.generateSemanticMap();
     const beatMap = alsProject.generateBeatMap(beatPosition);
     
     beatMap.activeElements.forEach(element => {
       if (element.type === 'clip') {
         this.drawClipActivity(api, element, beatPosition);
       }
     });
   }
   
   function drawClipActivity(api, element, beatPosition) {
     const { ink, screen } = api;
     const track = this.findTrackForClip(element.clip);
     
     // Draw beat-synchronized visual
     const intensity = element.intensity;
     const color = track.color;
     
     ink(color.r * intensity, color.g * intensity, color.b * intensity);
     // Draw animated element...
   }
   ```

### Integration with Current ableton.mjs

The enhanced ALSProject class should integrate with the existing minimal sculpture design:

```javascript
// In paint() function
if (alsProject && wavFile && sound.time > 0) {
  const beatPosition = (sound.time * alsProject.tempo) / 60;
  alsProject.drawMiniDiagram(api, beatPosition);
}
```

## Advanced Features for Future Implementation

### 1. Device Parameter Automation
- Extract automation curves
- Visualize parameter changes over time
- Sync visual effects to parameter modulation

### 2. Advanced MIDI Analysis
- Chord detection and visualization
- Rhythm pattern analysis
- Key signature detection

### 3. Audio Analysis Integration
- Extract audio feature data if available
- Sync visual effects to audio characteristics
- Multi-layer visualization combining MIDI and audio

### 4. Interactive Elements
- Click tracks to solo/mute
- Scrub timeline with mouse
- Live parameter control

## Real-World Implementation Examples

Based on research of existing ALS parsing tools, here are proven approaches:

### 1. Basic Decompression (Ruby guard-live-set)
```ruby
# Converts .als files to XML for git version control
Zlib::GzipReader.open(path) do |gz|
  File.open(path + '.xml', 'w') do |file|
    file << gz.read
  end
end
```

### 2. C# Implementation (corrupt-als-dotnet)
```cs
// Blazor app for fixing corrupt ALS files with duplicate IDs
public static async Task<byte[]> DecompressAsync(byte[] input)
{
    using var inmem = new MemoryStream(input);
    using var output = new MemoryStream();
    using var gz = new GZipStream(inmem, CompressionMode.Decompress);
    
    await gz.CopyToAsync(output);
    return output.ToArray();
}
```

### 3. MIDI Note Processing Example
```cs
// From corrupt-als-dotnet - processing MIDI notes with IDs
var midiNoteEvents = keyTrackCollection
    .Elements().Where((c) => c.Name == "KeyTrack")
    .Elements().Where((t) => t.Name == "Notes")
    .Elements().Where((n) => n.Name == "MidiNoteEvent");

foreach (var noteEvent in midiNoteEvents)
{
    var noteId = noteEvent.Attributes().First((a) => a.Name == "NoteId");
    noteId.Value = newUniqueId.ToString();
}
```

### 4. XML Structure Navigation
```cs
// Finding duplicate MIDI note IDs in the XML structure
var nodes = root.Descendants()
    .Where((d) => d.Attributes().Any((a) => a.Name == "NoteId"));
```

### Key Findings from Real Implementations

1. **File Format**: Confirmed .als files are GZIP-compressed XML
2. **Structure**: XML contains hierarchical Track > Clip > Notes organization  
3. **MIDI Notes**: Each MidiNoteEvent has unique NoteId attribute
4. **Navigation**: KeyTracks contain MidiNoteEvent elements with timing/pitch data
5. **Processing**: Standard XML parsing with XPath/LINQ works well

This documentation provides the foundation for building a sophisticated, semantically-aware visualization system for Ableton Live projects within the Aesthetic Computer environment.
