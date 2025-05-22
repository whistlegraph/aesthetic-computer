// ac-event-daemon, 25.05.22.06.44
// (Requires cargo-watch to be installed as well.)

use evdev::{ Device, InputEventKind, Key };
use rodio::{OutputStream, Sink, Source}; // Removed Sample and SineWave
use std::{ thread, time::Duration, sync::{Arc, atomic::{AtomicBool, Ordering}} }; // Added Arc, AtomicBool, Ordering
use crossterm::{
  cursor,
  event::{self, Event as CEvent, KeyCode, KeyModifiers},
  execute,
  terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen, size},
  style::{Print, SetForegroundColor, Color, ResetColor, Attribute, SetAttribute, SetBackgroundColor},
};
use std::io::{stdout, Write, Error as IoError, ErrorKind as IoErrorKind};

// Helper function to draw the static header
fn draw_static_header(stdout: &mut std::io::Stdout) -> std::io::Result<()> {
    execute!(
        stdout,
        cursor::MoveTo(1, 1),
        SetForegroundColor(Color::White),
        Print("ac-event-daemon"),
        ResetColor
    )?;
    execute!(stdout, SetBackgroundColor(Color::Magenta))?;
    Ok(())
}

#[derive(Clone, Debug, PartialEq)]
enum NoteState {
    Attacking,
    Sustaining,
    Decaying,
    Finished,
}

struct CustomNoteSource {
    frequency: f32, // Single, constant frequency for this note instance
    sample_rate: u32,
    phase: f32,
    
    attack_samples: u64,
    decay_samples: u64,
    sustain_level: f32,

    samples_elapsed: u64, // Counter for samples within the current ADSR state
    
    state: NoteState,
    key_release_requested: Arc<AtomicBool>, 
}

impl CustomNoteSource {
    #[allow(dead_code)]
    pub fn new(
        frequency: f32, // Target frequency for the note
        attack_duration: Duration,
        decay_duration: Duration,
        sustain_level: f32,
        sample_rate: u32,
        key_release_requested: Arc<AtomicBool>,
    ) -> Self {
        CustomNoteSource {
            frequency, // Store the constant frequency
            sample_rate,
            phase: 0.0,
            attack_samples: (attack_duration.as_secs_f32() * sample_rate as f32) as u64,
            decay_samples: (decay_duration.as_secs_f32() * sample_rate as f32) as u64,
            sustain_level,
            samples_elapsed: 0, // For ADSR phase timing
            state: NoteState::Attacking,
            key_release_requested,
        }
    }
}

impl Iterator for CustomNoteSource {
    type Item = f32;

    fn next(&mut self) -> Option<Self::Item> {
        if self.state == NoteState::Finished {
            return None;
        }

        // --- ADSR Envelope Logic ---
        let mut amplitude: f32; // Declare mutable amplitude with its type, no initial assignment
        // self.samples_elapsed tracks samples within the current ADSR state.

        match self.state {
            NoteState::Attacking => {
                if self.samples_elapsed < self.attack_samples {
                    amplitude = self.sustain_level * (self.samples_elapsed as f32 / self.attack_samples as f32);
                } else {
                    amplitude = self.sustain_level;
                    self.state = NoteState::Sustaining;
                    // self.samples_elapsed continues from attack_samples count into sustain
                }
            }
            NoteState::Sustaining => {
                amplitude = self.sustain_level;
                if self.key_release_requested.load(Ordering::SeqCst) {
                    self.state = NoteState::Decaying;
                    self.samples_elapsed = 0; // Reset counter for decay phase
                }
            }
            NoteState::Decaying => {
                // self.samples_jjelapsed counts from 0 for the decay phase
                if self.samples_elapsed < self.decay_samples {
                    amplitude = self.sustain_level * (1.0 - (self.samples_elapsed as f32 / self.decay_samples as f32));
                } else {
                    amplitude = 0.0;
                    self.state = NoteState::Finished;
                }
            }
            NoteState::Finished => return None, // Should be caught by the initial check
        }
        
        amplitude = amplitude.max(0.0); // Ensure amplitude is not negative

        // Square wave generation
        let value = if self.phase < 0.5 { amplitude } else { -amplitude };
        self.phase = (self.phase + self.frequency / self.sample_rate as f32) % 1.0;
        
        self.samples_elapsed += 1; // Increment ADSR phase counter

        Some(value)
    }
}

impl Source for CustomNoteSource {
    fn current_frame_len(&self) -> Option<usize> {
        None // Continuous stream
    }

    fn channels(&self) -> u16 {
        1 // Mono
    }

    fn sample_rate(&self) -> u32 {
        self.sample_rate
    }

    fn total_duration(&self) -> Option<Duration> {
        None // Depends on key press
    }
}

const SAMPLE_RATE: u32 = 192000; // Define a sample rate

fn main() -> std::io::Result<()> {
  let path = "/dev/input/event3";
  let mut stdout = stdout();

  enable_raw_mode()?;
  execute!(stdout, EnterAlternateScreen, cursor::Hide)?;
  execute!(
      stdout,
      SetBackgroundColor(Color::Magenta),
      crossterm::terminal::Clear(crossterm::terminal::ClearType::All)
  )?;
  draw_static_header(&mut stdout)?;
  stdout.flush()?;

  let (_audio_stream, stream_handle) = match OutputStream::try_default() {
      Ok((stream, handle)) => (Some(stream), Some(handle)),
      Err(e) => {
          eprintln!("Failed to open output stream: {}. Audio will be disabled.", e);
          (None, None)
      }
  };
  
  let mut active_sinks: Vec<(Key, Sink, Arc<AtomicBool>)> = Vec::new(); // Key, Sink, ReleaseTrigger
  // let mut current_note_release_trigger: Option<Arc<AtomicBool>> = None; // Managed per sink
  // let mut current_playing_key_code: Option<Key> = None; // Not needed for polyphony in this way
  // let mut last_played_frequency: Option<f32> = None; // Not needed without sliding
  let mut active_keys: Vec<Key> = Vec::new(); // Still useful to know what's held, though not for sliding logic

  const ATTACK_DURATION: Duration = Duration::from_millis(1); // Lowered for latency testing
  // const SLIDE_DURATION: Duration = Duration::from_millis(100); // Removed
  const DECAY_DURATION: Duration = Duration::from_millis(250); 
  const SUSTAIN_LEVEL: f32 = 0.3; // Quieter sustain level for a subtler sound

  fn connect_device(path: &str, stdout: &mut std::io::Stdout) -> std::io::Result<Device> {
      loop {
          match Device::open(path) {
              Ok(d) => {
                  execute!(stdout, SetBackgroundColor(Color::Magenta), crossterm::terminal::Clear(crossterm::terminal::ClearType::All))?;
                  draw_static_header(stdout)?;
                  stdout.flush()?;
                  return Ok(d);
              }
              Err(e) => {
                  execute!(stdout, SetBackgroundColor(Color::Magenta), crossterm::terminal::Clear(crossterm::terminal::ClearType::All))?;
                  draw_static_header(stdout)?;
                  let (cols, rows) = size()?;
                  let error_message = format!("Error opening input device {}: {:?}. Retrying in 5s...", path, e);
                  let err_msg_col = (cols.saturating_sub(error_message.len() as u16)) / 2;
                  let err_msg_row = rows / 2;
                  execute!(stdout, cursor::MoveTo(err_msg_col, err_msg_row), Print(error_message))?;
                  stdout.flush()?;
                  thread::sleep(Duration::from_secs(5));
              }
          }
      }
  }

  let mut dev = connect_device(path, &mut stdout)?;

  loop {
      if event::poll(Duration::from_millis(10))? { // Poll slightly less aggressively
          if let CEvent::Key(key_event) = event::read()? {
              if key_event.code == KeyCode::Char('c') && key_event.modifiers.contains(KeyModifiers::CONTROL) {
                  // Stop all active sinks before exiting
                  for (_key, sink, trigger) in active_sinks.drain(..) {
                      trigger.store(true, Ordering::SeqCst); // Signal release (optional if stopping immediately)
                      sink.stop(); // Force stop
                  }
                  execute!(stdout, ResetColor, LeaveAlternateScreen, cursor::Show)?;
                  disable_raw_mode()?;
                  return Ok(());
              }
          }
      }

      match dev.fetch_events() {
          Ok(iterator) => {
              for ev in iterator {
                  if let InputEventKind::Key(event_key_code) = ev.kind() {
                      let key_char = key_to_char(event_key_code);

                      if ev.value() == 1 { // Key press
                          if !active_keys.contains(&event_key_code) {
                              active_keys.push(event_key_code);
                          }

                          if let Some(sh) = &stream_handle {
                              // For polyphony, we don't stop the previous note, just start a new one.
                              
                              let release_trigger = Arc::new(AtomicBool::new(false));
                              let new_frequency = key_to_frequency(event_key_code);

                              let source = CustomNoteSource::new(
                                  new_frequency, // Just the target frequency
                                  ATTACK_DURATION,
                                  // SLIDE_DURATION, // Removed
                                  DECAY_DURATION,
                                  SUSTAIN_LEVEL,
                                  SAMPLE_RATE,
                                  release_trigger.clone(),
                              );
                              
                              match Sink::try_new(sh) {
                                  Ok(new_sink) => {
                                      new_sink.append(source);
                                      // Add the new sink to our list of active sinks
                                      active_sinks.push((event_key_code, new_sink, release_trigger));
                                  }
                                  Err(e) => eprintln!("Error creating audio sink: {:?}", e),
                              }
                          }

                          execute!(stdout, SetBackgroundColor(Color::Magenta), crossterm::terminal::Clear(crossterm::terminal::ClearType::All))?;
                          draw_static_header(&mut stdout)?;
                          
                          if key_char != '\u{FFFD}' { 
                              let char_str = key_char.to_string();
                              let (cols, rows) = size()?;
                              let col = (cols.saturating_sub(char_str.len() as u16)) / 2;
                              let row = rows / 2;
                              execute!(
                                  stdout,
                                  cursor::MoveTo(col, row),
                                  SetForegroundColor(Color::Yellow),
                                  SetAttribute(Attribute::Bold),
                                  Print(char_str),
                                  ResetColor,
                                  SetBackgroundColor(Color::Magenta),
                                  SetAttribute(Attribute::Reset)
                              )?;
                          }
                          stdout.flush()?;

                      } else if ev.value() == 0 { // Key release
                          active_keys.retain(|&k| k != event_key_code);

                          // Find the sink associated with the released key and trigger its release
                          for (key, _sink, trigger) in active_sinks.iter() { 
                              if *key == event_key_code {
                                  trigger.store(true, Ordering::SeqCst);
                                  // REMOVED: break; // Don't break, allow multiple notes of the same key to be released
                              }
                          }
                      }
                  }
              }
          }
          Err(e) => { 
              // Stop all active sinks on error
              for (_, sink, trigger) in active_sinks.drain(..) {
                  trigger.store(true, Ordering::SeqCst); // Signal release
                  sink.stop(); // Force stop
              }
              // current_playing_key_code = None; // Removed
              // current_note_release_trigger = None; // Removed

              execute!(stdout, SetBackgroundColor(Color::Magenta), crossterm::terminal::Clear(crossterm::terminal::ClearType::All))?;
              draw_static_header(&mut stdout)?;
              let (cols, rows) = size()?;
              let disconnected_message = "Input device disconnected. Attempting to reconnect...";
              let disc_msg_col = (cols.saturating_sub(disconnected_message.len() as u16)) / 2;
              let disc_msg_row = rows / 2;
              execute!(stdout, cursor::MoveTo(disc_msg_col, disc_msg_row), Print(disconnected_message))?;
              stdout.flush()?;
              // Instead of just break, return an error that represents the device disconnection
              // We can wrap the original evdev error 'e' into an std::io::Error if needed, or create a new one.
              // For simplicity, creating a new IoError.
              return Err(IoError::new(IoErrorKind::Other, format!("Input device error: {}", e)));
          }
      }
      
      // Clean up finished sinks
      active_sinks.retain(|(_key, sink, release_trigger)| {
        // Keep if not yet triggered for release OR if triggered but sink is not yet empty.
        // If already triggered for release and sink is empty, it can be removed.
        if release_trigger.load(Ordering::SeqCst) && sink.empty() {
            false // Remove
        } else {
            true  // Keep
        }
      });

      thread::sleep(Duration::from_millis(5)); // Small sleep to reduce CPU usage
  }
  // This part is now unreachable if the loop only exits via Ctrl+C (Ok) or device error (Err)
  // However, if other 'break' statements were added, this would be the default exit.
  // To satisfy the compiler if it thinks there's a path here:
  // Ok(()) 
}

fn key_to_char(key: Key) -> char {
    match key {
        Key::KEY_A => 'a', Key::KEY_B => 'b', Key::KEY_C => 'c', Key::KEY_D => 'd', Key::KEY_E => 'e',
        Key::KEY_F => 'f', Key::KEY_G => 'g', Key::KEY_H => 'h', Key::KEY_I => 'i', Key::KEY_J => 'j',
        Key::KEY_K => 'k', Key::KEY_L => 'l', Key::KEY_M => 'm', Key::KEY_N => 'n', Key::KEY_O => 'o',
        Key::KEY_P => 'p', Key::KEY_Q => 'q', Key::KEY_R => 'r', Key::KEY_S => 's', Key::KEY_T => 't',
        Key::KEY_U => 'u', Key::KEY_V => 'v', Key::KEY_W => 'w', Key::KEY_X => 'x', Key::KEY_Y => 'y',
        Key::KEY_Z => 'z', Key::KEY_1 => '1', Key::KEY_2 => '2', Key::KEY_3 => '3', Key::KEY_4 => '4',
        Key::KEY_5 => '5', Key::KEY_6 => '6', Key::KEY_7 => '7', Key::KEY_8 => '8', Key::KEY_9 => '9',
        Key::KEY_0 => '0', Key::KEY_SPACE => ' ', Key::KEY_ENTER => '\n', Key::KEY_ESC => '\u{238B}', // Corrected ESC char
        _ => '\u{FFFD}', // Corrected replacement char
    }
}

// New function to map keys to frequencies
fn key_to_frequency(key: Key) -> f32 {
    match key {
        // Lower Octave (C2 scale) - Was C3
        // White keys (cdefgab)
        Key::KEY_C => 65.41,  // C2 (was 130.81, C3)
        Key::KEY_D => 73.42,  // D2 (was 146.83, D3)
        Key::KEY_E => 82.41,  // E2 (was 164.81, E3)
        Key::KEY_F => 87.31,  // F2 (was 174.61, F3)
        Key::KEY_G => 98.00,  // G2 (was 196.00, G3)
        Key::KEY_A => 110.00, // A2 (was 220.00, A3)
        Key::KEY_B => 123.47, // B2 (was 246.94, B3)

        // Semitones for C2 octave (vswrq) - Was C3
        Key::KEY_V => 69.30,  // C#2 (was 138.59, C#3)
        Key::KEY_S => 77.78,  // D#2 (was 155.56, D#3)
        Key::KEY_W => 92.50,  // F#2 (was 185.00, F#3)
        Key::KEY_R => 103.83, // G#2 (was 207.65, G#3)
        Key::KEY_Q => 116.54, // A#2 (was 233.08, A#3)

        // Upper Octave (C3 scale) - Was C4
        // White keys (hijklmn)
        Key::KEY_H => 130.81, // C3 (was 261.63, C4)
        Key::KEY_I => 146.83, // D3 (was 293.66, D4)
        Key::KEY_J => 164.81, // E3 (was 329.63, E4)
        Key::KEY_K => 174.61, // F3 (was 349.23, F4)
        Key::KEY_L => 196.00, // G3 (was 392.00, G4)
        Key::KEY_M => 220.00, // A3 (was 440.00, A4)
        Key::KEY_N => 246.94, // B3 (was 493.88, B4)

        // Semitones for C3 octave (tyuop) - Was C4
        Key::KEY_T => 138.59, // C#3 (was 277.18, C#4)
        Key::KEY_Y => 155.56, // D#3 (was 311.13, D#4)
        Key::KEY_U => 185.00, // F#3 (was 369.99, F#4)
        Key::KEY_O => 207.65, // G#3 (was 415.30, G#4)
        Key::KEY_P => 233.08, // A#3 (was 466.16, A#4)
        
        _ => 55.00, // Default to A1 (was 110.0, A2)
    }
}
