#ifndef AC_USB_MIDI_H
#define AC_USB_MIDI_H

typedef struct {
    int enabled;
    int active;
    char reason[64];
    char udc[128];
    char port[64];
    char power_role[32];
    char data_role[32];
    char alsa_device[64];
    char serial[64];
} ACUsbMidiState;

int usb_midi_read_state(ACUsbMidiState *state);
void usb_midi_close(void);
int usb_midi_note_on(int note, int velocity, int channel);
int usb_midi_note_off(int note, int velocity, int channel);
int usb_midi_all_notes_off(int channel);

#endif
