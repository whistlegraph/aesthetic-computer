# live/ — speech-to-singing performance core.
#
# singer.c is the SHARED CORE: pure DSP, no audio device, no I/O. Menu Band
# (AVAudioEngine) and fedac/native (ALSA) both link it and supply their own
# device — the same pattern as gm_synth.c, which fedac owns and Menu Band
# symlinks. main.c is only the standalone driver; each host replaces it.
#
#   make            → ./singer            (standalone, miniaudio)
#   make libsinger.a → static lib to link from Swift / fedac
CC      = clang
CXX     = clang++
CFLAGS  = -O3 -ffast-math -Iworld -Wall
CXXFLAGS= -O3 -ffast-math -Iworld
LDFLAGS = -lc++ -lm -lpthread \
          -framework CoreAudio -framework CoreFoundation \
          -framework AudioToolbox -framework AudioUnit
WOBJ    = $(patsubst %.cpp,%.o,$(wildcard world/*.cpp))

singer: main.o singer.o $(WOBJ)
	$(CXX) -O3 $^ -o $@ $(LDFLAGS)

# the embeddable core — no device, no main()
libsinger.a: singer.o $(WOBJ)
	ar rcs $@ $^

%.o: %.c   ; $(CC)  $(CFLAGS)   -c $< -o $@
%.o: %.cpp ; $(CXX) $(CXXFLAGS) -c $< -o $@

clean: ; rm -f singer libsinger.a *.o world/*.o
.PHONY: clean
