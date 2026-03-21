# If you move this project and don't have GBDK_HOME environment variable set, 
# you can change the directory to match your GBDK root directory (ex: GBDK_HOME = "/GBDK/")

ifndef GBDK_HOME
GBDK_HOME = ../gbdk
endif

CC = $(GBDK_HOME)/bin/lcc
SDAR = $(GBDK_HOME)/bin/sdar

# If you move this project and don't have RGBDS_HOME environment variable set, 
# you can change the directory to match your RGBDS root directory (ex: RGBDS_HOME = "/RGBDS/"

ifndef RGBDS_HOME
RGBDS_HOME = ../rgbds
endif 

RGBASM = $(RGBDS_HOME)/rgbasm

EXAMPLE_DIR = ./build
BUILD_DIR = ./lib
OBJ_DIR = ./obj

LIB_NAME = hUGEDriver.lib

TOOLS_DIR = ../tools

RGB2SDAS = python $(TOOLS_DIR)/rgb2sdas.py
RGB2SDASFLAGS = -b 0

TARGET = $(BUILD_DIR)/$(LIB_NAME)
OBJS = $(OBJ_DIR)/hUGEDriver.o

EXAMPLE = $(EXAMPLE_DIR)/gbdk_player_example.gb

all: directories $(TARGET) $(EXAMPLE)

directories: $(EXAMPLE_DIR) $(BUILD_DIR) $(OBJ_DIR)

$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(EXAMPLE_DIR):
	mkdir -p $(EXAMPLE_DIR)

$(OBJ_DIR)/%.obj:	../%.asm
	$(RGBASM) -i.. -DGBDK -o$@ $<

$(OBJ_DIR)/%.o:	$(OBJ_DIR)/%.obj
	$(RGB2SDAS) $(RGB2SDASFLAGS) -o$@ $<

$(TARGET): $(OBJS)
	$(SDAR) -ru $@ $^

$(EXAMPLE): $(TARGET)
	$(CC) -I../include -Wl-l$(TARGET) -o $@ src/gbdk_player_example.c src/sample_song.c

clean:
	@echo "CLEANUP..."
	rm -rf $(OBJ_DIR)
	rm -rf $(BUILD_DIR)
	rm -rf $(EXAMPLE_DIR)
