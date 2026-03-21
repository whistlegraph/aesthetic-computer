# Simple Game Boy Assembly Examples

This is a collection of simple examples for learners of assembly for the Nintendo Game Boy. They are meant to be primarily single-file examples with minimal dependencies which can be built easily to demonstrate commonly used hardware features of the platform. These are not meant to be good examples of how to structure/build complex projects nor are they meant to be comprehensive tutorials.

I hope someone finds them useful!

A simple batch file (Windows) and bash script (GNU/Linux) are provided for quick conversion/assembly/linking/fixing of the examples. They require [RGBDS](https://rgbds.gbdev.io/) to be accessible, either installed somewhere in the path or copied locally. Please see the [RGBDS installation instructions](https://rgbds.gbdev.io/install) for more information on installing RGBDS.

# Building the Examples

To build an example, navigate to a given example's directory, such as `/src/background-tile` in a terminal, and run:

- Windows: `..\build.bat background-tile.asm`
- Linux: `../build.sh background-tile.asm`

The example should assemble/link/fix, creating a ready-to-run ROM. Symbol and map files will also be generated alongside the ROM file. Some examples include PNG assets which will be converted to 2bpp binary files and/or tilemaps.

# Helpful Resources

- [Pandocs](https://gbdev.io/pandocs/) - The main resource for details on the Game Boy hardware
- [hardware.inc](https://github.com/gbdev/hardware.inc) - The repository for the hardware.inc file (which still gets updates!)
- [GB ASM Tutorial](https://eldred.fr/gb-asm-tutorial/) - A modern and thorough tutorial teaching Game Boy assembly in much greater detail than these examples ever will
- [Awesome Game Boy Development](https://github.com/gbdev/awesome-gbdev) - A curated list of awesome resources for Game Boy development

# Examples

| Name                 | Description                                                           | Screenshot                                                                                                                                |
|----------------------|-----------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------|
|[background-tile](src/background-tile/background-tile.asm)       | Show a single background tile                                         |![background-tile](https://user-images.githubusercontent.com/10489588/186731675-1b7e7483-fbb8-4f5d-ba03-b18f044604f7.png)              |
|[background-tilemap](src/background-tilemap/background-tilemap.asm)    | Show a background tilemap                                             |![background-tilemap](https://user-images.githubusercontent.com/10489588/186731700-fd887c03-a0de-4c83-b0c3-0b1bace5976e.png)              |
|[background-fullscreen](src/background-fullscreen/background-fullscreen.asm) | Show a fullscreen background image (360 unique tiles)                 |![background-fullscreen](https://user-images.githubusercontent.com/10489588/189512741-56bdcc08-67f6-40a1-87b6-bf478940d799.png)              |
|[sprite](src/sprite/sprite.asm)                | Show two sprites manually copied to OAM RAM                           |![sprite](https://user-images.githubusercontent.com/10489588/186731710-10fbb01e-14cd-4744-84dc-c20e4f00db5b.png)                       |
|[oamdma](src/oamdma/oamdma.asm)                | Show 40 sprites copied using OAM DMA                                  |![oamdma](https://user-images.githubusercontent.com/10489588/186731729-c5997f17-951a-4acb-9509-a4c78b160212.png)                       |
|[vblank](src/vblank/vblank.asm)                | Use the vblank interrupt to limit main loop speed with an animation   |![vblank](https://user-images.githubusercontent.com/10489588/186731743-5c48bd21-405d-4d49-a0d5-defd07358a7c.png)                       |
|[joypad](src/joypad/joypad.asm)                | Poll the joypad and display the button states                         |![joypad](https://user-images.githubusercontent.com/10489588/186732972-b61c374e-4e73-435e-aab3-509705a3ef5d.png)                       |
|[grid-collision](src/grid-collision/grid-collision.asm)         | Perform simple collision on a grid                         |![grid-collision-1](https://user-images.githubusercontent.com/10489588/191090618-e9f512ac-2598-4ccc-80cb-a03132896684.png) |


# License

The code in this repository is licensed under the [CC0](https://creativecommons.org/publicdomain/zero/1.0/) license. *To the extent possible under law, all copyright and related or neighboring rights to code presented within simple-gb-asm-examples have been waived. This work is published from Canada.*

The majority of the assets in this repository are licensed under the [CC0](https://creativecommons.org/publicdomain/zero/1.0/) license. Assets not covered by this license are covered by a less permissive CC license, with their licensing information provided in the source file where they are included.
