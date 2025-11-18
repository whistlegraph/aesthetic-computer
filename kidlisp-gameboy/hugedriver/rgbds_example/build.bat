:: Assemble the hUGEDriver source into an object
rgbasm -ohUGEDriver.obj -i.. ../hUGEDriver.asm

:: Assemble the song into an object
rgbasm -osample_song.obj -i.. sample_song.asm

:: Assemble the example player code into an object
rgbasm -H -oplayer.obj -i.. -DSONG_DESCRIPTOR=sample_song ../player.asm

:: Link the objects together and run rgbfix
rgblink -ooutput.gb -noutput.sym player.obj hUGEDriver.obj sample_song.obj
rgbfix -p0 -fhg output.gb
