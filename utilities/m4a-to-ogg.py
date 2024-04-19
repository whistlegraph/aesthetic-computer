# m4a-to-ogg
# Converts all m4a audio files in a directory to ogg and keeps the originals.

# Usage: `python m4a-to-ogg.py /home/jas/Desktop/code/aesthetic-computer/system/public/assets/sounds`
#        `python m4a-to-ogg.py /home/jas/Desktop/code/aesthetic-computer/system/public/assets/bgm`
#        `python m4a-to-ogg.py /home/jas/Desktop/code/aesthetic-computer/system/public/assets/prutti/lnl`

import os
import sys
import subprocess

def convert_m4a_to_ogg(directory, quality=8):
    # List all files in the directory
    files = os.listdir(directory)
    
    # Filter for .m4a files
    m4a_files = [file for file in files if file.endswith('.m4a')]
    
    # Convert each .m4a file to .ogg
    for m4a_file in m4a_files:
        full_m4a_path = os.path.join(directory, m4a_file)
        ogg_file = m4a_file.replace('.m4a', '.ogg')
        full_ogg_path = os.path.join(directory, ogg_file)
        
        # Command to convert the file with specified quality
        command = [
            'ffmpeg',
            '-i', full_m4a_path,
            '-c:a', 'libvorbis',  # codec for ogg
            '-y',  # overwrite 
        #    '-qscale:a', str(quality),  # quality level
            full_ogg_path
        ]
        
        # Execute the command
        subprocess.run(command, check=True)
        
        print(f'Converted {full_m4a_path} to {full_ogg_path}')

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: python script.py <directory>")
        sys.exit(1)
    
    directory_path = sys.argv[1]
    convert_m4a_to_ogg(directory_path)