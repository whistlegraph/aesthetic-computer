# m4a-to-audio

# Converts all m4a audio files in a directory to ogg and keeps the originals.
# Usage: `python m4a-to-audio.py ogg ~/aesthetic-computer/system/public/assets/sounds`
#        `python m4a-to-audio.py ogg ~/aesthetic-computer/system/public/assets/bgm`
#        `python m4a-to-audio.py ogg ~/aesthetic-computer/system/public/assets/pruttipal/lnl`
import os
import sys
import subprocess

def convert_m4a_to_audio(directory, output_format, quality=8):
    # List all files in the directory
    files = os.listdir(directory)
    
    # Filter for .m4a files
    m4a_files = [file for file in files if file.endswith('.m4a')]
    
    # Convert each .m4a file to the specified format
    for m4a_file in m4a_files:
        full_m4a_path = os.path.join(directory, m4a_file)
        new_file = m4a_file.replace('.m4a', f'.{output_format}')
        full_new_path = os.path.join(directory, new_file)
        
        # Command to convert the file with specified quality
        if output_format == 'webm':
            command = [
                'ffmpeg',
                '-i', full_m4a_path,
                '-c:a', 'libopus',  # codec for Opus audio
                '-b:a', f'{quality}k',  # bitrate for audio quality
                '-y',  # overwrite without asking
                full_new_path
            ]
        elif output_format == 'ogg':
            command = [
                'ffmpeg',
                '-i', full_m4a_path,
                '-c:a', 'libvorbis',  # codec for Vorbis audio
                '-q:a', str(quality),  # quality level for Vorbis
                '-y',  # overwrite without asking
                full_new_path
            ]
        
        # Execute the command
        subprocess.run(command, check=True)
        
        print(f'Converted {full_m4a_path} to {full_new_path}')

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print("Usage: python script.py <output_format> <directory>")
        sys.exit(1)
    
    output_format = sys.argv[1]
    directory_path = sys.argv[2]

    if output_format not in ['webm', 'ogg']:
        print("Error: output_format must be 'webm' or 'ogg'")
        sys.exit(1)
    
    convert_m4a_to_audio(directory_path, output_format)
