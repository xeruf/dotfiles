#!/usr/bin/python
import re
import sys
import os

def convert_to_srt(input_file, output_file):
    with open(input_file, 'r', encoding='utf-8') as f:
        lines = f.readlines()

    srt_lines = []
    counter = 1

    for line in lines:
        match = re.match(r"\[(\d{2}:\d{2}:\d{2}\.\d{3}) --> (\d{2}:\d{2}:\d{2}\.\d{3})\]\s*(.*)", line)
        if match:
            start_time = match.group(1).replace('.', ',')
            end_time = match.group(2).replace('.', ',')
            text = match.group(3).strip()

            if text == '':
                continue  # skip empty subtitles

            srt_lines.append(f"{counter}")
            srt_lines.append(f"{start_time} --> {end_time}")
            srt_lines.append(text)
            srt_lines.append("")  # blank line
            counter += 1

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write('\n'.join(srt_lines))

    print(f"✅ Subtitle saved as: {output_file}")

convert_to_srt(sys.argv[1], sys.argv[2] if len(sys.argv) > 2 else os.path.splitext(sys.argv[1])[0] + ".srt")
