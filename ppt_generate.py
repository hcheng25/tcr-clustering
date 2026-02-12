import os
from PIL import Image
import subprocess

root_folder = './'
target_width = 450 # target width
target_height = 500 # target height
md_file = 'result_ppt.md' # markdown file
pptx_file = 'results/result.pptx' # output powerpoint

# resize images if needed
for dirpath, dirnames, filenames in os.walk(root_folder):
    for fname in filenames:
        if fname.lower().endswith(".png"):
            img_path = os.path.join(dirpath, fname)
            
            with Image.open(img_path) as img:
                width, height = img.size
                
                # resize if not target size
                if (width, height) != (target_width, target_height):
                    img_resized = img.resize((target_width, target_height), Image.LANCZOS)
                    img_resized.save(img_path)
                    print(f'Resized {img_path} to {target_width}x{target_height}')
                else:
                    print(f'{img_path} already at target size')

# convert md to pptx
pandoc_cmd = [
    'pandoc',
    md_file,
    '-o', pptx_file
]

subprocess.run(pandoc_cmd)
print('Powerpoint generated')
