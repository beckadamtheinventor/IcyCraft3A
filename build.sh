#!/bin/bash
#----------------------------------------
#Put your program name in place of "DEMO"
name='IC3CRAFT.8xp'
#----------------------------------------

mkdir "bin" || echo ""

echo "compiling to $name"
fasmg src/main.asm bin/IC3CRAFT.8xp
#convbin -i bin/uncompressed.8xp -o bin/$name -j 8x -k 8xp-auto-decompress -n IC3CRAFT
echo "Wrote binary to $name."

read -p "Finished. Press any key to exit"
