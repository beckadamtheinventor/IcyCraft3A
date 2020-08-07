#!/bin/bash
#----------------------------------------
#Put your program name in place of "DEMO"
name='IC3CRAFT.8xp'
#----------------------------------------

mkdir "bin" || echo ""

echo "compiling to $name"
fasmg src/main.asm bin/$name
echo "Wrote binary to $name."

read -p "Finished. Press any key to exit"
