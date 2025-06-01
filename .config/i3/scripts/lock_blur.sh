#!/bin/bash

# Caminho temporário para screenshots
SCREENSHOT="/tmp/screen.png"
BLURRED="/tmp/blurred.png"

# Tira screenshot da tela atual
scrot $SCREENSHOT

# Aplica blur na imagem
convert $SCREENSHOT -blur 0x8 $BLURRED

# Executa o i3lock com a imagem borrada
i3lock -i $BLURRED

# Remove os arquivos temporários (opcional)
rm $SCREENSHOT $BLURRED
