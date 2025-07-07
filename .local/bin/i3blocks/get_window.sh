#!/bin/bash

# Nome do workspace atual
WS=$(i3-msg -t get_workspaces | jq -r '.[] | select(.focused==true).name')

# Nome da janela ativa
APP=$(xdotool getactivewindow getwindowname 2>/dev/null)

echo "$WS - $APP"

