#!/bin/bash
cd . && mkdir Documents/ Downloads/
sudo pacman -Syu
sudo pacman -S --needed git base-devel && cd Documents/ && git clone https://aur.archlinux.org/yay.git && cd yay && makepkg -si

yay -S i3wsr
yay -S Alacritty
yay -S rofi

