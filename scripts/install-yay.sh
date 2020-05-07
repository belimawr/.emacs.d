#!/bin/bash

echo
echo "Installing yay"
echo

TEMP_DIR=$(mktemp -d)
CURRENT_DIR=$(pwd)

cd $TMP_DIR
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si --noconfirm

cd $CURRENT_DIR
