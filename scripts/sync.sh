#! /bin/sh

## System config/files

########## User/system in userspace ##########

# make sure all folders exist
#mkdir -p ../dotfiles/.config/i3
mkdir -p ../dotfiles/.config/gtk-3.0
mkdir -p ../dotfiles/.config/qt5ct
mkdir -p ../dotfiles/.config/volumeicon

# Copy files
cp -v ~/.Xresources ../dotfiles/
cp -v ~/.config/i3/config ../dotfiles/.config/i3/config
cp -v ~/.gtkrc-2.0 ../dotfiles/
cp -v ~/.tmux.conf ../dotfiles/
cp -v ~/.xinitrc ../dotfiles/
cp -v ~/.zshrc ../dotfiles/

# Copy folders
cp -Rv ~/.config/gtk-3.0/ ../dotfiles/.config
cp -Rv ~/.config/i3 ../dotfiles
cp -Rv ~/.config/qt5ct/ ../dotfiles/.config
cp -Rv ~/.config/volumeicon ../dotfiles/volumeicon
cp -Rv ~/.emacs.d ../dotfiles/
cp -Rv ~/.zsh/ ../dotfiles/
