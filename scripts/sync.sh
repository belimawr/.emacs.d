#! /bin/sh

## System config/files

########## User/system in userspace ##########

# make sure all folders exist
mkdir -p ../dotfiles/.config/gtk-3.0
mkdir -p ../dotfiles/.config/qt5ct
mkdir -p ../dotfiles/.config/systemd/user/
mkdir -p ../dotfiles/.emacs.d
mkdir -p ../dotfiles/.zsh

# Copy files
cp -Rv ~/.emacs.d/custom.el ../dotfiles/.emacs.d
cp -Rv ~/.emacs.d/init.el ../dotfiles/.emacs.d
cp -v ~/.Xresources ../dotfiles/
cp -v ~/.config/i3/config ../dotfiles/.config/i3/config
cp -v ~/.gtkrc-2.0 ../dotfiles/
cp -v ~/.tmux.conf ../dotfiles/
cp -v ~/.xinitrc ../dotfiles/
cp -v ~/.zshrc ../dotfiles/


# Copy folders
cp -Rv ~/.config/gtk-3.0/ ../dotfiles/.config
cp -Rv ~/.config/i3 ../dotfiles/.config/
cp -Rv ~/.config/keepassxc/ ../dotfiles/.config
cp -Rv ~/.config/qt5ct/ ../dotfiles/.config
cp -Rv ~/.config/systemd/user/*.service ../dotfiles/.config/systemd/user
cp -Rv ~/.config/volumeicon ../dotfiles/.config
cp -Rv ~/.zsh/ ../dotfiles/
