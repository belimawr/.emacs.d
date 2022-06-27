#! /bin/sh

mkdir -p /usr/local/share/kbd/keymaps/
cp ../system-files/usr/local/share/kbd/keymaps/* /usr/local/share/kbd/keymaps/
#localectl set-keymap --no-convert us-caps-2-ctrl
#localectl set-keymap --no-convert uk-caps-2-ctrl
echo "KEYMAP=/usr/local/share/kbd/keymaps/us-caps-2-ctrl.map" >> /etc/vconsole.conf
#echo "KEYMAP=/usr/local/share/kbd/keymaps/uk-caps-2-ctrl.map" >> /etc/vconsole.conf
echo "LANG=en_GB.UTF-8" >> /etc/locale.conf
cat ../system-files/etc/profile >>  /etc/profile
cat ../system-files/etc/environment >> /etc/environment
cp ../system-files/etc/xdg/picom.conf /etc/xdg/picom.conf
