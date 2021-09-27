# Linux-Laptop

This is my personal laptop setup, this repo contains everything I need
to install Arch Linux from scratch on a new Laptop/PC/VM.

This repo is inspired by Josh Octetz's [Arch Linux and Windows 10
(UEFI + Encrypted) Install
Guide](https://octetz.com/docs/2020/2020-2-16-arch-windows-install/). I
do not have Windows installed, but the Arch Linux instalation with disk
encryption is very detailed and well explained, that's the basis for
my Arch Linux instalation.

# Installation

Follow the tutorial liked above for the detailed step by step, the
next sections detail my specific configuration as well as some small
changes.

## Partitions layout

```
p1 - 512M - EFI - Fat32 - /efi
p2 - 512M - Boot/Grub - ext4 - /boot
p3 -  32G - swap - swap - swap (do I even need that?)
p4 - 920.9G - system - ext4 - / - encrypted
```

## Encrypt and format partitions

```sh
cryptsetup -y --use-random luksFormat /dev/nvme0n1p4
cryptsetup luksOpen /dev/nvme0n1p4 cryptroot

# Format the boot partition as EXT4
mkfs.ext4 /dev/nvme0n1p2

# Format the cryptroot as EXT4
mkfs.ext4 /dev/mapper/cryptroot

# Format EFI partition as FAT32
mkfs.fat -F 32 -n efi  /dev/nvme0n1p1

mkswap /dev/nvme0n1p3
```

## Mounting and installing the system

```sh
mount /dev/mapper/cryptroot /mnt
mkdir /mnt/boot
mount /dev/nvme0n1p2 /mnt/boot
mkdir /mnt/efi
mount /dev/nvme0n1p1 /mnt/efi
swapon /dev/nvme0n1p3
```

## Install initial packages

```sh
pacstrap /mnt linux linux-firmware base base-devel grub efibootmgr git intel-ucode emacs-nox networkmanager openssh wget tmux man-db man-pages
```

## Generate fstab

```sh
genfstab -U /mnt >> /mnt/etc/fstab
```

## Enter the new system (chroot)

```sh
arch-chroot /mnt
```

## Set the timezone and clock

```sh
ln -sf /usr/share/zoneinfo/Europe/Berlin /etc/localtime
timedatectl set-ntp true
hwclock --systohc
```

## Set locale

```sh
# Uncomment en_GB.UTF-8 UTF-8 in /etc/locale.gen.
# generate locale
locale-gen

# Set LANG variable
echo "LANG=en_GB.UTF-8" >> /etc/locale.conf
```

## Set hostname

```sh
echo "TARDIS" >> /etc/hostname
```

## Initial Ramdisk Configuration

```sh
# Add encrypt to HOOKS in /etc/mkinitcpio.conf (order matters).
# Move keyboard before modconf in HOOKS.
HOOKS=(base udev autodetect keyboard modconf block encrypt filesystems fsck)

#Build initramfs with the linux preset.
mkinitcpio -p linux
```

## GRUB Bootloader Setup

```sh
# List all partitions and their UUIDs. Write them down
sudo blkid 

# Edit the GRUB boot loader configuration.
emacs /etc/default/grub

# Update the GRUB_CMDLINE_LINUX to match the format
GRUB_CMDLINE_LINUX="cryptdevice=UUID=<UUID from /dev/nvme0n1p4>:cryptroot root=/dev/mapper/cryptroot"

grub-install --efi-directory=/efi
grub-mkconfig -o /boot/grub/grub.cfg
```

## User Administration

```sh
# Set the root password
passwd

# Add a user
useradd -m -G wheel belimawr
# Set password
passwd belimawr

EDITOR=/usr/bin/emacs visudo
## Uncomment to allow members of group wheel to execute any command
%wheel ALL=(ALL) ALL
```

## Enable Networking

```sh
# Enable NetworkManager to ensure it starts after boot.
systemctl enable NetworkManager
systemctl enable sshd
```

## Reboot

```sh
reboot
```

## Activate multiarch

```sh
# Edit /etc/pacman.conf
sudo emacs /etc/pacman.conf
# And uncomment:
# [multilib]
# Include = /etc/pacman.d/mirrorlist
sudo pacman -Syu # update everything
```

## Clone my linux-laptop repo

```sh
git clone https://github.com/belimawr/linux-laptop.git
cd linux-laptop/
```

# Run the make rules

```sh
make configure-system
make install-base-packages install-devel-packages
make install-yay
make configure-user
```

## Enable services and final configuration

```sh
sudo systemctl enable --now tailscaled
sudo tailscale up
tailscale ip -4 # show the IP address

# enabling .local domains
# Then, edit the file /etc/nsswitch.conf and change the hosts line to include mdns_minimal [NOTFOUND=return] before resolve and dns:
# hosts: ... mdns_minimal [NOTFOUND=return] resolve [!UNAVAIL=return] dns ...
sudo emacs -nw /etc/nsswitch.conf
systemctl enable avahi-daemon

systemctl enable --now bluetooth
systemctl enable --now cups

# Enable ssh-agent
systemctl enable --now --user ssh-agent
```

## Laptop Specific X1 Carbon Gen 9

```sh
pacman -S sof-firmware
```

Configure qt theme: qt5ct, choose breeze

Configure GTK themes: lxappearance, choose breeze, breeze dark

Configure external mixer from `volumeicon` to `pavucontrol`

## SSH agent & KeepassXC

[Using KeepassXC to manage SSH keys](https://ferrario.me/using-keepassxc-to-manage-ssh-keys/)

# Sources

[https://octetz.com/docs/2020/2020-2-16-arch-windows-install/](https://octetz.com/docs/2020/2020-2-16-arch-windows-install/)

[Old/not needed](https://www.notion.so/Old-not-needed-2aef2761fefe49e4801b8ccb8741680a)
