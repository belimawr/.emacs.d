install-all-packages:
	$(MAKE) install-base-packages
	$(MAKE) install-extra-packages
	$(MAKE) install-yay
	$(MAKE) install-aur-packages

install-base-packages:
	sudo pacman -Sy --needed $$(<packages/base.txt)

install-devel-packages:
	sudo pacman -Sy --needed $$(<packages/devel.txt)

install-yay:
	scripts/install-yay.sh

install-aur-packages:
	yay -S --needed $$(<packages/aur.txt)

install-fonts:
	sudo pacman -Sy --needed $$(<packages/fonts.txt)
	yay -S --needed $$(<packages/fonts-aur.txt)

configure-user:
	chsh -s /bin/zsh
	cd bin && ./sync-dotfiles.sh

configure-system:
	cd scripts && sudo ./configure-system.sh

sync:
	cd scripts && ./sync.sh
