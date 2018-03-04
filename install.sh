verify_uefi_boot() {
	if [[ ! -d /sys/firmware/efi/efivars ]]
	then
		die 'not booted in UEFI mode'
	fi
}

verify_network_connectivity() {
	ping -c1 archlinux.org >/dev/null
	if [[ $? != 0 ]]
	then
		die 'no network connectivity'
	fi
}

sync_clock() {
	timedatectl set-ntp true
	sleep 2
	if timedatectl status | grep 'synchronized.*no' >/dev/null
	then
		die 'clock not in sync'
	fi
}

install_arch() {
	announce Installing Arch
	cp -v $CWD/mirrorlist /etc/pacman.d
	pacstrap /mnt base btrfs-progs ${INSTALL_PKGS[@]} ${INSTALL_GROUPS[@]}

	genfstab -U /mnt >> /mnt/etc/fstab

	announce Configuring chroot Arch
	cp -v $CWD/install_chroot.sh /mnt
	cp -v $CWD/bootctl/loader.conf /mnt/boot/loader
	cp -v $CWD/bootctl/arch.conf /mnt/boot/loader/entries
	arch-chroot /mnt /install_chroot.sh
	rm -v /mnt/install_chroot.sh

	umount -R /mnt
	cryptsetup close cryptroot
	announce Done
}
