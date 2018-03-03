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
	pacstrap /mnt base btrfs-progs ${INSTALL_PKGS[@]} ${INSTALL_GROUPS[@]}

	# TODO: Customize /etc? fstab...
	genfstab -U /mnt >> /mnt/etc/fstab

	announce Configuring chroot Arch
	cp -v install_chroot.sh /mnt/install_chroot.sh
	arch-chroot /mnt /install_chroot.sh
	rm /mnt/install_chroot.sh

	umount -R /mnt
	cryptsetup close cryptroot
	announce Done
}
