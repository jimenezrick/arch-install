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
	local dev_rootfs=${1}2
	local rootfs_uuid=$(lsblk -n -o UUID $dev_rootfs)

	announce Installing Arch
	cp -v $CWD/mirrorlist /etc/pacman.d
	pacstrap /mnt base btrfs-progs ${INSTALL_PKGS[@]} ${INSTALL_GROUPS[@]}

	announce Configuring chroot Arch
	genfstab -U /mnt >> /mnt/etc/fstab

	cp -v $CWD/bootctl/loader.conf /mnt/boot/loader
	$CWD/bootctl/arch.conf.sh $rootfs_uuid >/mnt/boot/loader/entries/arch.conf
	$CWD/bootctl/arch-lts.conf.sh $rootfs_uuid >/mnt/boot/loader/entries/arch-lts.conf

	cp -v $CWD/install_chroot.sh /mnt
	arch-chroot /mnt /install_chroot.sh
	rm -v /mnt/install_chroot.sh

	umount -R /mnt
	cryptsetup close cryptroot
	announce Done
}
