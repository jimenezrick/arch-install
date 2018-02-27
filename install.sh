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
	if timedatectl status | grep 'synchronized.*no' >/dev/null
	then
		die 'clock not in sync'
	fi
}

install_arch() {
	announce Installing Arch...
	pacstrap /mnt base
	# TODO: btrfs-progs, my groups and pkgs

	# Customize /etc? fstab...
	genfstab -U /mnt >> /mnt/etc/fstab
	arch-chroot /mnt

	ln -sf /usr/share/zoneinfo/$ZONEINFO /etc/localtime
	hwclock --systohc

	sed -i "s/#${LOCALE}.UTF-8 UTF-8/${LOCALE}.UTF-8 UTF-8/" /etc/locale.gen
	locale-gen
	echo LANG=${LOCALE}.UTF-8 >/etc/locale.conf
	echo KEYMAP=$KEYMAP >/etc/vconsole.conf
	echo $HOSTNAME >/etc/hostname

	passwd
	exit # chroot
	umount -R /mnt
}
