find_disk_dev() {
	local model=$1
	local dev=$(lsblk -S -o kname,model | awk "/$model/"'{print $1}')

	if mount | grep /dev/$dev >/dev/null
	then
		die 'disk is mounted'
	fi
	echo $dev
}

prepare_disk() {
	local dev=$1

	announce Preparing $dev
	parted -s $dev -a optimal \
		mklabel gpt \
		mkpart primary 0% 513MiB \
		mkpart primary 513MiB 100% \
		set 1 boot on

	local dev_esp=${dev}1
	local dev_rootfs=${dev}2

	announce Formatting ESP $dev_esp
	mkfs.fat -F32 $dev_esp

	announce Formatting encrypted rootfs $dev_rootfs
	cryptsetup -y -v luksFormat --type luks2 $dev_rootfs
	cryptsetup --persistent --allow-discards open $dev_rootfs cryptroot
	mkfs.btrfs -f /dev/mapper/cryptroot

	announce Preparing rootfs subvolumes $dev_rootfs
	mount /dev/mapper/cryptroot /mnt
	btrfs subvolume create /mnt/@
	btrfs subvolume create /mnt/@etc
	btrfs subvolume create /mnt/@home
	btrfs subvolume create /mnt/@snapshots
	umount /mnt

	mount -o subvol=@ /dev/mapper/cryptroot /mnt
	mkdir /mnt/boot /mnt/etc /mnt/home /mnt/.snapshots
	mount $dev_esp /mnt/boot
	mount -o subvol=@etc /dev/mapper/cryptroot /mnt/etc
	mount -o subvol=@home /dev/mapper/cryptroot /mnt/home
	mount -o subvol=@snapshots /dev/mapper/cryptroot /mnt/.snapshots
}

install_bootloader() {
	local dev_esp=${1}1

	announce Installing bootloader on $dev_esp
	mkdir /mnt/boot
	mount $dev_esp /mnt/boot

	HOOKS='(base systemd autodetect keyboard sd-vconsole modconf block sd-encrypt filesystems fsck)'
	sed -i "s/^HOOKS=.*/HOOKS=$HOOKS/" /etc/mkinitcpio.conf
	mkinitcpio -p linux
	mkinitcpio -p linux-lts

	bootctl install
}
