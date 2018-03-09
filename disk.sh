find_disk_dev() {
	local model=$1

	if [[ $(lsblk -S -o kname,model | grep "$model" | wc -l) != 1 ]]
	then
		die 'could not identify an unique disk'
	fi

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
	btrfs subvolume create /mnt/@home
	btrfs subvolume create /mnt/@snapshots
	umount /mnt

	mount -o subvol=@ /dev/mapper/cryptroot /mnt
	mkdir /mnt/{boot,home,.snapshots}
	mount $dev_esp /mnt/boot
	mount -o subvol=@home /dev/mapper/cryptroot /mnt/home
	mount -o subvol=@snapshots /dev/mapper/cryptroot /mnt/.snapshots
}
