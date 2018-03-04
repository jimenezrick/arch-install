set -euo pipefail

ln -sf /usr/share/zoneinfo/$ZONEINFO /etc/localtime
hwclock --systohc

sed -i "s/#${LOCALE}.UTF-8 UTF-8/${LOCALE}.UTF-8 UTF-8/" /etc/locale.gen
locale-gen
echo LANG=${LOCALE}.UTF-8 >/etc/locale.conf
echo KEYMAP=$KEYMAP >/etc/vconsole.conf
echo $HOSTNAME >/etc/hostname

echo Enter root password:
passwd

# Shutdown error: https://github.com/systemd/systemd/issues/8155
HOOKS='(base systemd autodetect keyboard sd-vconsole modconf block sd-encrypt filesystems fsck sd-shutdown)'
sed -i "s/^HOOKS=.*/HOOKS=$HOOKS/" /etc/mkinitcpio.conf
mkinitcpio -p linux
mkinitcpio -p linux-lts
bootctl install
