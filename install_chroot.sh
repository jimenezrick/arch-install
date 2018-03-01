set -euo pipefail

ln -sf /usr/share/zoneinfo/$ZONEINFO /etc/localtime
hwclock --systohc

sed -i "s/#${LOCALE}.UTF-8 UTF-8/${LOCALE}.UTF-8 UTF-8/" /etc/locale.gen
locale-gen
echo LANG=${LOCALE}.UTF-8 >/etc/locale.conf
echo KEYMAP=$KEYMAP >/etc/vconsole.conf
echo $HOSTNAME >/etc/hostname

passwd
