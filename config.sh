export DISK_MODEL='Samsung SSD 850'
export ZONEINFO='Europe/London'
export LOCALE='en_US'
export KEYMAP='us'
export FONT='Lat2-Terminus16'
export HOSTNAME='viper.local'

export INSTALL_PKGS=($(cat $CWD/pkgs/explicit-pkgs.pacman))
export INSTALL_GROUPS=($(cat $CWD/pkgs/groups-pkgs.pacman))
export INSTALL_AUR=($(cat $CWD/pkgs/aur-pkgs.pacman))
