{ rootDiskModel = "Samsung SSD 850"
, zoneInfo      = "Europe/London"
, locale        = "en_US"
, keymap        = "us"
, hostname      = "viper.local"
, fstabEntries  = ./fstab.dhall

, pacmanMirrorlist       = ./pacman/mirrorlist as Text
, pacmanExplicitPackages = ./pacman/explicit.dhall
, pacmanPackageGroups    = ./pacman/groups.dhall
, pacmanAurPackages      = ./pacman/aur.dhall
}
