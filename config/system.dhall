{ rootDiskModel = "Samsung SSD 850"
, zoneinfo      = "Europe/London"
, locale        = "en_US"
, keymap        = "us"
, hostname      = "viper.local"
, fstab         = ./fstab.dhall

, pacmanMirrorlist       = ./pacman/mirrorlist as Text
, pacmanExplicitPackages = ./pacman/explicit.dhall
, pacmanPackageGroups    = ./pacman/groups.dhall
, pacmanAurPackages      = ./pacman/aur.dhall
}
