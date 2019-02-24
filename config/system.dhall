    let BlockDev = ./blockdev.dhall

in  { hostname =
        "viper.local"
    , zoneInfo =
        "Europe/London"
    , locale =
        "en_US"
    , keymap =
        "us"
    , rootDiskModel =
        "Samsung SSD 850" -- XXX: needed?
    , fstabEntries =
        ./fstab.dhall
    , cryptroot =
        BlockDev.Partition
        { diskModel = "Samsung_SSD_840_EVO_250GB", partNum = 2 }
    , pacman =
        ./pacman.dhall
    }
