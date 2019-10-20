{ boot =
    ./boot.dhall
, fstabEntries =
    ./fstab.dhall
, rootDiskModel =
    "Samsung_SSD_850_PRO_512GB"
, espImage =
    "../imgs/esp.img"
, rootfsImage =
    "../imgs/rootfs.img"
, espImageSize =
    "512M"
, rootfsImageSize =
    "20G"
, rootSubvolumes =
    [ { `1` = "@", `2` = "/" }
    , { `1` = "@etc", `2` = "/etc" }
    , { `1` = "@home", `2` = "/home" }
    , { `1` = "@snapshots", `2` = "/.snapshots" }
    ]
}