{ boot =
    ./boot.dhall
, fstabEntries =
    ./fstab.dhall
, espImage =
    "../imgs/esp.img"
, rootfsImage =
    "../imgs/rootfs.img"
, espImageSize =
    "512M"
, rootfsImageSize =
    "20G"
, rootSubvolumes =
    [ { _1 = "@", _2 = "/" }
    , { _1 = "@etc", _2 = "/etc" }
    , { _1 = "@home", _2 = "/home" }
    , { _1 = "@snapshots", _2 = "/.snapshots" }
    ]
}