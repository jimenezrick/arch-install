{ system =
    ./system.dhall
, boot =
    ./boot.dhall
, espImage =
    "../imgs/esp.img"
, rootfsImage =
    "../imgs/rootfs.img"
, espImageSize =
    "512M"
, rootfsImageSize =
    "20G"
}