    let BlockDev = ./blockdev.dhall 

in  { system =
        ./system.dhall 
    , espImage =
        "../imgs/esp.img"
    , rootfsImage =
        "../imgs/rootfs.img"
    , espImageSize =
        "512M"
    , rootfsImageSize =
        "20G"
    , rootDisk =
        BlockDev.DiskModel { model = "Samsung_SSD_850_XXX_XXX_XXX" }
    }
