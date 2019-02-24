{ mirrorlist =
    ./pacman/mirrorlist as Text
, explicitPackages =
    ./pacman/explicit.dhall 
, packageGroups =
    ./pacman/groups.dhall 
, aurPackages =
    ./pacman/aur.dhall 
}
