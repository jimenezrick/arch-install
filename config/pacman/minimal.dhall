-- Use this config instead of `pacman.dhall` in `system.dhall` to have a
-- smaller system to test that is quicker to generate
{ mirrorlist = ./mirrorlist.dhall
, packages = ./minimal/packages.dhall
, groups = ./minimal/groups.dhall
, aur = ./minimal/aur.dhall
}
