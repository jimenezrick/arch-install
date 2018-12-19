\(luksName : Text) -> \(luksUuid : Text) ->
{ loaderConf = ./systemd-boot/loader.conf as Text
, entries    =
    [ { name = "arch"
      , conf = ./systemd-boot/arch.conf.dhall luksName luksUuid
      }
    , { name = "arch-lts"
      , conf = ./systemd-boot/arch-lts.conf.dhall luksName luksUuid
      }
    ]
}
