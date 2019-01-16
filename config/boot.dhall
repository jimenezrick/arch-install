\(luksName : Text) -> \(luksUuid : Text) ->
{ loaderConf = ./systemd-boot/loader.conf as Text
, bootEntries =
    [ { bootName = "arch"
      , bootConf = ./systemd-boot/arch.conf.dhall luksName luksUuid
      }
    , { bootName = "arch-lts"
      , bootConf = ./systemd-boot/arch-lts.conf.dhall luksName luksUuid
      }
    ]
}
