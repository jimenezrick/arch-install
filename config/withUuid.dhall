let UUID = ./uuid.dhall

in    λ(uuid : UUID)
    → let handle = { UUID = λ(r : { uuid : Text }) → r.uuid }

      in  merge handle uuid : Text