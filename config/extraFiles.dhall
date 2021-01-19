-- The real secrets are stored in the `secrets` git branch
let Mode
    : Type
    = Text

let Owner
    : Type
    = Text

let Group
    : Type
    = Text

let Attrs
    : Type
    = { _1 : Optional Mode, _2 : Optional { _1 : Owner, _2 : Group } }

let FileType
    : Type
    = < Regular : { path : Text, content : Text, attrs : Attrs }
      | Directory : { path : Text, attrs : Attrs }
      | WithAttrs : { path : Text, attrs : Attrs }
      >

in  [] : List FileType
