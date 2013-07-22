open MonadStdlib

val parse : in_channel -> (ParseTree.t, [> failure ]) MonadExn.t
