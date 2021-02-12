# About Haskellish

Haskellish is a library for parsing small Haskell-like languages. It is used in the Estuary project
to parse the language CineCer0, in the TidalCycles project as part of the tidal-parse library, and in
the Punctual project (a web-based audiovisual live coding language). It is meant to be used together
with the haskell-src-exts library - a value of type Exp SrcSpanInfo (eg. parsed by haskell-src-exts from
  Text input) can be fed to a parser of type Haskellish a. Haskellish parsers are Functor, Applicative,
  Alternative, Monad, MonadPlus, and MonadState instances and thus can be composed together a la combinatorial parsing.

# Version History (changelog)

- 0.2.4 - modified error-handling to distinguish fatal and non-fatal cases
- 0.2.3.1 - widened bounds on template-haskell dependency
- 0.2.3 - added functionApplication
- 0.2.2 - added binaryApplication
- 0.2.1 - added ifThenElse
- 0.2.0 - stateful parsing (and MonadState and MonadError instances)
- 0.1.2 - added reverseApplication
- 0.1.1 - added MonadPlus instance
- 0.1.0 - initial Hackage release
