
HC := cabal v1-exec -- ghc

SRC := j.hs
SRC += Jassbot/Search.hs Jassbot/Typeof.hs Jassbot/DB.hs Jassbot/Signature.hs
SRC += Jass/Parser.hs Jass/Ast.hs Jass/Printer.hs
SRC += Data/Composeable.hs

j: $(SRC)
	$(HC) j



init:
	cabal v1-sandbox init
	cabal v1-install megaparsec optparse-applicative
