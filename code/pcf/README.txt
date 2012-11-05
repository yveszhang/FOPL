pcf.hs: main file, including test cases.
PcfLexer.x, PcfLexer.hs: lexical processing of PCF programs.
PcfParser.y, PcfParser.hs: PCF syntax parsing.
PcfTyping.hs(PcfTyping.plain.hs): direct implementation (without monad) of typing.
PcfTyping.FV.hs: implementation with a monad FV for fresh type variables.
PcfTyping.MaybeT.hs: implementation with two monads (Maybe and FV), using MaybeT as monad transformer.
PcfTyping.FVT.hs: implementation with two monads, using FVT as monad transformer.

