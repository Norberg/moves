default:
	ghc --make GetActivitys.hs
	ghc --make CreateAccessToken.hs
	ghc --make MovesGet.hs
	ghc --make Test.hs

run-test:
	./Test
