default:
	ghc --make GetActivitys.hs
	ghc --make CreateAccessToken.hs
	ghc --make MovesGet.hs
	ghc --make Test.hs
	ghc --make WorkHours.hs

run-test:
	./Test
