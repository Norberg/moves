while true; do
	time cabal test
	inotifywait -e modify -r  . --exclude '(.pyc|.swp|.png)'
done
