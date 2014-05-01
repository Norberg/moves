while true; do
	make
	inotifywait -e modify -r  . --exclude '(.pyc|.swp|.png)'
done
