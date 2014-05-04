while true; do
	make
	make run-test
	inotifywait -e modify -r  . --exclude '(.pyc|.swp|.png)'
done
