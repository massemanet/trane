all: 
	(if [ ! -d ebin ]; then mkdir ebin; fi)
	erl -make

clean:
	rm -rf ./ebin/*.beam
