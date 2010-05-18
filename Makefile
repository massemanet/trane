all: 
	(if [ ! -d ebin ]; then mkdir ebin; fi)
	${MAKE} -C src

clean:
	rm -rf ./ebin/*.beam
