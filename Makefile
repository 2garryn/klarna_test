all: clean compile test

clean:
	rm -f ebin/*.beam
compile:
	erlc -o ebin/ src/*
test:
	./rebar3 eunit --dir ebin/