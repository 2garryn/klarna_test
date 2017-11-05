all: clean compile test

clean:
	rm ebin/*
compile:
	erlc -o ebin/ src/*
test:
	./rebar3 eunit --dir ebin/