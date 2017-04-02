CC=sbcl --script


all: dungeonworld-tests

dungeonworld-tests: dungeonworld_src.lisp dungeonworld_test.lisp compile.lisp
	$(CC) compile.lisp

test: dungeonworld-tests
	./dungeonworld-tests


clean:
	rm dungeonworld-tests

