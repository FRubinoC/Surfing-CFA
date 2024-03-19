all: main.native

main.native: 
	ocamlbuild -use-menhir -pkg unix main.native

clean: 
	rm main.native
	rm -r _build
	rm results
	rm inputs
	rm extresults
	rm finalresults
	rm -r attacks