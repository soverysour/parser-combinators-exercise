default: ghcid

ghcid-exe:
	-@clear
	-@ghcid --command="stack ghci lexer-and-parser:exe:lexer-and-parser-exe"

ghcid:
	-@clear
	-@ghcid

run:
	-@stack run app-hw-exe

build:
	-@stack build

clean:
	-@stack clean
	-@clear

.PHONY: default run clean ghcid ghcid-exe build
