build: mlkit

mlkit: make_parser.sml
	mlkit --output mixml-mlkit mixml.mlb

mlton: make_parser.sml
	mlton -output mixml-mlton -default-ann 'warnUnused true' mixml.mlb

make_parser.sml: make_parser.cmyacc
	cmyacc -o make_parser.sml make_parser.cmyacc

test: mlkit
	go run test.go

update-test: mlkit
	go run test.go -update

.PHONY: build mlkit mlton test update-test
