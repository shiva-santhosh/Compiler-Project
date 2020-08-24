

%.lex.sml : %.lex 
	mllex $< 

%.grm.sml : %.grm
	mlyacc $<

all : tigc

PHONY : all clean test

tigc: tiger.mlb ast.sml Identation.sml tiger.sml tiger.lex.sml tiger.grm.sml
	mlton -output tigc tiger.mlb

clean : rm tigc
	rm -f *.lex.sml 
	rm -f *.grm.sml

test : tigc 
	${CURDIR}/tigc testprogram
	

