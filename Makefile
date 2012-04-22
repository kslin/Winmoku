PROG = gomooku

LIBS = graphics.cma

CAMLC = ocamlc
CAMLDOC = ocamldoc

%.cmo: %.ml
	$(CAMLC) $(CAMLFLAGS) -c $<

FILES = \
	event.ml \
	boardstuffs.ml \
	pieceobject.ml \
	boardobject.ml \
	miniboard.ml \
	piece.ml \
	blackpiece.ml \
	whitepiece.ml \
	unoccpiece.ml \
	horizontalboard.ml \
	verticalboard.ml \
	diagrightboard.ml \
	diagleftboard.ml \
	board.ml \
	GUI.ml \
	main.ml \

OBJECTS = $(FILES:.ml=.cmo)

$(PROG)_basic: $(OBJECTS)
	$(CAMLC) $(CAMLFLAGS) $(LIBS) $(OBJECTS) -o $(PROG)_basic

build_basic: $(PROG)_basic

run_basic: build_basic
	@./$(PROG)_basic

#board: $(CAMLC) $(CAMLFLAGS) $(LIBS) $(OBJECTS)
	# ocamlc -c event.ml
	# ocamlc -c boardstuffs.ml
	# ocamlc -c pieceobject.ml
	# ocamlc -c boardobject.ml
	# ocamlc -c miniboard.ml
	# ocamlc -c piece.ml
	# ocamlc -c horizontalboard.ml
	# ocamlc -c verticalboard.ml
	# ocamlc -c diagrightboard.ml
	# ocamlc -c diagleftboard.ml
	# ocamlc -c board.ml
	# ocamlc -g event.cmo boardstuffs.cmo pieceobject.cmo boardobject.cmo miniboard.cmo piece.cmo horizontalboard.cmo verticalboard.cmo diagrightboard.cmo diagleftboard.cmo boardobject.cmo

gomooku: 
	ocamlc -c main.ml
	ocamlc -g -o gomoku_final main.cmo
	
clean: 
	rm -f *.cmi *.cmo gomoku_final

