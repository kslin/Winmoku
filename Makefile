PROG = gomooku

LIBS = graphics.cma

CAMLC = ocamlc
CAMLDOC = ocamldoc

%.cmo: %.ml
	$(CAMLC) $(CAMLFLAGS) -c $<

TESTFILES = \
	boardstuffs.ml \
	boardcomp.ml \
	board.ml 

FILES = \
	event.ml \
	ImportImage.ml \
	boardstuffs.ml \
	boardcomp.ml \
	board.ml \
	GUI.ml \
	threats.ml \
	mainhelpers.ml

TESTOBJECTS = $(TESTFILES:.ml=.cmo)

OBJECTS = $(FILES:.ml=.cmo)

all: clean run_basic

tests: clean run_tests

$(PROG)_tests: $(OBJECTS) testGetThreats.cmo
	$(CAMLC) $(CAMLFLAGS) $(LIBS) $(TESTOBJECTS) testGetThreats.cmo -o $(PROG)_tests

build_tests: $(PROG)_tests

run_tests: build_tests
	@./$(PROG)_tests

$(PROG)_basic: $(OBJECTS) main.cmo
	$(CAMLC) $(CAMLFLAGS) $(LIBS) $(OBJECTS) main.cmo -o $(PROG)_basic

build_basic: $(PROG)_basic

run_basic: build_basic
	@./$(PROG)_basic



$(PROG)_game: $(OBJECTS) main_game.cmo
	$(CAMLC) $(CAMLFLAGS) $(LIBS) $(OBJECTS) main_game.cmo -o $(PROG)_game

build_game: $(PROG)_game

run_game: build_game
	@./$(PROG)_game


$(PROG)_compete: $(OBJECTS) main_compete.cmo
	$(CAMLC) $(CAMLFLAGS) $(LIBS) $(OBJECTS) main_compete.cmo -o $(PROG)_compete

build_compete: $(PROG)_compete

run_compete: build_compete
	@./$(PROG)_compete


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
	ocamlc -c unix.cma main.ml
	ocamlc -g -o unix.cma gomoku_final main.cmo
	
clean: 
	rm -f *.cmi *.cmo gomoku_final gomooku_basic gomooku_game gomooku_compete

