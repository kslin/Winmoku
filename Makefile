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
  minimax.ml \
	mainhelpers.ml \
	sequenceBoards.ml

TESTOBJECTS = $(TESTFILES:.ml=.cmo)

OBJECTS = $(FILES:.ml=.cmo)

all: clean run_basic

tests: clean run_tests

$(PROG)_tests: $(OBJECTS) testGetThreats.cmo testInsert.cmo testBoardFuncs.cmo
	$(CAMLC) $(CAMLFLAGS) $(LIBS) $(TESTOBJECTS) testGetThreats.cmo testInsert.cmo testBoardFuncs.cmo -o $(PROG)_tests

build_tests: $(PROG)_tests

run_tests: build_tests
	@./$(PROG)_tests


$(PROG)_basic: $(OBJECTS) main.cmo
	$(CAMLC) $(CAMLFLAGS) $(LIBS) $(OBJECTS) main.cmo -o $(PROG)_basic

build_basic: $(PROG)_basic

run_basic: build_basic
	@./$(PROG)_basic

$(PROG)_move: $(OBJECTS) main_move.cmo
	$(CAMLC) $(CAMLFLAGS) $(LIBS) $(OBJECTS) main_move.cmo -o $(PROG)_move

build_move: $(PROG)_move

run_move: build_move
	@./$(PROG)_move

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


clean: 
	rm -f *.cmi *.cmo gomooku_basic gomooku_game gomooku_compete

