TARGETC = reversi
SRCS = Board.hs Command.hs Eval.hs Search.hs

all: $(TARGETC)

$(TARGETC): Client.hs $(SRCS)
	ghc -O2 -msse4.2 --make -o $(TARGETC) Client.hs 

clean:
	rm -f *.o
	rm -f *.hi
	rm -f $(TARGETC)
	rm -f $(TARGETC).exe

