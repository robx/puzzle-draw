TARGETS = thermo.svg masyusample.svg

all: $(TARGETS)

thermo: thermo.hs
	ghc --make thermo

thermo.svg: thermo
	./thermo -w 400 -o $@

masyusample: masyusample.hs
	ghc --make masyusample

masyusample.svg: masyusample
	./masyusample -w 400 -o $@

clean:
	rm -f $(TARGETS)
