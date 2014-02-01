TARGETS = pyramid.svg kpyramid.svg kpyramid-sol.svg \
          litsplus.svg thermo.svg magictapa.svg thermo.svg \
          masyu.svg masyusample.svg

DRAWGRID = ../dist/build/drawgrid/drawgrid
DRAWPYRAMID = ../dist/build/drawpyramid/drawpyramid
DRAWKPYRAMID = ../dist/build/drawkpyramid/drawkpyramid
DRAWMASYU = ../dist/build/drawmasyu/drawmasyu

all: $(TARGETS)

pyramid.svg: pyramid.txt $(DRAWPYRAMID)
	$(DRAWPYRAMID) -w 200 -o $@ < $<

kpyramid.svg: kpyramid.txt $(DRAWKPYRAMID)
	$(DRAWKPYRAMID) -w 200 -o $@ < $<

kpyramid-sol.svg: kpyramid-sol.txt $(DRAWKPYRAMID)
	$(DRAWKPYRAMID) -w 200 -o $@ < $<

litsplus.svg: litsplus.txt $(DRAWGRID)
	$(DRAWGRID) -w 600 -o $@ < $<

thermo: thermo.hs
	ghc --make thermo

thermo.svg: thermo
	./thermo -w 400 -o $@

magictapa: magictapa.hs
	ghc --make magictapa

magictapa.svg: magictapa
	./magictapa -w 400 -o $@

masyu.svg: masyu.txt
	$(DRAWMASYU) -w 300 -o $@ < $<

masyusample: masyusample.hs
	ghc --make masyusample

masyusample.svg: masyusample
	./masyusample -w 400 -o $@

clean:
	rm -f $(TARGETS)
