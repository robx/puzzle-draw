TARGETS = pyramid.svg kpyramid.svg kpyramid-sol.svg litsplus.svg

DRAWGRID = ../dist/build/drawgrid/drawgrid
DRAWPYRAMID = ../dist/build/drawpyramid/drawpyramid
DRAWKPYRAMID = ../dist/build/drawkpyramid/drawkpyramid

all: $(TARGETS)

pyramid.svg: pyramid.txt $(DRAWPYRAMID)
	$(DRAWPYRAMID) -w 200 -o $@ < $<

kpyramid.svg: kpyramid.txt $(DRAWKPYRAMID)
	$(DRAWKPYRAMID) -w 200 -o $@ < $<

kpyramid-sol.svg: kpyramid-sol.txt $(DRAWKPYRAMID)
	$(DRAWKPYRAMID) -w 200 -o $@ < $<

litsplus.svg: litsplus.txt $(DRAWGRID)
	$(DRAWGRID) -w 400 -o $@ < $<

clean:
	rm -f $(TARGETS)
