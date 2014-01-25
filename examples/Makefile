TARGETS = pyramid.svg kpyramid.svg kpyramid-sol.svg litsplus.svg

all: $(TARGETS)

pyramid.svg: pyramid.txt
	drawpyramid -w 200 -o pyramid.svg < pyramid.txt

kpyramid.svg: kpyramid.txt
	drawkpyramid -w 200 -o kpyramid.svg < kpyramid.txt

kpyramid-sol.svg: kpyramid-sol.txt
	drawkpyramid -w 200 -o $@ < $<

litsplus.svg: litsplus.txt
	drawgrid -w 400 -o $@ < $<

clean:
	rm -f $(TARGETS)
