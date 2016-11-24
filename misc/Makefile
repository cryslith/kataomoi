all: favicon.ico kataomoi-16.png kataomoi-32.png kataomoi-48.png kataomoi-192.png

kataomoi.svg:
	asy -f svg kataomoi.asy

kataomoi-%.png: kataomoi.svg
	convert -size $*x$* -background transparent kataomoi.svg -resize $*x$* $@

favicon.ico: kataomoi-16.png kataomoi-32.png kataomoi-48.png
	icotool -c -o $@ kataomoi-16.png kataomoi-32.png kataomoi-48.png

clean:
	rm *.svg *.png *.ico
