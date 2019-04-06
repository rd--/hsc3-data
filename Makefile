all:
	echo hsc3-data

clean:
	(cd cmd ; make clean)
	(cd data/roland/d50 ; make clean)

push-rd:
	darcs push -a rd@rohandrape.net:sw/hsc3-data

pull-rd:
	darcs pull -a http://rohandrape.net/sw/hsc3-data
