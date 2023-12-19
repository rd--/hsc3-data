all:
	echo "hsc3-data"

mk-cmd:
	(cd cmd ; make all install)

clean:
	rm -Rf dist dist-newstyle *~
	(cd cmd ; make clean)
	(cd data/roland/d50 ; make clean)

push-all:
	r.gitlab-push.sh hsc3-data

indent:
	fourmolu -i Sound

doctest:
	doctest -Wno-x-partial -Wno-incomplete-uni-patterns Sound
