GL_GIT=git@gitlab.com:rd--/hsc3-data.git
GL_HTTP=https://gitlab.com/rd--/hsc3-data.git

all:
	echo "hsc3-data"

mk-cmd:
	(cd cmd ; make all install)

clean:
	rm -Rf dist dist-newstyle *~
	(cd cmd ; make clean)
	(cd data/roland/d50 ; make clean)

push-gl:
	git push $(GL_GIT)

pull-gl:
	git pull $(GL_HTTP)

push-tags:
	git push $(GL_GIT) --tags

update-rd:
	ssh rd@rohandrape.net "(cd sw/hsc3-data; git pull $(GL_HTTP))"

push-all:
	make push-gl update-rd
