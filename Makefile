all:
	echo hsc3-data

clean:
	(cd cmd ; make clean)

push-sp:
	darcs push -a rd@slavepianos.org:sw/hsc3-data

pull-sp:
	darcs pull -a http://rd.slavepianos.org/sw/hsc3-data
