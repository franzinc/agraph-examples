# Two variables you may wish to specify on the make command line.
# While it's unlikely that you'll specify testrelease
# note that the value has the form: .t2  (the period is important).
#
# e.g.  make agversion=6.5.0 build
#
dockeraccount=unspecified
fullagversion=8.5.0
agversion=8.5.0
testrelease=
tag=8.5.0

build:
	docker build --pull \
		--build-arg fullagversion=$(fullagversion) \
		--build-arg agversion=$(agversion) \
		--build-arg testrelease=$(testrelease) \
		-t $(dockeraccount)/agmmr:$(tag) .

push:
	docker image push $(dockeraccount)/agmmr:$(tag)



