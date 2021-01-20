# Two variables you may wish to specify on the make command line.
# While it's unlikely that you'll specify testrelease
# note that the value has the form: .t2  (the period is important).
#
# e.g.  make agversion=6.5.0 build
#
dockeraccount = unspecified
agversion=7.0.4 
tag=7.0.4

build:
	docker build -t $(dockeraccount)/agmmr:$(tag) --build-arg agversion=$(agversion) --build-arg $(dockeraccount)/agmmr:$(tag) .

push:
	docker image push $(dockeraccount)/agmmr:$(tag)



