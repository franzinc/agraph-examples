threads ?= 4
queries ?= 50
lifetime ?= 32
initial ?= 7
worker ?= 9999


# once the distribution is extracted this can be used to recompile
# the class files
compile:
	javac -cp "lib/*" com/franz/sessionpool/*.java

# once "make compile" is done and the class files are created this can
# be used to run the demo
# you can set the parameters with
# %  make queries=100 threads=10 run
#
run:
	java -cp "lib/*:." com.franz.sessionpool.LbDemo --threads $(threads)\
           --queries $(queries) --lifetime $(lifetime) --initial $(initial) --worker $(worker)




# this is used when creating 
# a distribution from the main Netbeans directory where the code is developed

dist:
	tar cfz dist.tgz Readme.txt Makefile lib -C src/main/java com

