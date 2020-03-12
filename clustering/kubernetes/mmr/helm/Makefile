# running agraph mmr in kubernetes using helm
#
# type
#  % make
# to see the steps you must perform to install the cluster
#

name ?= --generate-name

info:
	@echo
	@echo You can install the mmr cluster in kubernetes
	@echo using a generated release name
	@echo     % make install
	@echo
	@echo or if you wish to choose a name for the cluster:
	@echo     % make name=foo install
	@echo
	@echo 'make install' installs AllegroGraph with no license
	@echo so its capabilities are limited.
	@echo If you have a license in file ~/license.txt then
	@echo use make target  install-with-license
	@echo for example
	@echo    % make file=~/license.txt name=foo install-with-license
	@echo


install:
	helm install $(name) ./agraphmmr

install-with-license:
	helm install --set-file license=$(file) $(name) ./agraphmmr


## make a Helm repository with the agraphmmr chart in it
repository:
	helm package ./agraphmmr
	mkdir -p repo
	mv agraphmmr-*.tgz repo
	helm repo index repo
