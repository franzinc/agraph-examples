# Installing AllegroGraph with ansible

## Introduction
 This directory contains [ansible](https://www.ansible.com)  playbooks for installing,
starting and stopping an AllegroGraph server on one or
more machines.

  You must edit three files to personalize the configuration
and then you can use the Makefile to install, start and stop the
AllegroGraph servers on one or more machines.
  

## Configuration
 There are a vast number of server configuration parameters
for AllegroGraph, far more than you would want to express as
arguments to a configuration function.



 In this directory there is a file `agraph.cfg-template` that
you should edit to add or modify the
configuration options ([Link Here](https://franz.com/agraph/support/documentation/current/daemon-config.html))
you wish to set.

 The only options you should *not* specify in `agraph.cfg-template` are
``` 
 Port
 SSLPort
```
as these will be added to the final `agraph.cfg` file based on
values you put in `vars.yaml`.

## basedir

 One important variable in `vars.yaml` is `basedir`.  The server
will be installed in a newly created directory that is the
value of `basedir`.
Also a
```
BaseDir
```
directive will be put in the agraph.cfg specifying this value.
This means that inside agraph.cfg you can (and should)
use relative pathanmes to refer to directories and files inside
this directory tree.

## settings
This line is always in the agraph.cfg
```
SettingsDirectory settings
```
It places  the settings directory as a subdirectory
of the `basedir`.
Do not change this line as the settings directory has to be
here in order for the super user password to be installed correctly.


## Installation
 Before starting the installation edit the following files:

### inventory.txt
Insert the name of the machines on which you want the AllegroGraph server to be installed.
Replace the sample machine names already in the file
with the names of your machines.
After you edit the `inventory.txt` file you can type
```
% make
```
to see if the machines you specified are reachable by ansible.


### vars.yaml

That file contains descriptions of the variables to be set as well
as some sample values that you'll need to change.


### agraph.cfg-template

This is the file that will be modified to create the agraph.cfg  that
will be installed with AllegroGraph.
You should review the [server settings](https://franz.com/agraph/support/documentation/current/daemon-config.html) document to see which additional configuration
parameters you wish to specify.

### make install
The command
```
% make install
```
will run though the installation steps to install the server.
It will do a superseding install meaning it will overwrite the server
executables but it will not remove any repositories.  However it is best
to backup your installion before doing the install in case something
unexpected happens and repos are lost.

### make clean-install
If you wish to completely remove an installed AllegroGraph server so
that `make install` gives you a totally fresh directory then
```
% make clean-install
```
will delete everything including repos that are found in subdirectories
of the installation.

### make start
To start the server on all machines do
```
% make start
```

### make stop
To stop the server on all machines do
```
% make stop
```

If you are going to `make install` be sure to stop all servers before doing so.


  


