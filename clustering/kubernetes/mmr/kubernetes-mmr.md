# MMR Replication Clusters in Kubernetes 



## Outline of This Document

In this document we describe running a [Multi-Master Replication cluster (MMR)](https://franz.com/agraph/support/documentation/current/multi-master.html)
inside Kubernetes.   We use the [Helm](https://helm.sh/) package manager to
perform the installation.

The document first describes how we build the docker image that is run
in Kubernetes containers.
Then we describe the Kubernetes objects we'll create in order to
run the AllegroGraph MMR cluster in Kubernetes
Finally we'll show how to use Helm to create the MMR cluster in
Kubernetes.

If you just wish to install the cluster but aren't interested in the
low level details you can skip to the end where we show the
Helm command to install the cluster using the files that accompany this document.
You need not create a custom docker image.  There is one prebuilt and stored
on the [Docker Hub](https://hub.docker.com/) that you can use.

Finally if you want to install an AllegroGraph MMR cluster without
reading any further and without downloading any files you can just choose
a unique name for the this particular installation of the chart (here we choose `samplename`)
and:

```
% helm install --repo https://franz.com/ftp/pub/helm/charts samplename agraphmmr
```

By naming each installation of a chart (we chose `samplename`) you can then operate on that
particular installation with other Helm commands.  For example
```
% helm uninstall samplename
```
would remove most objects installed by the `helm install` command.  We'll discuss more
on this below.

## Introduction

MMR replication clusters are different from distributed  AllegroGraph clusters in these important ways:

1. Each member of the cluster needs to be able to make a TCP connection to each other member of the cluster.   The connection is to a port computed at run time.  The range of port numbers to which a connection is made can be constrained by the `agraph.cfg` file but typically this will be a large range to ensure that at least one port in that range is not in use.
2. All members of the cluster hold the complete database (although for brief periods of time they can be out of sync and catching up with one another).

MMR replication clusters don't quite fit the Kubernetes model in these ways

1. When the cluster is running normally each instance knows the DNS name or IP address of each other instance.    In Kubernetes you don't want to depend on the IP address of another cluster's pod as those pods can go away and  a replacement started at a different IP address.   We'll describe below our solution to this.
2. Services are a way to hide the actual location of a pod however they are designed to handle a set of known ports.  In our case we need to connect from one pod to a known-at-runtime port of another pod and this isn't what services are designed for.
3. A key feature of Kubernetes is the ability to scale up and down the number of processes in order to handle the load appropriately.  Processes are usually single purpose and stateless.   An MMR process is a full
database server with a complete copy of the repository.  Scaling up is not a quick and simple operation - the database must be copied from another node.  Thus scaling up is a more deliberate process rather than something automatically done when the load on the system changes during the day.



## The Design

1. We have a headless service for our controlling instance StatefulSet and that causes there to be a DNS entry for the name `releasename-controlling` that
points to the current IP address of the node in which the controlling instance runs.
Using Helm when you install an MMR cluster in Kubernetes you specify a
release name which is then made part of the name for the container to make
the name unique to allow you to run multiple MMR clusters in the
same Kubernetes cluster.  Thus the controlling instance has a name combining
the release name when the MMR cluster was installed and the word "controlling".
We don't need to hardwire the IP address of the controlling instance (as we do in our [AWS load balancer implementation](https://github.com/franzinc/agraph-examples/blob/master/clustering/terraform-elb/using-terraform.md)).
2. The controlling instance uses a Persistent Volume Claim to store the repo we're replicating.  Should the controlling instance AllegroGraph server die (or the pod in which it runs dies) then when the pod is started again it will have access to the data on this persistent volumes.
3. We call the other instances  in the cluster Copy instances.   These are full read-write instances of the repository but we don't back up their data in a persistent volume.  This is because we want to scale up and down the number of Copy instances.  When we scale down we don't want to save the old data since when we scale down we remove that instance from the cluster thus the repo in the cluster can never join the cluster again.   We denote the Copy instances by their IP addresses.  The Copy  instances can find the address of the controlling instance via DNS.  The controlling instance will pass the cluster configuration to the Copy instance and that configuration information will have the IP addresses of the other Copy instances.  This is how the Copy instances find each other.
4. We have a load balancer that allows one to access a random Copy instance from an external IP address.   This load balancer doesn't support sessions so it's only useful for doing queries and quick inserts that don't need a session.
5. We have a load balancer that allows access to the Controlling instance via HTTP.  While this load balancer also doesn't have session support, because there is only one controlling instance it's not a problem if you start an AllegroGraph session because all sessions will live on the single controlling instance.


We've had the most experience with Kubernetes on the Google Cloud Platform.
There is no requirement that the load balancer support sessions and
the GCP version does not at this time, but that doesn't mean that
session support isn't present in the load balancer in other cloud platforms.
Also there is a large community of Kubernetes developers and one may
find a load balancer with session support available from a third party.

## Implementation

We deploy the cluster using Helm. Helm sends commands to a Kubernetes cluster which retrieves
images containing AllegroGraph from the Docker Hub and then runs them in Kubernetes pods.

We supply two directories here.  One is **agmmr** which is where we show how we create the
docker image with AllegroGraph and setup for MMR
stored on the Docker Hub.
The image built by the code in agmmr is already stored under the [franzinc account](https://hub.docker.com/search?q=franzinc&type=image) on the Docker Hub so
you needn't look inside the agmmr directory unless you're curious or want to personalize
the image.
The other directory is **helm** which contains a Helm Chart called agraphmmr which is what directs Helm to
setup the AllegroGraph MMR cluster in a Kubernetes cluster

### Directory agmmr/

In this directory we build a Docker image holding the files necessary
to install AllegroGraph in the container and to create or join an MMR cluster when started.

We don't perform the AllegroGraph installation while building this docker image
because we want to delay specifying certain parameters (shown below) until the
very last second allowing this image to be as generally useful as possible.

The only thing fixed in the docker image is the version of AllegroGraph
server that will run.

We do **not** specify
1. The username of the admin user
2. The password of the admin user
3. The name of the repo in the root catalog we'll replicate
4. The port on which AllegroGraph will listen for  Webview
5. The license to be used when the AllegroGraph server starts

These are all specified just before the docker image is run in a container by Kubernetes.

The Dockerfile is

```
FROM centos:7

#
# agraph root is /app/agraph
# rootcatalog at /app/rootcatalog
#

# ensure we have a good set of network programs installed
RUN yum -y install net-tools iputils bind-utils wget hostname

ARG agversion=6.6.0
ARG testrelease=

ENV agdistfile=agraph-${agversion}-linuxamd64.64.tar.gz
ENV agversion=${agversion}

## Show the curl command we're about to execute
## This useful for debugging curl problems due to invalid arguments
RUN echo will do: curl -f -o ${agdistfile} http://franz.com/ftp/pri/acl/ag/ag${agversion}${testrelease}/linuxamd64.64/${agdistfile}

RUN curl -f -o ${agdistfile} http://franz.com/ftp/pri/acl/ag/ag${agversion}${testrelease}/linuxamd64.64/${agdistfile}

## extract the AllegroGraph distribution
RUN tar xfz ${agdistfile}
RUN rm ${agdistfile}

# This is needed for agraph 6.7.0 and can't hurt for others
# change to 11 if you only have OpenSSL 1.1 installed
ENV ACL_OPENSSL_VERSION=10

# so prompts are readable in an emacs window if we're debugging
# this interactively
ENV PROMPT_COMMAND=

# agraph will be the user running agraph
RUN groupadd agraph && useradd -d /home/agraph -g agraph agraph 
RUN mkdir /app /app/rootcatalog /app/misc /app/secrets

# copy in items we'll need when the docker image runs
COPY . /app/misc


# we will attach persistent storage to this directory
# in the controlling instance only.
VOLUME ["/app/rootcatalog"]


ENTRYPOINT ["/app/misc/runrepl.sh"]
```


You build and push the image to the Docker Hub using the Makefile.
The minimum two arguments you should supply are
* agversion - the form is "6.5.0".
* dockeraccount - a docker account to which you can push images

e.g.
```
% make dockeraccount=myaccount agversion=6.6.0 build push
```

will build and push the docker image (called agmmr) to the account
you specified.  It's important to push the image to the Docker Hub
so that Kubernetes can retrieve it.

Commands in the Dockerfile cause the selected version of AllegroGraph
to be retrieved from the web as a gzip'ed tar file and then
to extract it

The Dockerfile states that when this Docker image is started
the **runrepl.sh** script is run which completes the work of installing
AllegroGraph and creating or joining an MMR cluster.

The runrepl.sh script is this

```
#!/bin/bash
#
## This starts in a container where the AllegroGraph installation
## files are present.  It
## 
##

echo secrets
ls /app/secrets

secretsroot=/app/secrets

port=$(cat $secretsroot/port)
reponame=$(cat $secretsroot/reponame)
authuser=$(cat $secretsroot/user)
authpassword=$(cat $secretsroot/password)

myip=$(hostname -I | sed -e 's/ .*$//')

## put useful information into the console log
## to diagnose startup problems
echo port is $port
echo reponame is $reponame
echo authuser is $authuser
echo authpassword is not shown

echo myip is $myip

## install the agraph server
(cd agraph-${agversion} ;  ./install-agraph /app/agraph -- --non-interactive \
		--runas-user agraph \
		--super-user $authuser \
		--super-password $authpassword ) 

## insert dns patch so we cache dns entries for a very short time
## so that if the controlling instance pod died and is restarted we
## learn about the new IP address of the controlling instance quickly.
cp /app/misc/dnspatch.cl /app/agraph/lib/patches/dnspatch.cl

# personalize the agraph.cfg file with the port selected by
# the user found in the secrets file.  Also add the agraph license
# to the agraph.cfg file.
sed -e s/Config_Port/$port/  /app/misc/agraph.cfg.in > xagraph.cfg

base64 -d /app/secrets/license > license.txt
cat xagraph.cfg license.txt > /app/agraph/lib/agraph.cfg
rm xagraph.cfg license.txt


chown -R agraph.agraph /app/agraph /app/rootcatalog


agtool=/app/agraph/bin/agtool


## start agraph
/app/agraph/bin/agraph-control --config /app/agraph/lib/agraph.cfg start

term_handler() {
    # this signal is delivered when the pod is
    # about to be killed.  We remove ourselves
    # from the cluster.
   echo got term signal
   /bin/bash ./remove-instance.sh
   exit
}

sleepforever() {
    # This unusual way of sleeping allows
    # a TERM signal sent when the pod is to
    # die to then cause the shell to invoke
    # the term_handler function above.
    date
    while true
    do
        sleep 99999 & wait ${!}
    done
}    

if [ -e /app/agraph/data/rootcatalog/$reponame ] ; then
    echo repository $reponame already exists in this persistent volume
    sleepforever
fi    

## the DNS name of the controlling host is specified in the yaml file
## starting this container
controllinghost=${ControllingHost}

controllingspec=$authuser:$authpassword@$controllinghost:$port/$reponame

if [ x$Controlling == "xyes" ] ;
then
   # It may take a little time for the DNS record for $controllinghost to be present
   # and we need that DNS lookup to work because the agtool program below will use it
   until host $controllinghost ; do  echo $controllinghost not in DNS yet; sleep 5 ; done
    
   ## create the first luster instance which is the controlling instance
   $agtool repl create-cluster $controllingspec controlling
    

else
   # wait for the controlling ag server to be running
   until curl -s http://$authuser:$authpassword@$controllinghost:$port/version ; do echo wait for controlling ; sleep 5; done
   echo
   
   # wait for server in this container to be running
   until curl -s http://$authuser:$authpassword@$myip:$port/version ; do echo wait for local server ; sleep 5; done
   echo
    
   # wait for cluster repo on the controlling instance to be present
   until $agtool repl status $controllingspec > /dev/null ; do echo wait for repo ; sleep 5; done
   echo
   
   ## give the instance to be created a unique name within the cluster
   myiname=i-$myip
   echo $myiname > instance-name.txt

   # construct the remove-instance.sh shell script to remove this instance
   # from the cluster when the instance is terminated.
   echo $agtool repl remove $controllingspec $myiname > remove-instance.sh
   chmod 755 remove-instance.sh
   #

   # note that
   #  % docker kill container
   # will send a SIGKILL signal by default.  we can't trap on  SIGKILL.
   # so use
   #  % docker kill -s TERM container
   # in order to test the container's reaction to be commanded to shut down.
   
   trap term_handler SIGTERM SIGHUP SIGUSR1
   trap -p
   echo this pid is $$

   # join the cluster
   echo joining the cluster
   $agtool repl grow-cluster $controllingspec $authuser:$authpassword@$myip:$port/$reponame $myiname
   
fi
sleepforever
```

When the runrepl.sh script is run it's assumed to be running
inside Kubernetes and that there exists a Secret object.
The Secret object contains the username, password, repo name, port number and license.
The license can be an empty string.

The runrepl.sh script retrieves the Secret information
that has been made visible as files in the filesystem inside the container.
We'll see how this is done when we show the Kubernetes configuration files.
Then runrepl.sh installs AllegroGraph, creates the appropriate
agraph.cfg file and then starts AllegroGraph.

After that runrepl.sh wants  AllegroGraph to either recover from a crash,
create a cluster or join a cluster

This script can be run in three different cases:

1. Run when the Controlling instance is starting for the first time
2. Run when the Controlling instance is restarting having run before and died (perhaps the machine on which it was running crashed or the AllegroGraph process had some error)
3. Run when a Copy instance is starting for the first time.   Copy instances are not restarted when they die, instead a new instance is created to take the place of the dead instance.
Therefore we don't need to handle the case of a Copy instance restarting.



In cases 1 and 2 the environment variable *Controlling* will have the value "yes".

In case 2 there will be a directory at `/app/agraph/data/rootcatalog/$reponame`.

In all cases we start an AllegroGraph server.

In case 1 we create a new cluster.   In case 2 we just sleep and let the AllegroGraph server recover the replication repository and reconnect to the other members of the cluster.

In case 3 we wait for the controlling instance's AllegroGraph to be running.  Then we wait for our AllegroGraph server to be running.
Then we wait for the replication repository on the controlling instance we want
to copy to be up and running.
At that point we can grow the cluster by copying the cluster repository.

We also create a script which will remove this instance from the cluster should this pod be terminated.
When the pod is killed (likely due to us scaling down the number of Copy instances) a termination signal will be sent to the shell process allowing it to run this remove script before the pod completely disappears.

We determine the DNS name of the controlling instance by looking at the value of the ControllingHost environment variable.
We can't use a specific name for this because we want to be able to install multiple MMR clusters in a single Kubernetes
cluster and each controlling instance must have a unique name.  The [Helm](https://helm.sh/) installer uses this feature.


## Directory helm/

This directory contains a useful `Makefile` and the directory `agraphmmr` which is called a Helm Chart.
A Helm Chart is a directory tree of files that describe Kubernetes services using yaml as well
as files containing metadata.
When using Helm the yaml files are put in a **templates/** directory as they aren't files to be sent
to Kubernetes as is but are to be preprocessed first by Helm to replace Helm expressions in the
yaml files with the values of the expressions.
This is a huge improvement over normal Kubernetes yaml files where you
end up repeating the same constant multiple times in the files and this makes it
hard to change that constant to a different constant in all places it's used.

`agraphmmr/values.yaml` is a file containing the parameters that Helm will use to evaluate
expressions that begin with `.Values`. 

values.yaml is

```
# Default values for agraphmmr
# This is a YAML-formatted file.
# Declare variables to be passed into your templates.

# set pullPolicy to Always when doing development of
# the docker image

# useful values for pullPolicy
#  Always   - use this during development of the repository
#  IfNotPresent - use this when repository is stable
#
image:
  repository: franzinc/agmmr:1.0
  pullPolicy: Always

replicas: 2

## persistent space to allocate for the controlling instance's
## repository data.  
dataStorage: 20Gi


# these values are put in a secrets file
user: test
password: xyzzy
port: 10035
reponame: myrepl
license:





```
The `image.repository` value is very important.
The one shown above (`franzinc/agmmr:1.0`) refers to the image  we've placed in the franzinc repository.
If you create your own image you'll want to change this entry in values.yaml to name your image.

At the end of `values.yaml` you'll see the five parameters that will become secrets
and will be used when the image is run in a container.

To change these values you can edit the values.yaml file.
Alternatively to set the parameters for just one release
without modifying `values.yaml` you can use the --set argument to `helm install`

```
% helm install --set password=123hellomac relname ./agraphmmr
```

The value for `license` in values.yaml is empty meaning that the AllegroGraph cluster
will start servers with no license. That will restrict their operation.

If you wish to use your license in the cluster look at the `install-with-license` rule
in the `Makefile`.  You'll see that the license file must be converted into a single-line base64
encoded string representation.   This encoding will preserve the newlines in the license file.


The yaml files are in **agraphmmr/templates** and we'll describe them next.

### secrets.yaml

A Secret is created by this yaml file.  This is how some of the parameters in values.yaml
are made visible inside Kubernetes containers

```
kind: Secret
apiVersion: v1
metadata:
  name: {{ .Release.Name }}-agraphsecrets
stringData:
  user: "{{ .Values.user }}"
  password: "{{ .Values.password }}"
  port: "{{ .Values.port }}"
  reponame: "{{ .Values.reponame }}"
  license: "{{ .Values.license }}"
```


### controlling-service.yaml

This defines the service that puts the
controlling instance's IP address in DNS so other pods can locate the pod for the controlling instance.
In the `runrepl.sh` script above we include a test to check when the DNS information is
present before allowing the application to proceed.

The expression `{{ .Release.Name }}` returns the name given when  `helm install` was done.
For example if you do `helm install frob ./agraphmmr` then the release name is frob.

Note that we use the port value from values.yaml with `{{ .Values.port }}`

```
#
# this service exposes the internal IP address
# of the controlling instance and makes it
# available via dns at the
# name 'releasename-controlling'
# using the dns search list
#
apiVersion: v1
kind: Service
metadata:
 name: {{ .Release.Name }}-controlling
spec:
 clusterIP:  None
 selector:
   app: {{ .Release.Name }}-controlling
 ports:
 - name: http
   port: {{ .Values.port }}
   targetPort: {{ .Values.port }}
   
   
```



This selector defines a service for any
container with a label with a key `app` and a value `releasename-controlling`.
It doesn't matter if a matching app doesn't exist when this Service object is created.
When the app eventually does exist the link will be made.
Thus the order in which you create Kubernetes objects is not important.

There will be only one controlling app.  

### copy-service.yaml

We do a similar service for all the copy applications.  There will usually
be more than one copy app.


```
#
apiVersion: v1
kind: Service
metadata:
 name: {{ .Release.Name }}-copy
spec:
 clusterIP: None
 selector:
   app: {{ .Release.Name }}-copy
 ports:
 - name: {{ .Release.Name }}-main
   port: {{ .Values.port }}
   targetPort: {{ .Values.port }}

```




### controlling.yaml

This is the most complex resource description for the cluster.
We use a StatefulSet so we have a predictable name for the single pod
we create.
The name of the container will always be  releasename-controlling-0.
The name put in DNS will be releasename-controlling (by virtue of the
service definition shown above). Since we'll only have one container
for this StatefulSet the DNS name releasename-controlling will be the
IP address of the container releasename-controlling-0.

We define a persistent volume to hold the repository
for the controlling instance.  A StatefulSet is designed
to control more than one pod so rather than a VolumeClaim we
have a VolumeClaimTemplate so that each Pod can have its own
persistent volume… but as it turns out we have only one pod in
this set and we never scale up.

There must be exactly one controlling instance.  While we could easily change the number
of replicas of this StatefulSet
we must never set the replicas value to any number other than 1.

 We setup a liveness check so that if the AllegroGraph server dies Kubernetes will restart the pod and thus the AllegroGraph server.   Because we've used a persistent volume for the AllegroGraph repositories when the AllegroGraph server restarts it will find that there is an existing MMR replication repository that was in use when the AllegroGraph server was last running. AllegroGraph will restart that replication repository which will cause that replication instance to reconnect to all the copy instances and become part of the cluster again.
 
We set the environment variable `Controlling` to `yes` and this causes this container to start up as a controlling instance (you'll find the check
for the `Controlling` environment variable in the `runrepl.sh` script above).
We set the environment variable `ControllingHost` to the name of this controlling instance.  The name
includes the Release Name so it's unique within this Kubernetes cluster.

We have a volume mount for `/dev/shm`, the shared memory filesystem, because
the default amount of shared memory allocated to a container by Kubernetes
is too small to support AllegroGraph.

We mount a volume of secrets.
A volume named releasename-agraphsecrets
contains the parameters to control the installation and use of AllegroGraph
within this cluster.  This Secret object has a name unique to this release.
The Secret values are visible in the filesystem in the /app/secrets directory.
By putting the values in the filesystem and not the environment (our other option) we
ensure that environment dumps of this container won't reveal the admin user name
and password.


```
#
# stateful set of controlling instance
#

apiVersion: apps/v1beta1
kind: StatefulSet
metadata:
  name: {{ .Release.Name }}-controlling
spec:
  serviceName: {{ .Release.Name }}-controlling
  replicas: 1
  template:
    metadata:
      labels:
        app: {{ .Release.Name }}-controlling
    spec:
        containers:
        - name: {{ .Release.Name }}-controlling
          image: {{ .Values.image.repository }}
          imagePullPolicy: {{ .Values.image.pullPolicy }}
          livenessProbe:
            httpGet:
              path: /version
              port: {{ .Values.port }}
            initialDelaySeconds: 30
          volumeMounts:
          - name: shm
            mountPath: /dev/shm
          - name: {{ .Release.Name }}-data
            mountPath: /app/rootcatalog
          - name: secrets
            mountPath: /app/secrets
            readOnly: true
          env:
          - name: Controlling
            value: "yes"
          - name: ControllingHost
            value: "{{ .Release.Name }}-controlling"
        volumes:
        - name: shm
          emptyDir:
             medium: Memory
        - name: secrets
          secret:
             secretName: {{ .Release.Name }}-agraphsecrets
  volumeClaimTemplates:
         - metadata:
            name: {{ .Release.Name }}-data
           spec:
            resources:
              requests:
                storage: {{ .Values.dataStorage }}
            accessModes:
            - ReadWriteOnce


```

### copy.yaml

This StatefulSet is responsible for starting all the other instances.    It's much simpler as it doesn't use Persistent Volumes

```
#
# stateful set of copies of the controlling instance
#

apiVersion: apps/v1beta1
kind: StatefulSet
metadata:
  name: {{ .Release.Name }}-copy
spec:
  serviceName: {{ .Release.Name }}-copy
  replicas: {{ .Values.replicas }}
  template:
    metadata:
      labels:
        app: {{ .Release.Name }}-copy
    spec:
        volumes:
         - name: shm
           emptyDir:
             medium: Memory
         - name: license
           secret:
              secretName: aglicense
         - name: secrets
           secret:
               secretName: {{ .Release.Name }}-agraphsecrets
        containers:
        - name: {{ .Release.Name }}-controlling
          image: {{ .Values.image.repository }}
          imagePullPolicy: {{ .Values.image.pullPolicy }}
          livenessProbe:
            httpGet:
              path: /version
              port: {{ .Values.port }}
            initialDelaySeconds: 30
          volumeMounts:
          - name: shm
            mountPath: /dev/shm
          - name: secrets
            mountPath: /app/secrets
            readOnly: true
          - name: license
            mountPath: /app/license
            readOnly: true
          env:
          - name: ControllingHost
            value: "{{ .Release.Name }}-controlling"



```


### controlling-lb.yaml

We define a load balancer so applications on the internet outside of our cluster can communicate with the controlling instance.    The IP address of the load balancer isn't specified here.   The cloud service provider (i.e. Google Cloud Platform or AWS) will determine an address after a minute or so and will make that value visible if you run

```
% kubectl get svc 
```

The file is

```
apiVersion: v1
kind: Service
metadata:
  name: {{ .Release.Name }}-controlling-loadbalancer
spec:
  type: LoadBalancer
  ports:
  - port: {{ .Values.port }}
    targetPort: {{ .Values.port }}
  selector:
    app: {{ .Release.Name }}-controlling

```


### copy-lb.yaml

As noted earlier the load balancer for the copy instances does not support sessions.   However you  can use the load balancer to issue queries or simple inserts that don't require a session.


```
apiVersion: v1
kind: Service
metadata:
  name: {{ .Release.Name }}-copy-loadbalancer
spec:
  type: LoadBalancer
  ports:
  - port: {{ .Values.port }}
    targetPort: {{ .Values.port }}
  selector:
    app: {{ .Release.Name }}-copy

```    


### copy-0-lb.yaml

If you wish to access one of the copy instances explicitly so that you can create sessions you can create a load balancer which links to just one instance, in this case the first copy instance which is named "copy-0".

```
apiVersion: v1
kind: Service
metadata:
  name: {{ .Release.Name }}-copy-0-loadbalancer
spec:
  type: LoadBalancer
  ports:
  - port: {{ .Values.port }}
    targetPort: {{ .Values.port }}
  selector:
    app: {{ .Release.Name }}-copy
    statefulset.Kubernetes.io/pod-name: {{ .Release.Name }}-copy-0

```





### Running MMR using Helm

[Helm](https://helm.sh/) is a package manager for Kubernetes.
The collection of Kubernetes objects
an application needs are defined in yaml files and collected into what's called a *Chart* in Helm.
A Chart is simply a directory tree containing yaml files and related information.
Helm gives you the ability to parameterize the yaml files, something Kubernetes is sorely lacking
(forcing everyone using just Kubernetes to write their own
parameterization system usually using sed scripts).

Helm also allows you to consider the service in a chart to be a unit.  You can install a chart
and all the Kubernetes objects are created. You can then uninstall the chart and the objects are
removed (except persistent Volume Claims) solving a problem in using Kubernetes of finding
and removing all the objects you've added
when you wish to remove your application from the Kubernetes cluster.

Helm can also do upgrades.  You change your Chart and then ask Helm to upgrade and Helm will
figure out what has to be removed or added to make that happen with the fewest changes.

The Helm Chart to install using Helm is in the helm directory.
We'll assume below that you've cd'ed into the helm directory.

The helm directory contains one Chart: agraphmmr.
The yaml files are in agraphmmr/templates.
These yaml files include Helm expressions like `{{ .Values.port }}` so these
yaml files can't be used as is in Kubernetes.
The templating engine in Helm takes files with a yaml extension in agraphmmr/templates
and processes them, converting Helm expressions to legal yaml syntax.
The file agraphmmr/values.yaml specifies the values of parameters that are
referenced as `{{ .Values.paramname }}`.
Some parameters come from other places.  For example `{{ .Release.name }}` is the
release name given by the user or auto generated when a Chart is installed.

The `helm install` command operates on a Helm Chart stored in a number
of different formats.
A Helm Chart can be directory tree,  a gzip'ed tar of the directory tree,
a URL  that returns a gzip'd tar of a directory tree
or a retrieved by name from a Helm repository either local or on the internet.

To use Helm you need to have access to a Kubernetes cluster via `kubectl`.
You can test that you have access with

```
% kubectl get pods
```
and if that doesn't give you an error message then you're ready to proceed.


#### Quick Install

You can install the AllegroGraph MMR cluster from the files accompanying
this document with one command:
```
% helm install somename  ./agraphmmr
```

The name you choose (here `somename`) will identify this particular installation
of this chart.  This is refered to as the *release name*.
You can use the release name in subsequent Helm commands to operate
on this particular installation.

Alternatively you can install it from the repository on the franz.com web site.

```
% helm install --repo https://franz.com/ftp/pub/helm/charts somename agraphmmr
```



After a minute or two if you 

```
kubectl get svc
```
you'll see a line like

```
somename-controlling-loadbalancer   LoadBalancer   10.12.1.46    35.224.156.23   10035:30551/TCP   30m
```
which tells us that we can connect to Webview on the controlling instance at `http://35.224.156.23:10035`
Since we didn't change the defaults, you can log in with the user name `test` and the
password `xyzzy`.
Then examine the `myrepl` repo and then click on the link `Manage Replication Instances as Controller`.
You'll see three instances in this MMR cluster.

If you want to remove all of the objects added to Kubernetes by
this install (except the Persistent Volume Claim)
then do something like

```
% helm uninstall somename
```
This will not remove the Persistent Volume Claims however.
You'll have to
```
% kubectl get pvc
```
and then for each volume to remove
```
% kubectl delete pvc pvcname
```
The names to look for are those ending in controlling-0.

#### Specify a license

An AllegroGraph license is a text file.  In order to make the AllegroGraph servers
running in containers in Kubernetes use your license file you'll have to specify
the license when you `helm install`.

You can place the contents of the license file in `agraphmmr/values.yaml`.
The license file is a multiline object and you must use the yaml syntax in order
to insert that file in `values.yaml`.
The yaml syntax requires that you use a pipe character as shown below and then
indent each of the following lines the same amount using spaces and not tabs


```
license: |
  Licensee Spacely Sprockets
  LicenseLimit no-limit
  LicenseExpires no-expiration
  <LicenseCode>
  1244234134123412341234123412341234432234
  3434245251351345234523452345234523452344
  9087498234148239481239482394123982349123
  </LicenseCode>
```


Alternatively you can specify the license on the command line.  

```
% helm install --set-file license=/path/to/license.txt somename ./agraphmmr
```

or using the Makefile
```
% make file=/path/to/license.txt name=somename install-with-license
```



#### Change Parameters

There are other parameters you might wish to change on the `helm install` command line.

If you call `helm install` directly (instead of `make install`) you can
override values found in values.yaml on the command line as in
the following where we get the password out of the shell environment.

```
% helm install --set password=$ClustPass relxxx ./agraphmmr
```

Some parameters can be changed while the MMR cluster is running.
For example you could change the number of replicas in values.yaml and then

```
% helm upgrade somename ./agraphmmr
```
(where `somename` is the name you specified when you installed this chart)
and this will cause the number of copy instances to change to be the value
given for `replicas`.

#### Troubleshooting
The most common problem we've seen in getting the AllegroGraph MMR cluster to connect is
a result of failing to remove the Persistent Volume Claim that was created when the
`helm install` was done.

This problem will occur if you
```
% helm install myname ./agraphmmr
% helm uninstall myname
% helm install myname ./agraphmmr
```

Helm doesn't create the Persistent Volume Claim so it doesn't know to remove it when
you run `helm uninstall`.

If you `helm install foo ./agraphmmr`, then `helm uninstall foo` the Persistent Volume Claim
whose name begins with `foo-` remains.  Should you then `helm install foo ./agraphmmr`, the runrepl.sh
script will think that the controlling instance has crashed and restarted instead of thinking
that it should create a new controlling instance.

Therefore you should remove Persistent Volume Claims with kubectl (or the web interface
in Google Cloud Platform) after an uninstall and definitely before reusing a release name.

Alternatively when experimenting with this chart choose different release names each time

```
% helm install myname1 ./agraphmmr
% helm uninstall myname1
% helm install myname2 ./agraphmmr
% helm uninstall myname2
```
and so on.  Don't forget to remove all the Persistent Volume Claims that still exist
that you no longer need.




