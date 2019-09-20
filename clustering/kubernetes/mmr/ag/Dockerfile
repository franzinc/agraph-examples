FROM centos:7

#
# agraph root is /app/agraph
#

RUN yum -y install net-tools iputils bind-utils wget hostname

ARG agversion=agraph-6.6.0
ARG agdistfile=${agversion}-linuxamd64.64.tar.gz

# This ADD command will automatically extract the contents
# of the tar.gz file
ADD ${agdistfile} .

# needed for agraph 6.7.0 and can't hurt for others
# change to 11 if you only have OpenSSL 1.1 installed
ENV ACL_OPENSSL_VERSION=10

# so prompts are readable in an emacs window
ENV PROMPT_COMMAND=

RUN groupadd agraph && useradd -d /home/agraph -g agraph agraph 
RUN mkdir /app 

# declare ARGs as late as possible to allow previous lines to be cached
# regardless of ARG values

ARG user
ARG password

RUN (cd ${agversion} ;  ./install-agraph /app/agraph -- --non-interactive \
		--runas-user agraph \
		--super-user $user \
		--super-password $password ) 

# remove files we don't need
RUN rm -fr /app/agraph/lib/doc /app/agraph/lib/demos

# we will attach persistent storage to this directory
VOLUME ["/app/agraph/data/rootcatalog"]

# patch to reduce cache time so we’ll see when the controlling instance moves.
# ag 6.7.0 has config parameter StaleDNSRetainTime which allows this to be
# done in the configuration.
COPY dnspatch.cl /app/agraph/lib/patches/dnspatch.cl

RUN chown -R agraph.agraph /app/agraph



