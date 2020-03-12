FROM centos:7

#
# agraph root is /app/agraph
# rootcatalog at /app/rootcatalog
#

# ensure we have a good set of network programs installed
RUN yum -y install net-tools iputils bind-utils wget hostname openssl

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
