ARG DockerAccount=specifyaccount

FROM ${DockerAccount}/ag:latest

#
# Agraph root is /app/agraph

RUN mkdir /app/agraph/scripts
COPY . /app/agraph/scripts

# since we only map one port from the outside into our cluster
# we need any sessions created to continue to use that one port.
RUN echo "UseMainPortForSessions true" >> /app/agraph/lib/agraph.cfg

# settings/user will be overwritten with a persistent mount so copy
# the data to another location so it can be restored.
RUN cp -rp /app/agraph/data/settings/user /app/agraph/data/user

ENTRYPOINT ["/app/agraph/scripts/repl.sh"]
