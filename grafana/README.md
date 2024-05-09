# AllegroGraph + Grafana

## About

About Grafana from [their website](https://grafana.com/grafana/):

> Query, visualize, alert on, and understand your data no matter where it’s stored. With Grafana you can create, explore, and share all of your data through beautiful, flexible dashboards.

This demo shows how users can fetch data from AllegroGraph (without authentication) in Grafana and render it.

## Launch instruction

In this example, we launch grafana inside a docker container. If you're interested in other ways to install Grafana please see [their documentation](https://grafana.com/docs/grafana/latest/setup-grafana/installation/). All your changes to Grafana will be lost on removing container. To keep it across launches, please see [Grafana documentation](https://grafana.com/docs/grafana/latest/setup-grafana/installation/docker/#save-your-grafana-data).

Execute the following to launch Grafana instance (assuming your current working directory is `agraph-examples/grafana`):
```
docker run -d -p 3000:3000 --name=grafana -e "GF_INSTALL_PLUGINS=yesoreyeram-infinity-datasource" -v $(pwd)/provisioning/datasources:/etc/grafana/provisioning/datasources -v $(pwd)/provisioning/dashboards:/etc/grafana/provisioning/dashboards grafana/grafana-oss
```

Where:

- docker run is a Docker CLI command that runs a new container from an image.
- -d (--detach) runs the container in the background.
- -p <host-port>:<container-port> (--publish) publish a container’s port(s) to the host, allowing you to reach the container’s port via a host port. In this case, we can reach the container’s port 3000 via the host’s port 3000.
- --name assign a logical name to the container (e.g. grafana). This allows you to refer to the container by name instead of by ID.
- -e "GF_INSTALL_PLUGINS=yesoreyeram-infinity-datasource" installs [Infinity datasource plugin for Grafana](https://grafana.com/docs/plugins/yesoreyeram-infinity-datasource/latest/) that allows rendering JSON/CSV/GraphQL/XML data from HTTP responses.
- `-v ./provisioning/datasources:/etc/grafana/provisioning/datasources` mounts configured datasource from demo AllegroGraph instance https://gruff.allegrograph.com/webview without authentication.
- `-v ./provisioning/dashboards:/etc/grafana/provisioning/dashboards` mounts simple dashboards with visualization AllegroGraph data.
- grafana/grafana-oss is the image to run.

After that, you can verify that your Grafana instance is successfully launched by executing `docker ps` and see output similar to the following:
```
# This will display a list of containers that looks like the following:
CONTAINER ID   IMAGE  COMMAND   CREATED  STATUS   PORTS    NAMES
cd48d3994968   grafana/grafana-oss   "/run.sh"   8 seconds ago   Up 7 seconds   0.0.0.0:3000->3000/tcp   grafana
```

If you see the `grafana` container in the output, then you can visit it in your browser (in this example, it's http://localhost:3000). The default user for Grafana instance is admin with `admin` password. If you don't see the `grafana` container please double-check Grafana documentation on launching Grafana instance via docker.

In Grafana web UI, you can click on "Dashboards" and click on "Simple Example" dashboard to see the demo dashboard.

## Grafana dashboard

With successfully loaded dashboard, you should see:
- "Host" text input with `https://gruff.allegrograph.com` value in it. This value is used as host to send SPARQL queries.
- "Repository" text input with `actors-extended` value in it. This value is used as the repository to send SPARQL queries.
- "Predicate frequency" is a bar chart panel that shows predicates and their occurrences in the "Repository".
- "Class frequency" is a pie chart panel that shows objects from triples with rdf:type predicate and their occurrences in the "Repository".
- "Top 10 actors by starred films" is a bar gauge panel chart that shows top 10 actors ordered by numbers of starred films in the "Repository".

To see and edit SPARQL query any chart:
1. Mouse over the top-right corner of the panel and click on the 3 dots icon and click "Edit".
2. Scroll to the "Query" section of the panel and click on "Headers, Body, Request params" button.
3. In the "Body Type" widget, there are "Key" and "Value" columns. "Value" column holds the SPARQL query.
4. To apply post-processing on the SPARQL query response, there is [UQL](https://grafana.com/docs/plugins/yesoreyeram-infinity-datasource/latest/query/uql/) query. Let's see as an example
SPARQL query and appropriate UQL query for the "Predicate frequency" panel.
SPARQL query which counts the number of predicates and returns only the IRI part after the last `/` character:
```
SELECT (STR(COUNT(?pred)) as ?predCounter) (REPLACE(STR(?pred), "^.*/", "") AS ?localName) { 
    ?s ?pred ?o .
}
GROUP BY ?pred
```
UQL query which removes excessive `"` characters for numbers and convert it to number type in Grafana:
```
parse-json # parses string HTTP response as JSON format.
# For AG instance JSON response has the following structure:
# { "names": ["variable1", "variable2"],
#   "values":[["variable-value-1", "variable-value-2"], ...other rows... ]
# }
| project "values" # projects the top-level "values" field from the JSON response 
| extend "predicate"="1" # creates "predicate" column from the second items in row array
| extend "predicate_count_1"=replace_string("0",'"','') # replaces `"` character with empty string to convert "\"42\"" into "42"
| extend "predicate_count"=tonumber("predicate_count_1") # converts "42" into 42
| project-away "0" # removes the original first column from response
| project-away "1" # removes the original second column from response
| project-away "predicate_count_1" # removes intermediate column
```
