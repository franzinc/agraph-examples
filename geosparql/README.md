# Geospatial Queries with AllegroGraph

In these notebooks we will lay out how to structure geospatial RDF data and then perform a variety of interesting SPARQL queries. By extending the capabilities of SPARQL to handle geospatial data, you can perform spatial queries, such as determining proximity, spatial intersections, and more. These queries are highly useful for applications like geographic information systems (GIS), urban planning, environmental monitoring, and any domain where spatial relationships are essential.

## Python Environment

You can create a new python environment with all the required packages with the following command (NOTE: this assumes you are either in an anconda or minicondan base environment already)


### Conda

```shell
conda env create -f environment.yml
```

Please make sure to `activate` your new environment using:

```shell
conda activate geo-example
```

### PIP

Using at least `python 3.10`, please run the following:

```shell
python3 -m venv geo-example
```

Then to activate the environment please run:

```shell
source geo-example/bin/activate
```

and then finally run:

```shell
pip install -r requirements.txt
```

### Launching the notebook

Please launch `Jupyter Lab` by executing in your terminal:

```shell
jupyter lab
```

## The Notebooks

* [geosparql-tutorial](geosparql-tutorial.ipynb) - Gives a more technical explanation of the GeoSPARQL and the necessary structure of the ontology. This demo also provides a few examples.

* [geosparql-examples](geosparql-examples.ipynb) - This notebook demonstrates how to **ETL** geopandas dataframes with `geometry` data into AllegroGraph and then shows the structure most GeoSPARQL queries will follow.
