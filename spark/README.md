# Graph Analytics by AllegroGraph and [Apache Spark](https://spark.apache.org/)

This example shows how to read data from AllegroGraph and then perform graph analytics by [Apache Spark](https://spark.apache.org/). Please start by the Jupyter Notebook `AGSpark.ipynb` from the current directory.

If you want to run the example interactively, please install **Docker** and **docker-compose**. In the currently directory, run `make` (or `make demo`) to start a small Spark cluster (one master and two workers), an AllegroGraph instance and the Jupyter Notebook server:

1. visit [localhost:8080](localhost:8080) for the Spark cluster Web UI
2. visit [localhost:10015](localhost:10015) for the AllegroGraph WebView
3. visit [localhost:8088](localhost:8088) for the Jupyter Notebook server
