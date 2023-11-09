# AllegroGraph LLM Embedding Examples

Welcome to AllegroGraph's LLM Embedding Example Jupyter notebook. There are two example notebooks in this directory: [AGVector-example.ipynb](AGVector-example.ipynb) is for all non-cloud users. If you are using a repository on our hosted cloud platform please use [AGVector-cloud-example.ipynb](AGVector-cloud-example.ipynb). To start, please run the following commands to create a conda environment.

First, create a new environment
```shell
conda create -n agvector python=3.10
```

then activate the new conda environment with:
```shell
conda activate agvector
```

then run the install environment script

```shell
./create-environment.sh
```


## Setting up OpenAI API Key in AG Webview

To actually perform the SPARQL queries you need to set your OpenAI API key as a query option in webview. Please follow the following instructions to do so.

1. Please navigate to your local installation of the new webview
2. Go to the repository where your data is stored (`llm-philosophy` if you're using the repo created from this demo)
3. Go to `Repository Control` in the left column under `Repository` and search for `query execution options`. Select it.
4. Select `+ New Query Option` and add **openaiApiKey** as the _name_, and your OpenAI api key as the _value_. Set the `Scope` to **Repository**
5. Don't forget to save by hitting `Save Query Options`!

## Steps to deleting existing vector databases

1. In your terminal, navigate to your AllegroGraph installation.
2. `cd` to `/data/rootcatalog/` and then to the repository, in our case `llm-philosophy`
3. `rm *.vdb.*` if you want to delete all existing VDBs associated with that repo, otherwise you can manually `rm` the `llm-philosophy.vdb.vec` and `llm-philosophy.vdb.dat` files.