# AllegroGraph LLM Embedding Examples

We recommend you run the [Embedding-with-Properties.ipynb](Embedding_with_Properties.ipynb) notebook to see how embedding and vector stores work in AllegroGraph `>= 8.1.0`. The [AGVector-example.ipynb](AGVector-example.ipynb) notebook works but its methods have been greatly improved upon. It was created for versions `8.0.0` and `8.0.1`. 

## Installation
Welcome to AllegroGraph's LLM Embedding Example Jupyter notebook. To start, please run the following commands to create a conda environment.

### Conda
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

### pip

first create a new environment with:

```shell
python3.11 -m venv venv
```

Activate it using:

```shell
source venv/bin/activate
```

then install the requirements with:

```shell
pip install -r requirements.txt
```


## Setting up OpenAI API Key in AG Webview for the `AGVector-example.ipynb` Notebook only

This only applies to agraph `8.0.0` and `8.0.1`.

To actually perform the SPARQL queries you need to set your OpenAI API key as a query option in webview. Please follow the following instructions to do so.

1. Please navigate to your local installation of the new webview
2. Go to the repository where your data is stored (`llm-philosophy` if you're using the repo created from this demo)
3. Go to `Repository Control` in the left column under `Repository` and search for `query execution options`. Select it.
4. Select `+ New Query Option` and add **openaiApiKey** as the _name_, and your OpenAI api key as the _value_. Set the `Scope` to **Repository**
5. Don't forget to save by hitting `Save Query Options`!

## Steps to deleting existing vector databases

Deletion of vector databases has been added to AGWebView. This is only relevant to versions `8.0.0` and `8.0.1`

1. In your terminal, navigate to your AllegroGraph installation.
2. `cd` to `/data/rootcatalog/` and then to the repository, in our case `llm-philosophy`
3. `rm *.vdb.*` if you want to delete all existing VDBs associated with that repo, otherwise you can manually `rm` the `llm-philosophy.vdb.vec` and `llm-philosophy.vdb.dat` files.
