# AllegroGraph Ollama Integration

## Python environment

To use the following notebook we recommend you create a virtual environment.

### conda

```shell
conda env create -f environment.yml
```

and then:

```shell
conda activate ag-examples-llm
```

### pip

Please run the following commands

```shell
python3 -m venv .venv
source .venv/bin/activate
pip install agraph-python jupyterlab
```

## Ollama

`Ollama` is a platform designed for deploying, managing, and interacting with AI language models locally. It focuses on making it easy for users to run large language models (LLMs) on their own hardware, without needing to rely on cloud services or external servers. The platform's primary advantage is allowing users to operate AI models while maintaining control over their data and reducing latency associated with cloud-based processing.

If you use `Ollama`, you can provide information about your `Ollama` setup and set llm query options in the *agraph.cfg* file, or pass it in for each SPARQL query. The notebooks will show how to pass in your connection info as SPARQL Prefix parameters.

Not all models are supported as supported models need to support function calling. The models `llama3.1`, `mistral`, and `qwen2` do provide such support and others models will also work if they support function calling. If you have questions about other models, please contact support@franz.com.

[AllegroGraph Ollama Documentation](https://franz.com/agraph/support/documentation/ollama.html)

### Installing Ollama

To download `Ollama` please visit their [download page](https://ollama.com/)

Once `Ollama` is installed, you can pull models using:

```shell
ollama pull llama3.1:latest
```

Test that your model is working using:

```shell
ollama run llama3.1:latest
```

Please start with [ollama-sparql-integration.ipynb](ollama-sparql-integration.ipynb) to learn how we've integrated Ollama with our SPARQL engine!
