#!/bin/bash

pip install agraph-python==104.2.0
wait

conda install -c conda-forge langchain -y
wait

conda install -c conda-forge shortuuid -y 
wait

conda install -c conda-forge jupyterlab=3.5.0 -y
wait

conda install -c anaconda nb_conda_kernels -y
wait

conda install -c anaconda pandas -y
wait

conda install tqdm -y
wait
