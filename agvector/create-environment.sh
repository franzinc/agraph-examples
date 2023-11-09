#!/bin/bash

conda install -c franzinc agraph-python -y
wait

conda install -c conda-forge pycurl -y
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