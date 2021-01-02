.PHONY: init manual_init remove rstudio export

CONDA_BASE=$(shell conda info --base)

init:
	conda env create -f environment.yml

manual_init:
	conda create -y --no-default-packages -n clifford-attractor -c r r-base=3.6.3
	source $(CONDA_BASE)/etc/profile.d/conda.sh && \
	conda activate clifford-attractor && \
	conda install -y -c conda-forge r-tidyverse && \
	conda install -y -c conda-forge r-purrrlyr && \
	conda install -y -c r rstudio

remove:
	conda remove -n clifford-attractor --all

rstudio:
	source $(CONDA_BASE)/etc/profile.d/conda.sh && \
	conda activate clifford-attractor && \
	rstudio

export:
	source $(CONDA_BASE)/etc/profile.d/conda.sh && \
	conda activate clifford-attractor && \
	conda env export --no-builds | grep -v ^prefix > environment.yml
