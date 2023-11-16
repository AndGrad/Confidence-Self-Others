README
================

# How to interact with this repository

This repository was structured to maximize modularity and ease of access
for different use cases. There is some redundancy in the code, but it
should be easier to find specific things that you are looking for.

For any questions, you can reach out to: andrea.gradassi@gmail.com.

General content of each folder:

- data: the full dataset for both experiments, both in .csv and .rda format (data/full_dataset.csv, data/full_dataset.rda). subfolders contain convenience dataset derived from the full_data file, used for plots or other analysis

- scripts: scripts used to analyse the data.

- tables: demographics information, summary of model fitting output.

- model_fits: output files of the regression models. 

### I just want to check the code analyses and the plots in the paper.

You should check the file paper_summary.hmtl. it contains all the code
used to output the content of the main paper, as an .html file. This is
not where you re-run the code yourself, or thinker with it.

### I want to play with the code for analyses and the plots in the paper.

You should check the file scripts/analyses/paper_summary.hmtl. This is a
R Markdown used to generate the report above, so it has the same
content, but now you can play around with the data.

### I want to double check the whole analysis pipeline.

You should go into the folder scripts/analyses

**Nice plots! how do I make them?**

Each figure has its own associated file, they are in the folder
scripts/plots.
