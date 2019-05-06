# pmc2nc

## Overview
Pmc2nc stands for PubMed Central citation network construction. The purpose of this tool is to generate an edge list to build a citation network from the articles in PubMed Central.

## Problem
Due to the increasing number of articles being published, it is getting harder for researchers to filter the important articles for their research. This tool will quickly generate an edge list of relevant articles. The edge list can be imported into network visualization and analysis tools.

## Example
![alt text](https://github.com/myz09001/pmc2nc/blob/master/pmc2nc_example1.gif)


## Availablity

### Download  pmc2nc
```
# Install the R package from Github using the _devtools_ package:
devtools::install_github(myz09001/pmc2nc')

# generate an edge list for one article
pmid <- 21876761
res1 <- retrieveEdgeList(pmid, conMySQL = con_mysql)
```
