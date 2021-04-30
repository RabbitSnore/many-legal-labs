# Many Legal Labs

A multi-lab replication of legal psychology studies.

**NOTE: ALL DATA ARE CURRENTLY SIMULATED. NO ACTUAL DATA HAVE BEEN COLLECTED YET. THE CODE WILL BE UPDATED WHEN DATA COLLECTION HAS BEEN COMPLETED.**

## Reproducing the analyses

The most straightforward way to reproduce the analyses is to clone this repository into an R Project and run the following R code:

```
source("MLL_central_executive.R")
```

Running this script will reproduce all the analyses for the project and render the main report and supplementary reports. The script also automatically detects what packages needed for the project must be installed and installs them.

More control can be achieved by changing parameters in `MLL_central_executive.R` and/or by manually executing the scripts.