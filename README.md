# Commercial fishing portfolio analyses 

The 4 main projects (each representing a different paper, descriptions below) are in the folders:
`diversity`,
`portfolio`,
`evos-ifq`, and
`salmon`.

The folder `data` contains the raw data.

Any data files that are generated throughout analyses 
can be saved in a `data-generated` folder within a given project. 

The folder `pfxr` contains an R package with any reusable functions. It can be loaded from within any of the analysis folders with:

```r
devtools::load_all("pfxr")
```

## Papers and analyses

### Effects of diversification on individual fishers (Anderson et al.)
The main files for this project are in the `portfolio` folder, including code to replicate the modeling (`portfolio/analysis`) - including model files in Stan, text and bibliography for the paper (`portfolio/paper`) and plots (`portfolio/figs`) - including many not included in the main paper.

### Changing benefits of diversification for salmon fishers (Ward et al.)
The main files for this project (`salmon`) are the code to replicate the modeling (`salmon/analysis`) - including model files in Stan, and the text and bibliography for the paper (`salmon/paper`).

### Dynamics and opportunities of diversification in Alaskan fisheries (Beaudreau et al.)

