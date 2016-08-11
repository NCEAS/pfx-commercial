# Commercial fishing portfolio analyses 

The 4 main projects are in the folders:
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
