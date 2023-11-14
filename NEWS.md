# Version: 0.99.0
* Date: 2023-04-03
* Details: This version is equal to the version 0.2.3 on GitHub 
(https://github.com/EvaYiwenWang/PLSDAbatch-source-code)

# Version: 0.99.1
* Date: 2023-05-26
* Text: Update Imports
* Details: Move `pheatmap`, `vegan`, `Biobase`, `BiocStyle` from Suggests to 
Imports to ensure the generation of vignette

# Version: 0.99.2
* Date: 2023-11-14
* Text: Updates for Bioconductor
* Details: Update several documents
  * DESCRIPTION: Add a URL field pointing to the GitHub repository 
  * vignettes: Wrap as many texts as possible to a character limit of 80; 
                Use `library()` calls instead of `sapply()`;
                Use `TreeSummarizedExperiment` package to reshape datasets;
                Remove `colorize()`
  * R: Update `for()` loops; Remove extra `###` lines
