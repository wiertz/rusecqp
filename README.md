# rusecqp: An R package for performing corpus linguistic analysis on Corpus Workbench corpora

## Description
The Open Source Project [IMS Corpus Workbench](http://cwb.sourceforge.net) (CWB, Evert & Hardie, 2011) provides a data model and corpus query processor (CQP) for linguistic analysis. The existing [rcqp-Package](https://cran.r-project.org/package=rcqp) (Desgraupes & Loiseau 2018) enables users to perform queries on Corpus Workbench corpora in R and access token level information, but does not provide high level functionality for corpus linguistic analysis. *rusecqp* builds on top of rcqp to provide functions, such as frequency distribution, ngrams, or keyword and collocation analysis.

## Installation
### Prerequisites
The package requires a working installation of the [IMS Corpus Workbench](http://cwb.sourceforge.net) on the system and [rcqp](https://cran.r-project.org/package=rcqp). For installation instructions of both dependencies see the respective websites. Make sure to configure the path to the corpus workbench registry directory by `Sys.setenv(CORPUS_REGISTRY = path_to_registry_dir)` before loading rcqp. To check whether all dependencies are met and corpora are available, you can type in `rcqp::cqi_list_corpora()`. This should print a list of corpora available on the system.

### Installation
To install rusecqp, install the devtools package and than rcqp. This will also install the dependency packages data.table and stringr as required:
````
install.packages("devtools")
devtools::install_github("retide/rusecqp")
`````
Load the package with
`````
library("rusecqp")
`````

### Overview on functions
See the help of the respective functions for explanations of their usage. Currently implemented functions are:
#### Accessing and subsetting corpora
* `list_corpora` lists corpora available on the system.
* `get_corpus` initializes a corpus for further processing.
* `subset_corpus` creates a corpus subset based on corpus meta information (CWB structural attribute values).
#### Corpus based analysis
* `frequency_list` calculates a frequency list of tokens for a corpus or corpus subset.
* `ngrams` calculates a frequency list of ngrams for a corpus or subcorpus. Note that ngram-calculation is very memory intensive and may cause the computer to hang if performed on large corpora (e.g. above 100 Mio. tokens).
* `keywords` performs keyword analysis based on two frequency lists (e.g. as retrieved from `frequency_list`).
#### Query processing
* `query_corpus` can be used to query a corpus using the [CQP syntax](http://cwb.sourceforge.net/temp/CQPTutorial.pdf). The result can be handed to the following functions for analysis:
* `q_frequency_breakdown` returns a frequency table of query matches.
* `q_distribution` shows number of matches in different categories defined by a structural attribute.
* `q_collocations` analyzes collocations based on a window (context) around the query match.

## References
* Desgraupes, B. and S. Loiseau (2016): rcqp: Interface to the Corpus Query Protocol. – Available online at: https://cran.r-project.org/package=rcqp. – accessed online 22/1/2018
* Evert, S., and A. Hardie 2011. Twenty-First Century Corpus Workbench: Updating a Query Architecture for the New Millennium. – Proceedings of the Corpus Linguistics 2011 Conference. – University of Birmingham, UK
