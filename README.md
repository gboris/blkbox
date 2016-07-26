# blkbox

##Data Exploration with Multiple Machine Learning Algorithms 

###Introduction

Transcript expression is the most informative high throughput modality for predicting clinical phenotype (Ray et al., 2014). Transcriptomic data is high dimensional, with very large numbers of transcripts (features or p > 10,000), but a comparatively small number of biological replicates (n = 2 to 1000). Although transcriptional data can be highly predictive of biological state, extracting robust and informative insights from such complexity is a longstanding challenge in data analysis (Capobianco, 2014).
Machine Learning (ML) algorithms are statistical methods that leverage this data complexity through ‘learning’ from data (James et al., 2013).  By identifying highly predictive features, ML can identify transcripts useful to robustly diagnose patients or identify pre-symptomatic individuals {Iizuka:2003gs}. Unfortunately, ML methods are computationally demanding currently underutilized (Libbrecht and Noble, 2015), to some extend due to the >199k  (gencode v24) known human transcripts.
The computational requirements of ML methods can be reduced by restricting analysis to only the most informative transcripts, with little loss of accuracy (Fig. 1C) (Xing et al., 2001; Grate, 2005).  However, the process of selecting informative transcripts, termed feature selection, is complex: no single best method exists, and it is known that optimal input features can differ between ML algorithms (Fig 1D).  This diversity explains the varying performance of ML algorithms on different input data. In machine learning the biggest challenge is selecting the best approach {James:2013gz}, which highlights the necessity to test multiple algorithms to obtain the most accurate model for a specific dataset. This need to explore multiple algorithms can be time-consuming and computationally challenging, and hinders the application of ML approaches (Ray et al., 2014).
Here we describe blkbox, a tool to simplify the exploration of ML algorithm.  blkbox is simple to use (including a graphical shiny interface), provides comparative output over nine relevant ML algorithms, provides enhanced efficiency in potential biomarker selection through scored feature lists, reduces computational cost through optional feature selection, uses accepted standards in the ML field, is under constant development and provides publication ready graphical results to the user.  With a consistent interface, blkbox provides multiple high-performance ML algorithms to users without expertise in the ML field, streamlining access to these methods for researchers in the transcriptomics community.
 
2	Workflow
blkbox implements nine representative high-performance ML algorithms (Fig. 1A): Random Forests (rf) (Breiman, 2001; Lim et al., 2013; Liaw and Wiener, 2008), Support Vector Machines (SVM) (Cortes and Vapnik, 1995; Dimitriadou et al., 2008), neural networks (nnet) (Ripley, 1996), k-Nearest Neighbors (kNN) (Hechenbichler and Schliep, 2004), shrunken centroid (pamR) (Tibshirani et al., 2002), conditional inference trees (party) (Hothorn et al., 2012), Bayesian additive regression trees (bart) (Chipman et al., 2010), extreme gradient boosting (xgb) {Chen:2016ga} and lasso or elastic-net (glm) (Friedman et al., 2010). blkbox contains functions for a range of ML workflows including standard training and testing (blkbox), cross fold validation (blkboxCV), and nested cross fold validation (blkboxNCV) (Fig1B).
blkbox provides the functionality to plot different metrics (AUC, ACC, MCC, F-1, and ERR), allowing comparison of algorithm effectiveness for a given dataset (Supp. Fig. 2 & 3). The intersection of selected features in each testing holdout can be visualized in a Venn diagram or a heatmap to identify commonly used features and assess variance among holdouts (Fig. 1D). Importance of each feature can be found for each fold, tables present the values weighted by their performance on holdouts, enabling feature exploration and additional calculations of feature stability across holdouts (Vignette: blkbox output). 


###Paper abstract

Machine learning (ML) is a powerful tool to create supervised models that can distinguish between classes and facilitate biomarker selection in high-dimensional datasets including RNA Sequencing (RNA-Seq). However, identifying the best performing ML algorithm(s) for a specific dataset is time consuming. blkbox is a software package including a shiny frontend, that integrates eight ML algorithms to select the most accurate classifier for a (specific) dataset. blkbox also provides an easy to use feature selection step to enable convenient and rapid potential biomarker selection and extensive visualization, without requiring parameter optimization, based on a simple count matrix. 

Results: Feature selection makes blkbox computationally inexpensive while multi-functionality, including nested cross-fold validation (NCV), ensures robust results. blkbox identifies algorithms outperforming prior published ML results. Applying NCV identifies features, which are utilized to gain high accuracy. 

###Acknowledgements

This work was supported in part by grants from the NHMRC (APP1067350), Michael J. Fox Foundation for Rapid Response (MJFF22), and a Geoff and Dawn Dixon fellowship to B. G. . We gratefully acknowledge the Swiss National Science Foundation for the funding of B. G. (P2EZP3_152143). We are thankful to Peter Holberton for a code review and to Eugene Dubossarsky, Tim Peters and Max Skipper for helpful discussions.


###Installation

R-package blkbox can be installed:

    library(devtools)
    install_github("gboris/blkbox")

After installation, the package can be loaded into R.

    library(blkbox)

Details of how to use this package, please see the vignette.

**Package**: blkbox

**Type**: Package

**Title**: Data exploration with multiple machine learning algorithms

**Version**: 1.0

**Date**: 2016-05-23

**Author**: Zachary Davies, Boris Guennewig

**Maintainer**: Boris Guennewig <b.guennewig@garvan.org.au>

**Description**: Allows data to be processed by multiple machine learning algorithms at the same time, enables feature selection of data by single a algorithm or combinations of multiple. Easy to use tool for k-fold cross validation and nested cross validation.

**License**: GPL (>= 2)

**LazyData**: TRUE

**NeedsCompilation**: no

**Packaged**: 2016-04-27 22:19:09 UTC; Davies

**Built**: R 3.2.5; ; 2016-04-29 00:49:44 UTC; unix



