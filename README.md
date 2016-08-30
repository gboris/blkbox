# blkbox http://www.r-pkg.org/badges/version/blkbox  http://cranlogs.r-pkg.org/badges/grand-total/blkbox

##Data Exploration with Multiple Machine Learning Algorithms 

###Introduction

Machine learning (ML) is a powerful tool to create supervised models that can distinguish between classes and facilitate biomarker selection in high-dimensional datasets, including RNA Sequencing (RNA-Seq). However, identifying the best performing ML algorithm(s) for a specific dataset is time consuming. blkbox is a software package including a shiny frontend, that integrates nine ML algorithms to select the best performing classifier for a specific dataset. blkbox accepts a simple abundance matrix as input, includes extensive visualization, and also provides an easy to use feature selection step to enable convenient and rapid potential biomarker selection, all without requiring parameter optimization. 
**Results:** Feature selection makes blkbox computationally inexpensive while multi-functionality, including nested cross-fold validation (NCV), ensures robust results. blkbox identifies algorithms outperforming prior published ML results. Applying NCV identifies features which are utilized to gain high accuracy. 
Availability: The code is available as a CRAN R package and github (https://github.com/gboris/blkbox).


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

**Version**: 1.0.1

**Date**: 2016-08-05

**Author**: Zachary Davies, Boris Guennewig

**Maintainer**: Boris Guennewig <b.guennewig@garvan.org.au>

**Description**: Allows data to be processed by multiple machine learning algorithms at the same time, enables feature selection of data by single a algorithm or combinations of multiple. Easy to use tool for k-fold cross validation and nested cross validation.

**License**: GPL (>= 2)

**LazyData**: TRUE

**NeedsCompilation**: no

**Packaged**: 2016-08-05 22:19:09 UTC; Guennewig

**Built**: R 3.2.5; ; 2016-08-05 00:49:44 UTC; unix



