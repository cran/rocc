\name{tr.rocc}
\Rdversion{1.2}
\alias{tr.rocc}
\title{
Training of a ROC based classifier
}
\description{
The function establishes the ROC based classifier, returning the classifier specifications.  
}
\usage{
tr.rocc(g, out, xgenes = 200)
}
\arguments{
  \item{g}{
  the input data in form of a matrix with genes as rows and samples as columns.
  rownames(g) and colnames (g) must be specified.
}
  \item{out}{
  describes the phenotype of the samples. a factor vector with levels 0 and 1 (in this order) 
  with as many values as there are samples.
}
  \item{xgenes}{
  numeric (vector of length 1), determines the number of features to be selected in feature selection.
}
}
\details{
For feature selection the function picks the given number of xgenes with highest AUC (AUC below 0.5 are mirrored).
Features negatively associated (AUC below 0.5) are multiplied by -1. The selected features are merged by the mean values to form a metagene. 
Samples are ranked according to the metagene expression. The optimal split of positive (i.e., 1) and negative (i.e., 0) samples is determined as the split yielding the highest accuracy, i.e. correct class assignments in respect to the real class.
The split yielding optimal accuracy in the ROC curve is determined using the package ROCR. The metagene threshold is computed as the mean metagene expression value of the two samples that build the boarder of the split. 
The final classifier specifications consist of a) the selected genes b) positive (AUC above 0.5) or negative (AUC below 0.5) association of these genes to the true class, and c) the metagene threshold. 
A new sample can be classified using the o.rocc() function.
}
\value{
a list as a trocc object with components
  \item{AUCs}{a matrix containing the selected features with corresponding AUC (aucv), positiv or negativ association (posneg), and mirrored AUC (allpos).
    }
  \item{genes}{character vector containing the genes selected in the feature selection.
    }
  \item{positiv}{character vector containing all positively associated genes (AUC above 0.5) selected in the feature selection.
    }
  \item{negativ}{character vector containing all negatively associated genes (AUC below 0.5) selected in the feature selection.
    }
  \item{metagene.expression}{numeric vector containing the metagene values of the training samples.
    }
  \item{metagene.expression.ranked}{numeric vector containing the samples ranked by metagene expression values.
    } 
  \item{cutoffvalue}{the metagene threshold obtained from the best split of training samples.
    }
  \item{method}{the classification method used: ROC.based.predictor.
    }  
    
       
}
\references{
Lauss M, Frigyesi A, Ryden T, Hoglund M. Robust assignment of cancer subtypes from expression data using a uni-variate gene expression average as classifier. BMC Cancer 2010 (in print)
}
\author{
Martin Lauss
}
\note{
depends on the package ROCR
}

\seealso{
p.rocc,o.rocc
}
\examples{
### Random Dataset and phenotype
set.seed(100)
## Dataset should be a matrix
g <- matrix(rnorm(1000*25),ncol=25)
rownames(g) <- paste("Gene",1:1000,sep="_")
colnames(g) <- paste("Sample",1:25,sep="_")
## Phenotype should be a factor with levels 0 and 1: 
out <- as.factor(sample(c(0:1),size=25,replace=TRUE))

predictor <- tr.rocc (g,out,xgenes=50)

## find classifier specification:
predictor$positiv
predictor$negativ
predictor$cutoffvalue


}
\keyword{classif}

