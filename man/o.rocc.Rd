\name{o.rocc}
\Rdversion{1.1}
\alias{o.rocc}
\title{LOOCV using the ROC based classifier
}
\description{
The function performs classification by leave-one-out-cross-validation (LOOCV) using the ROC based classifier: 
Features are combined to a metagene by the mean expression and samples are ranked according to the metagene expression.
The metagene threshold that yields optimal accuracy in the training samples is then used to classify new samples according to their metagene expression values.
}
\usage{
o.rocc(g, out, xgenes = 200)
}
\arguments{
  \item{g}{the input data in form of a matrix with genes as rows and samples as columns.
  rownames(g) and colnames (g) must be specified.
}
  \item{out}{describes the phenotype of the samples. a factor vector with levels 0 and 1 (in this order) 
  with as many values as there are samples.
}
  \item{xgenes}{number of genes in the classifier. numeric vector with length of at least 1.

}
}
\details{
For feature selection the genes are ranked by AUC values and the top ranking AUC genes according to xgenes are picked (all AUCs below 0.5 are mirrored). 
To obtain AUC values for a given gene signature an arithmetic mean is computed by summing up the expression values after multiplying expression values for genes negatively associated with the feature (AUC below 0.5) by -1. 
The resulting expression values for the thus formed metagenes are then used in ROC analysis. The optimal split of positive (i.e., 1) and negative (i.e., 0) samples is determined as the metagene expression threshold which produces the highest accuracy, correct class assignments in respect to the real class, in the training set. The split yielding optimal accuracy in the ROC curve is determined using the package ROCR.  
The threshold is computed as the mean metagene expression value of the two samples at the boarder of the split. 
A new sample to be classified has its metagene expression value determined with the same genes to be multiplied by -1. The new sample is classified according to which side of the threshold the sample falls in, with a sample having higher metagene expression being classified as positive (i.e., 1) and with lower expression as negative (i.e., 0).
Performance of the classifier is estimated in the dataset by leave-one-out cross validation with feature selection and classifier specification repeated in each loop to ensure that the remaining sample has not seen the classifier.
}
\value{
a list (orocc object) with components
  \item{confusion}{a matrix containing the classifier performance in leave-one-out cross validation for each gene size determined by xgenes. The following measures are returned:
    accuracy, lower and upper 95 percent confidence interval, the largest prior class (accuracy null), the p value from the binomial test that
    the accuracy is different from accuracy null, accuracy obtained by random assignment (using prior distributions), the p value from the binomial test
    that the accuracy is different from random assignment, sensitivity, specificity, positive predictive value, negative predictive value,
    prevalence, contingency table values of predicted versus true class, and the balanced accuracy that is calculated as (sensitivity+specificity)/2.
    }
  \item{concordance}{a matrix that contains the predicted classification obtained by leave-one-out cross validation. Additionally the true classes (out) are shown.
    }
  \item{method}{the classification method used: ROC.based.predictor.
    }

}
\references{
Lauss M, Frigyesi A, Ryden T, Hoglund M. Robust assignment of cancer subtypes from expression data using a ROC-based classifier. (in preparation)
}
\author{
Martin Lauss
}
\note{
depends on the packages ROCR and caret
}
\seealso{
tr.rocc(), p.rocc()
}
\examples{
## Random dataset and phenotype (small dataset for demonstration)
## Dataset should be a matrix
set.seed(100)
g <- matrix(rnorm(1000*25),ncol=25)
rownames(g) <- paste("Gene",1:1000,sep="_")
colnames(g) <- paste("Sample",1:25,sep="_")
## Phenotype should be a factor with levels 0 and 1: 
out <- as.factor(sample(c(0:1),size=25,replace=TRUE))
## Set the size of the Gene Signature
xgenes=c(50,500)

####### o.rocc

results<-o.rocc (g,out,xgenes)
results



####### performance of a given gene set by LOOCV in independent data

## load given genes (or take $genes from tr.rocc output)
genes<-paste("Gene",1:50,sep="_")
## load validation data
set.seed(101)
f <- matrix(rnorm(1000*25),ncol=25)
rownames(f) <- paste("Gene",1:1000,sep="_")
colnames(f) <- paste("Sample",1:25,sep="_")
outf <- as.factor(sample(c(0:1),size=25,replace=TRUE))
## reduce validation set to gene signature genes
f<-f[genes,]
## use all genes of reduced dataset for LOOCV
xgenes<-length(genes)

resultval<-o.rocc (f,outf,xgenes)
resultval


######### o.rocc results can be redone as a LOOCV with tr.rocc und p.rocc functions

results$concordance[,"50"]

## now with a LOOCV loop of tr.rocc and p.rocc
pr<-as.numeric(rep(NA,length(colnames(g))))
pr<-factor(pr,level=c(0,1))

for (i in 1:length(colnames(g))){
e<-g[,-i]
oute<-out[-i]
train<-tr.rocc(e,oute,xgenes=50)
procc<-p.rocc(train,g[,i]) ## ignore warnings, they dont apply here
pr[i]<-procc
}

all.equal(results$concordance[,"50"],pr)
# TRUE


}
\keyword{classif}
