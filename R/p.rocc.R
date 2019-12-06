p.rocc <-
function (trocc,newsample) 
{
  
  if(class(trocc)!="trocc")
    stop("Object has to be a trocc object obtained from tr.rocc function")  
  if(is(newsample,"matrix")==FALSE)
    warning("newsample should be a matrix (with genes as rows and samples as columns)")  
  if(is.null(colnames(newsample)))
    warning("Colnames for newsample with sample names are missing")
  
  if(is(newsample,"matrix")==FALSE)
  newsample<-as.matrix(newsample)
  
  if(is.null(rownames(newsample)))
    stop("Rownames for newsample with gene names are missing")
  if(table(trocc$genes%in%rownames(newsample))["TRUE"]!=length(trocc$genes))
    stop("newsample does not contain all genes of classifier trocc object")
  

pr<-as.numeric(rep(NA,length(colnames(newsample))))
pr<-factor(pr,levels=c(0,1))
names(pr)<-colnames(newsample)

for (v in 1:dim(newsample)[2])
{
plusn<-newsample[trocc$positiv,v]
minusn<-newsample[trocc$negativ,v]
minustoplusn<-minusn*-1
newdata<-mean(c(plusn,minustoplusn))
ifelse(newdata>trocc$cutoffvalue,pr[v]<-1,pr[v]<-0)
} 

KRAVAL<- pr
return(KRAVAL)

}

