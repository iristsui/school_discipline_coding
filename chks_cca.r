
rcc <- function (X, Y, lambda1, lambda2) {
    Xnames <- dimnames(X)[[2]]
    Ynames <- dimnames(Y)[[2]]
    ind.names <- dimnames(X)[[1]]
    Cxx <- var(X, na.rm = TRUE, use = "pairwise") + diag(lambda1, 
        ncol(X))
    Cyy <- var(Y, na.rm = TRUE, use = "pairwise") + diag(lambda2, 
        ncol(Y))
    Cxy <- cov(X, Y, use = "pairwise")
    Cxy <- nearPD(Cxy, corr=FALSE, keepDiag=TRUE, maxit=200)
    Cxy <- as.matrix(Cxy$mat)
    res <- geigen(Cxy, Cxx, Cyy)
    names(res) <- c("cor", "xcoef", "ycoef")
    scores <- comput(X, Y, res)
    return(list(cor = res$cor, names = list(Xnames = Xnames, 
        Ynames = Ynames, ind.names = ind.names), xcoef = res$xcoef, 
        ycoef = res$ycoef, scores = scores))
}




write.csv(wested.lausd.x, "/Users/HumanImpactPartners/Desktop/wested_lausd.csv")




ev<-cc1$cor^2
ev2<-1-ev

n<-dim(a)[1]
p<-length(a)
q<-length(b)
m<-n -3/2 - (p+q)/2
w<-cbind(NULL)  # initialize wilks lambda

for (i in 1:27){
    w<-cbind(w,prod(ev2[i:27]))
  }

d1<-cbind(NULL)
d2<-cbind(NULL)
f<-cbind(NULL)  # initialize f
for (i in 1:27){
    s<-sqrt((p^2*q^2-4)/(p^2+q^2-5))
    si<-1/s
    df1<-p*q
    d1<-cbind(d1,df1)
    df2<-m*s-p*q/2+1
    d2<-cbind(d2,df2)
    r<-(1-w[i]^si)/w[i]^si
    f<-cbind(f,r*df2/df1)
    p<-p-1
    q<-q-1
}

pv<-pf(f,d1,d2,lower.tail=FALSE)
dmat<-cbind(t(w),t(f),t(d1),t(d2),t(pv))
colnames(dmat)<-c("WilksL","F","df1","df2","p")
rownames(dmat)<-c(seq(1:length(w)))

dmat

