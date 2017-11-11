

gen.graf.resid<-function(mY,mresult,var,typeresid,wplot)
{
  mresiduo <- mresult$residuals
  mbeta <- coef(mresult)
  mX <- as.matrix(model.matrix(mresult))
  n <- nrow(mX)
  p <- ncol(mbeta)
  q <- nrow(mbeta)
  mSigma<-t(mY-mX%*%mbeta)%*%(mY-mX%*%mbeta)/(n-q)
  if (typeresid == "univariate")
  {
    auxres <- diag((diag(n) - mX%*%solve(t(mX)%*%mX)%*%t(mX)))
    mresiduo <- mresiduo/(sqrt((matrix(auxres,n,p))%*%diag(diag(mSigma))))
  }
  else if (typeresid == "multivariate")
  {
    mresiduo <- t(solve(t(chol(mSigma)))%*%t(mresiduo))
  }
  mfit <- fitted.values(mresult)
  #
  if (wplot == "diagnostics")
  {
    par(mfrow =c(2,2))
    plot(mresiduo[,var],ylim=c(min(-3,min(mresiduo[,var])),max(3,max(mresiduo[,var]))),xlab="índice",ylab="resíduo studentizado")
    abline(-2,0,lty=2)
    abline(2,0,lty=2)
    abline(0,0,lty=2)
    #
    plot(mfit[,var],mresiduo[,var],ylim=c(min(-3,min(mresiduo[,var])),max(3,max(mresiduo[,var]))),xlab="valor ajustado",ylab="resíduo studentizado")
    abline(-2,0,lty=2)
    abline(2,0,lty=2)
    abline(0,0,lty=2)
    #
    hist(mresiduo[,var],probability=TRUE,xlab="resíduo studentizado",main="",ylab="densidade")
    #
    qqPlot((mresiduo[,var]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",xlab="quantil da N(0,1)",ylab=paste("quantil do resíduo studentizado"),cex=1.2,id.cex=1.2)
  }

  else if (wplot == "envelope")
  {
    par(mfrow =c(1,1))
    qqPlot((mresiduo[,var]),dist="norm",mean=0,sd=1,col.lines=1,grid="FALSE",xlab="quantil da N(0,1)",ylab=paste("quantil do res?duo studentizado"),cex=1.2,id.cex=1.2)
  }
}
