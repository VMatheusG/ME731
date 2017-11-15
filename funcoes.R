## carrega e instala pacotes

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
##calcula coeficiente de correlacao(%)
cv <- function(x){
  100 * sd(x)/mean(x)
}

parametro <- function(n,c){
  y <- character(n)
  for(i in 1:n){
    y[i] <- ifelse(i %% 2 == 1,  paste0("$","\\mu_{",(i+1)/2,"}$"),
         paste0("$","\\alpha_{",c,i/2,"}$") )
  }
return(y)
}

acertar_coluna <- function(x){
  y <- character(length(x))
  for( i  in 1:length(x))
    y[i] <- ifelse(i%%2==0,NA,x[i])

  return(y)
}

