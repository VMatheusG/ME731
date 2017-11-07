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
