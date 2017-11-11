library(ggplot2)
library(car)
library(MASS)
library(grid)
library(gridBase)

graf_resid <- function(fit=NULL, ajustado = NULL, stud_res = NULL){
if ( !is.null(fit)) {
  ajustado <- fitted(fit)
  stud_res <-  studres(fit)
}
  Indice <- 1:length(stud_res)

p1 <- qplot(x = Indice, y = stud_res ,geom = "point") +
    labs(y = "Residuo Studentizado",x = "Indice") +
    theme_bw()

p2 <- qplot(x = ajustado, y = stud_res ,geom = "point")  +
   labs(y = "Residuo Studentizado",x = "Valores Ajustados")  +
   theme_bw()

p3 <-   ggplot(data.frame(stud_res),aes(stud_res,col = I("black"),fill = I("white"))) +
   geom_histogram(binwidth = 1, aes(y=..density..)) +
   labs( x = "Residuo Studentizado", y = "Densidade") +
   theme_bw()

plot.new()
pushViewport(viewport(layout = grid.layout(2,3 )))

#Draw ggplot 1
pushViewport(viewport(layout.pos.col = 1,layout.pos.row = 1))
print(p1, newpage = FALSE)
popViewport()
#Draw ggplot 2
pushViewport(viewport(layout.pos.col = 2,layout.pos.row = 1))
print(p2, newpage = FALSE)
popViewport()
#Draw ggplot 3
pushViewport(viewport(layout.pos.col = 1,layout.pos.row = 2))
print(p3, newpage = FALSE)
popViewport()

#Draw base plot
pushViewport(viewport(layout.pos.col = 2,layout.pos.row = 2))
par(fig = gridFIG(), new = TRUE)
par(mar = c(2.5,2.6,0.5,0),cex.axis = 0.85,
    cex.lab = .96,las = 1,mgp = c(1.5, .5, 0))
qqPlot(stud_res,ylab = "Residuo Studentizado", xlab = "Quantils Teoricos",
       dist = "norm", mean = 0, sd = 1, pch = 1,cex = 1.2, grid = "FALSE")
popViewport()

}


