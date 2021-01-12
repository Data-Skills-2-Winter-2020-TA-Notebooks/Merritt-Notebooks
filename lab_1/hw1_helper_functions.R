library(ggplot2)
greet <- function(greeting, subject){
  return(paste(c(greeting, ', ', subject, '!'), collapse = ''))
}


plot_mtcars <- function(){
  mtcars_plot <- ggplot2::ggplot(data = mtcars) +
    geom_density_2d(aes(x=mpg, y=hp)) +
    ggtitle("Miles per gallon vs. Horsepower in mtcars dataset")+
    xlab('Miles per gallon') +
    ylab('Horsepower')
  return (mtcars_plot)
}
