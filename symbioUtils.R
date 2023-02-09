
#' Not %in% Function
#'
#' This function is equivalent to `not in` function
#' This can be used to check absense of variable in a vector.
#' @export
#' @example 'a' %!in% c('b', 'c', 'd')
#' @return Logical output (TRUE or FALSE)
`%!in%` = Negate(`%in%`)

#' Standard error with NA removal
#'
#' Base R do not have standard error with NA removal.
#' This function takes in a vector and calculate relative growth difference
#' @export
#' @param x is a vector containing integer
#' @return Standard error calculated after removing NAs
#' @example se(c(1, 4, 2, 5, 3, NA, 1, 6))
se <- function(x) sd(x, na.rm=T)/sqrt(length(x))

#' Regus 2015 equation for scaled relative growth difference
#'
#' This function takes in sample biomass and control biomass.
#' Then estimate the difference difference and scale by control biomass.
#' @export
#' @param x.rg is sample biomass
#' @param c.rg is control biomass
#' @return Scaled growth difference or scaled relative growth
#' @example calculate.RGD(c(5, 3, 4), 4)
calculate.RGD <-  function(x.rg, c.rg){
  return (((x.rg - c.rg) / c.rg ) * 100)
}

#' Wendlandt 2019 for RG ratio
#'
#' This function takes in sample biomass and control biomass
#' It returns the ratio of sample and control biomass
#' @export
#' @param x.rg is sample biomass
#' @param c.rg is control biomass
#' @return Sample to control biomass proportion
#' @example calculate.RG(c(5,3,4), 4)
calculate.RG <-  function(x.rg, c.rg){
  return (x.rg / c.rg)
}

#' Canonical Function For Generating Barplot with SE and datapoints
#'
#' For data analysis in symbiosis experiment, we often need to break the dataset in
#' diifferent variables to visualize it. This function gives a handy way to break down
#' the dataset in one or two factors and visualize the response variable. It aggregate
#' the dataset by given variables, calculate mean and standard error, and shows a handy
#' barplot with standard error and individual data points.
#'
#' @export
#' @param dataset dataframe containing master harvest dataset
#' @param response response variable for y axis from the dataset
#' @param factor1 independant variable for y axis from the dataset
#' @param factor another independant variable from the dataset.
#' @return a barplot showing response variable in y and independant variables in the x axis
#' @example symbio.plot(harvest, 'relativeGrowth', 'host')
#' @example symbio.plot(harvest, 'relativeGrowth', 'host', 'treatment')
symbio.plot <- function(dataset, response, factor1, factor2=NULL){
  # Defining necessary function inside of this R function
  se <- function(x) sd(x, na.rm=T)/sqrt(length(x))

  # Generation of aggregated mean and se dataset and merge them
  if (is.null(factor2)){
    temp.dataset <- dataset[, c(response, factor1)]
    colnames(temp.dataset) <- c('y', 'x1')
    agg.mean <- aggregate(y~x1, data=temp.dataset, mean, na.rm=T)
    agg.se <- aggregate(y~x1, data=temp.dataset, se)
    colnames(agg.se) [2] <- 'se'
    agg <- merge(agg.mean, agg.se, by='x1')
    # Plotting the aggregated dataset
    ggplot2::ggplot(agg, aes(x=x1, y=y, fill=x1)) +
      geom_bar(stat='summary',position='dodge', fun.y='mean', na.rm=T) +
      geom_errorbar(aes( ymin=y-se, ymax=y+se), position = position_dodge(width = 0.90), width=0.2) +
      geom_point(data=temp.dataset, aes(x=x1, color=x2), colour='black', pch=21, position=position_dodge(width=0.9)) +
      #ggtitle('Median Nodules by Nod+ Treatments') +
      xlab(factor1) + ylab(response) +
      facet_grid(~x1, space='free_x', scales='free_x')


  } else {
    temp.dataset <- dataset[, c(response, factor1, factor2)]
    colnames(temp.dataset) <- c('y', 'x1', 'x2')
    agg.mean <- aggregate(y~x1+x2, data=temp.dataset, mean, na.rm=T)
    agg.se <- aggregate(y~x1+x2, data=temp.dataset, se)
    colnames(agg.se) [3] <- 'se'
    agg <- merge(agg.mean, agg.se, by=c('x1', 'x2'))

    # Plotting the aggregated dataset
    ggplot2::ggplot(agg, aes(x=x1, y=y, fill=x2)) +
      geom_bar(stat='summary',position='dodge', fun.y='mean', na.rm=T) +
      geom_errorbar(aes( ymin=y-se, ymax=y+se), position = position_dodge(width = 0.90), width=0.2) +
      geom_point(data=temp.dataset, aes(x=x1, color=x2), colour='black', pch=21, position=position_dodge(width=0.9)) +
      #ggtitle('Median Nodules by Nod+ Treatments') +
      xlab(factor1) + ylab(response) +
      facet_grid(~x1, space='free_x', scales='free_x')
  }

}


#' Function to generate diagnostic plot of lm model and summary
#'
#' This function takes in a linear model (lm) object and visualize the diagnostics in pretty ggplot
#' @export
#' @param model.lm lm object
#' @return a plot showing four diagnostics and model summary
#' @example model.diagnostics(lm(relativeGrowth~host+treatment, data=harvest))
model.diagnostics <-  function(model.lm) {
  requires(ggplot2)
  diagnostics <- ggplot::autoplot(model.lm) + theme_light()
  gridExtra::grid.arrange(grobs=diagnostics@plots, top=format(model.lm$call$formula))
  summary(model.lm)
}


#' Interaction plots between two variables on response
#'
#' This funciton takes in a dataset, columns of the dataset containging response variable and two independant variables
#' @export
#' @param dataset a dataframe containing harvest data
#' @param response response variable, represented by a column of the dataset
#' @param x_group1 independant variable, represented by a column of the dataset
#' @param group2 independant variable, represented by a column of dataset
#' @return interaction plot where y is response variable, x-axis is mean and standard error of first independant variable,
#' and colored line conneecting the means of representing second variable accross first independant variable
#' @example interaction.plot(harvesstData, 'relativeGrowth', 'Species', 'Treatment')
interaction.plot <- function(dataset, response, x_group1, group2){
  temp.dataset <- dataset[, c(response, x_group1, group2)]
  colnames(temp.dataset) <- c('y', 'x1', 'x2')

  ggplot2::ggplot(temp.dataset, aes(x=factor(x1), y=y, group=x2, color=x2)) +
    stat_summary(fun.y=mean,
                 fun.ymin = function(x) mean(x) - sd(x),
                 fun.ymax = function(x) mean(x) + sd(x),
                 geom='linerange', position=position_dodge(width=0.3)) +
    stat_summary(fun.y=mean, geom='line', size=2) +
    ggtitle('Interaction Plot') + xlab(as.character(x_group1)) + ylab(as.character(response)) +
    labs(color=as.character(group2))
}
