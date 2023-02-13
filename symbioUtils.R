
# Not %in% Function
#
# This function is equivalent to `not in` function
# This can be used to check absense of variable in a vector.
`%!in%` = Negate(`%in%`)

# Standard error with NA removal
#
# Base R do not have standard error with NA removal.
# This function takes in a vector and calculate relative growth difference
se <- function(x) sd(x, na.rm=T)/sqrt(length(x))

# Regus 2015 equation for scaled relative growth difference
#
# This function takes in sample biomass and control biomass.
# Then estimate the difference difference and scale by control biomass.
calculate.RGD <-  function(x.rg, c.rg){
  return (((x.rg - c.rg) / c.rg ) * 100)
}

# Wendlandt 2019 for RG ratio
#
# This function takes in sample biomass and control biomass
# It returns the ratio of sample and control biomass
calculate.RG <-  function(x.rg, c.rg){
  return (x.rg / c.rg)
}

# Canonical Function For Generating Barplot with SE and datapoints
#
# For data analysis in symbiosis experiment, we often need to break the dataset in
# diifferent variables to visualize it. This function gives a handy way to break down
# the dataset in one or two factors and visualize the response variable. It aggregate
# the dataset by given variables, calculate mean and standard error, and shows a handy
# barplot with standard error and individual data points.
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


# Function to generate diagnostic plot of lm model and summary
#
# This function takes in a linear model (lm) object and visualize the diagnostics in pretty ggplot
model.diagnostics <-  function(model.lm) {
  requires(ggplot2)
  diagnostics <- ggplot::autoplot(model.lm) + theme_light()
  gridExtra::grid.arrange(grobs=diagnostics@plots, top=format(model.lm$call$formula))
  summary(model.lm)
}


# Interaction plots between two variables on response
#
# This funciton takes in a dataset, columns of the dataset containging response variable and two independant variables
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



# Generic function to automate plot with fill and facet
#
# Given a dataset, this will order the response variable (y axis value), order the treatments (x axis factor), fill the bars with color according to given variable, and facet into subplots  
response.plot <- function(dataset, y, x, fill=NULL, facet=NULL){
  # Prepare and subset the dataset for data aggregation
  if (is.null(fill) != T & is.null(facet) == T){
    temp.dataset <-dataset[, c(y, x, fill)]
    colnames(temp.dataset) <- c("y", "x1", 'f')
  } else if (is.null(fill) != T & is.null(facet) != T) {
    temp.dataset <- dataset[, c(y, x, fill, facet)]
    colnames(temp.dataset) <- c("y", "x1", 'f', 'sub')
  } else {
    temp.dataset <- dataset[, c(y, x)]
    colnames(temp.dataset) <- c("y", "x1")
  }
  
  # Aggregation by treatment
  if (is.null(fill) != T & is.null(facet) == T){
    agg.o <- aggregate(y ~ x1+f, data=temp.dataset, mean)
  } else if (is.null(fill) != T & is.null(facet) != T) {
    agg.o <- aggregate(y ~ x1+f+sub, data=temp.dataset, mean)
  } else {
    agg.o <- aggregate(y ~ x1, data = temp.dataset, mean, na.rm = T)
  }
  
  agg.se <- aggregate(y ~ x1, data = temp.dataset, se)
  colnames(agg.se)[2] <- "se"
  agg <- merge(agg.o, agg.se, by = "x1")
  
  
  # Generate plot
  if (is.null(fill) != T & is.null(facet) == T){
    ggplot2::ggplot(agg, aes(x=reorder(x1, -y), y=y, fill=f)) +
      geom_bar(stat = "identity", position = "dodge", na.rm = T) + 
      geom_errorbar(aes(ymin = y - se, ymax = y + se), position = position_dodge(width = 0.9), width = 0.2) +
      xlab(x) + ylab(y) + theme(legend.title = element_blank(), axis.text.x = element_text(angle=90))
    
  } else if (is.null(fill) != T & is.null(facet) != T) {
    ggplot2::ggplot(agg, aes(x=reorder(x1, -y), y=y, fill=f)) +
      geom_bar(stat = "identity", position = "dodge", na.rm = T) + 
      geom_errorbar(aes(ymin = y - se, ymax = y + se), position = position_dodge(width = 0.9), width = 0.2) +
      xlab(x) + ylab(y) + theme(legend.title = element_blank(), axis.text.x = element_text(angle=90)) +
      facet_grid(~sub, scales='free')
    
  } else {
    ggplot2::ggplot(agg, aes(x=reorder(x1, -y), y=y)) +
      geom_bar(stat = "identity", position = "dodge", na.rm = T) + 
      geom_errorbar(aes(ymin = y - se, ymax = y + se), position = position_dodge(width = 0.9), width = 0.2) +
      xlab(x) + ylab(y) + theme(legend.title = element_blank(), axis.text.x = element_text(angle=90))
  }
  
}
