# function for multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL){
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# function to read certain lines of the code
sourcePartial <- function(fn,startTag='#from here',endTag='#to here') {
  lines <- scan(fn, what=character(), sep="\n", quiet=TRUE)
  st<-grep(startTag,lines)
  en<-grep(endTag,lines)
  tc <- textConnection(lines[(st+1):(en-1)])
  source(tc)
  close(tc)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# function for good plot with blue plots
plot_ag = function(...){
  # Input: x,y, .... 
  #          where input are parameters for the plot and ... implies you can pass multiple parameter for the plot fuction
  #output: return the plot of the input with defined color and plotting style
  
  plot(col = "dodgerblue", pch = 20, cex =1.5,...)
}

# calculating rsq values
rsq.from.data = function(data.actual,data.predicted,...){
  #input data.actual = real values from the dataset
  #      data.predicted = predicted value from the model
  #output: rsq.from.data = R^2 value from the two input parameters
  
  iii=0
  SSR = 0
  SSTO = 0
  for (iii in 1:length(data.actual)){
    SSR = SSR + (data.actual[iii]-data.predicted[iii])^2
    SSTO = SSTO + (data.actual[iii]-mean(data.actual))^2
  }
  
  rsq.from.data = 1-(SSR/SSTO)
  
}

# theme for plotting the ggplot2

theme.ggplot2 = function(){
  theme(plot.title = element_text(hjust = 0.5,size = 10, face = "bold"),
        axis.title.x = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 10, face = "bold"))
  
}

# calculaitng AUC
auc.calc = function(probability.of.class,actual.class,...){
  # input : probability.of.class = probability of class/value that classification is based on i.e. output of program
  #       : actual.class = actual class of the values
  #
  # output : auc = Area under then curve for the ROC Curve
  
  ## Making ROC and calcuaitng AUC 
  pred = prediction(probability.of.class,actual.class)
  roc =  performance(pred,"tpr","fpr")
  auc = performance(pred,"auc")
  auc = round(unlist(slot(auc,"y.values")),4)
  
}

#plotting the ROC curve and calcualting the AUC value
plot.roc = function(probability.of.class,class.of.var,title.curve,...){
  # input : probability.of.class = values use to sperate the class
  #       : class.of.var = Class of the variable after seperating the class
  #       : title.curve = title for curve
  # output : plot of ROC curve with auc value
  
  pred = prediction(probability.of.class,class.of.var) # using prediction function from ROCR package
  roc =  performance(pred,"tpr","fpr") # getting ROC values based on pred
  
  plot(roc, colorize = T,main = title.curve,...) # plot function from ROCR package
  abline(a=0,b=1)
  auc = performance(pred,"auc") # calculting AUC for the ROC curve
  auc = round(unlist(slot(auc,"y.values")),2) # extracing the AUC value 
  legend(0.6,0.2,auc,title = "AUC") # plotting the legend to the figure
}

# plotting mean perofmance of the model for classification problems
plot.mean.performance = function(fpr.performance,tpr.performance,mean.performance,auc,
                                 fpr.performance.train,tpr.performance.train,mean.performance.train,auc.train,scheme){
  
  # input : fpr.performance : matrix of false postive ration for each simulation and each fold
  #       : tpr.performance : matrix of true postive ratio for each simulation and each fold
  #       : mean.performance = matrix of performane for each simulation and each fold
  #       : auc = auc for each simulation and each fold
  # output: plot of correct prediction rate and auc for different simulations
  
  fpr.performance = stack(as.data.frame(fpr.performance)) # changing the matrix to dataframe and stacking the columns
  
  p1 = ggplot(data = fpr.performance,aes(x = as.factor(ind), y = values))+
    geom_boxplot(notch = T, outlier.color = "red")+
    geom_point(col= "blue", size = 2, alpha = 0.2)+
    xlab("Folds")+ylab("False Positive Rate")+
    ggtitle(paste0("False Positive Rate of Test Set for  ",scheme))+
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.text.x = element_text(size=10,face="bold"),
          axis.text.y = element_text(size=10,face="bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold")) # plot of each fold and their correponding distribution of mean performance
  
  
  tpr.performance = stack(as.data.frame(tpr.performance)) # changing the matrix to dataframe and stacking the columns
  
  p2 = ggplot(data = tpr.performance,aes(x = as.factor(ind), y = values))+
    geom_boxplot(notch = T, outlier.color = "red")+
    geom_point(col= "blue", size = 2, alpha = 0.2)+
    xlab("Folds")+ylab("True Positive Rate")+
    ggtitle(paste0("True Positive Rate of Test Set for  ",scheme))+
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.text.x = element_text(size=10,face="bold"),
          axis.text.y = element_text(size=10,face="bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold")) # plot of each fold and their correponding distribution of mean performance
  
  
  mean.performance = stack(as.data.frame(mean.performance)) # changing the matrix to dataframe and stacking the columns
  
  p3 = ggplot(data = mean.performance,aes(x = as.factor(ind), y = values))+
    geom_boxplot(notch = T, outlier.color = "red")+
    geom_point(col= "blue", size = 2, alpha = 0.2)+
    xlab("Folds")+ylab("Correct Prediction Rate")+
    ggtitle(paste0("Performance of Test Set for  ",scheme))+
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.text.x = element_text(size=10,face="bold"),
          axis.text.y = element_text(size=10,face="bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold")) # plot of each fold and their correponding distribution of mean performance
  
  
  auc = stack(as.data.frame(auc)) # changing the matrix to dataframe and stacking the columns
  
  p4 = ggplot(data = auc,aes(x = as.factor(ind), y = values))+
    geom_boxplot(notch = T, outlier.color = "red")+
    geom_point(col= "blue", size = 2, alpha = 0.2)+
    xlab("Folds")+ylab("AUC")+
    ggtitle(paste0("AUC of Test Set for  ", scheme))+
    theme(plot.title = element_text(hjust = 0.5,size = 12, face = "bold"),
          axis.text.x = element_text(size=10,face="bold"),
          axis.text.y = element_text(size=10,face="bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold")) # plot of each fold and their correponding distribution of mean performance
  
  # For training set
  
  fpr.performance.train = stack(as.data.frame(fpr.performance.train)) # changing the matrix to dataframe and stacking the columns
  
  p5 = ggplot(data = fpr.performance.train,aes(x = as.factor(ind), y = values))+
    geom_boxplot(notch = T, outlier.color = "red")+
    geom_point(col= "blue", size = 2, alpha = 0.2)+
    xlab("Folds")+ylab("False Positive Rate")+
    ggtitle(paste0("False Positive Rate of Train Set for  ",scheme))+
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.text.x = element_text(size=10,face="bold"),
          axis.text.y = element_text(size=10,face="bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold")) # plot of each fold and their correponding distribution of mean performance
  
  
  tpr.performance.train = stack(as.data.frame(tpr.performance.train)) # changing the matrix to dataframe and stacking the columns
  
  p6 = ggplot(data = tpr.performance.train,aes(x = as.factor(ind), y = values))+
    geom_boxplot(notch = T, outlier.color = "red")+
    geom_point(col= "blue", size = 2, alpha = 0.2)+
    xlab("Folds")+ylab("True Positive Rate")+
    ggtitle(paste0("True Positive Rate of Train Set for  ",scheme))+
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.text.x = element_text(size=10,face="bold"),
          axis.text.y = element_text(size=10,face="bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold")) # plot of each fold and their correponding distribution of mean performance
  
  
  mean.performance.train = stack(as.data.frame(mean.performance.train)) # changing the matrix to dataframe and stacking the columns
  
  p7 = ggplot(data = mean.performance.train,aes(x = as.factor(ind), y = values))+
    geom_boxplot(notch = T, outlier.color = "red")+
    geom_point(col= "blue", size = 2, alpha = 0.2)+
    xlab("Folds")+ylab("Correct Prediction Rate")+
    ggtitle(paste0("Performance of Train Set for  ",scheme))+
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.text.x = element_text(size=10,face="bold"),
          axis.text.y = element_text(size=10,face="bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold")) # plot of each fold and their correponding distribution of mean performance
  
  
  auc.train = stack(as.data.frame(auc.train)) # changing the matrix to dataframe and stacking the columns
  
  p8 = ggplot(data = auc.train,aes(x = as.factor(ind), y = values))+
    geom_boxplot(notch = T, outlier.color = "red")+
    geom_point(col= "blue", size = 2, alpha = 0.2)+
    xlab("Folds")+ylab("AUC")+
    ggtitle(paste0("AUC of Test Set for  ", scheme))+
    theme(plot.title = element_text(hjust = 0.5,size = 12, face = "bold"),
          axis.text.x = element_text(size=10,face="bold"),
          axis.text.y = element_text(size=10,face="bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold")) # plot of each fold and their correponding distribution of mean performance
  
  
  
  
  # plotting all the data together 
  fpr.performance$ind = "FPR Test"
  tpr.performance$ind = "TPR Test"
  mean.performance$ind = "Correct \n Test"  # combining all the values of each column to one
  auc$ind = "AUC Test"
  fpr.performance.train$ind = "FPR Train"
  tpr.performance.train$ind = "TPR Train"
  mean.performance.train$ind = "Correct \n Train"  # combining all the values of each column to one
  auc.train$ind = "AUC Train"
  
  
  
  mean.performance =  rbind(fpr.performance,tpr.performance,mean.performance,auc,
                            fpr.performance.train,tpr.performance.train,mean.performance.train,auc.train)
  mean.error = mean(mean.performance$values) # mean of the whole data mean performance
  
  
  p9 = ggplot(data = mean.performance,aes(x = as.factor(ind), y = values))+
    geom_boxplot(notch = T, outlier.color = "red")+
    geom_point(col= "blue", size = 2, alpha = 0.05)+
    xlab(" ")+ylab(" ")+
    ggtitle(paste0("Performance of ", scheme))+
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.text.x = element_text(size=10,face="bold"),
          axis.text.y = element_text(size=10,face="bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold"))+
    guides(fill = "black")+
    stat_summary(fun.y="mean", geom="point", shape="x", size=8, fill="Green", col ="Orange")
  
  return(list(p1,p2,p3,p4,p5,p6,p7,p8,p9))
  
}


plot.mean.performance.old = function(fpr.performance,tpr.performance,mean.performance,auc, scheme){
  
  # input : fpr.performance : matrix of false postive ration for each simulation and each fold
  #       : tpr.performance : matrix of true postive ratio for each simulation and each fold
  #       : mean.performance = matrix of performane for each simulation and each fold
  #       : auc = auc for each simulation and each fold
  # output: plot of correct prediction rate and auc for different simulations
  
  fpr.performance = stack(as.data.frame(fpr.performance)) # changing the matrix to dataframe and stacking the columns
  
  p1 = ggplot(data = fpr.performance,aes(x = as.factor(ind), y = values))+
    geom_boxplot(notch = T, outlier.color = "red")+
    geom_point(col= "blue", size = 2, alpha = 0.2)+
    xlab("Folds")+ylab("False Positive Rate")+
    ggtitle(paste0("False Positive Rate of Test Set for  ",scheme))+
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.text.x = element_text(size=10,face="bold"),
          axis.text.y = element_text(size=10,face="bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold")) # plot of each fold and their correponding distribution of mean performance
  
  
  tpr.performance = stack(as.data.frame(tpr.performance)) # changing the matrix to dataframe and stacking the columns
  
  p2 = ggplot(data = tpr.performance,aes(x = as.factor(ind), y = values))+
    geom_boxplot(notch = T, outlier.color = "red")+
    geom_point(col= "blue", size = 2, alpha = 0.2)+
    xlab("Folds")+ylab("True Positive Rate")+
    ggtitle(paste0("True Positive Rate of Test Set for  ",scheme))+
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.text.x = element_text(size=10,face="bold"),
          axis.text.y = element_text(size=10,face="bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold")) # plot of each fold and their correponding distribution of mean performance
  
  
  mean.performance = stack(as.data.frame(mean.performance)) # changing the matrix to dataframe and stacking the columns
  
  p3 = ggplot(data = mean.performance,aes(x = as.factor(ind), y = values))+
    geom_boxplot(notch = T, outlier.color = "red")+
    geom_point(col= "blue", size = 2, alpha = 0.2)+
    xlab("Folds")+ylab("Correct Prediction Rate")+
    ggtitle(paste0("Performance of Test Set for  ",scheme))+
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.text.x = element_text(size=10,face="bold"),
          axis.text.y = element_text(size=10,face="bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold")) # plot of each fold and their correponding distribution of mean performance
  
  
  auc = stack(as.data.frame(auc)) # changing the matrix to dataframe and stacking the columns
  
  p4 = ggplot(data = auc,aes(x = as.factor(ind), y = values))+
    geom_boxplot(notch = T, outlier.color = "red")+
    geom_point(col= "blue", size = 2, alpha = 0.2)+
    xlab("Folds")+ylab("AUC")+
    ggtitle(paste0("AUC of Test Set for  ", scheme))+
    theme(plot.title = element_text(hjust = 0.5,size = 12, face = "bold"),
          axis.text.x = element_text(size=10,face="bold"),
          axis.text.y = element_text(size=10,face="bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold")) # plot of each fold and their correponding distribution of mean performance
  
  
  
  # plotting all the data together 
  fpr.performance$ind = "FPR Test"
  tpr.performance$ind = "TPR Test"
  mean.performance$ind = "Correct \n Test"  # combining all the values of each column to one
  auc$ind = "AUC Test"
  
  
  
  mean.performance =  rbind(fpr.performance,tpr.performance,mean.performance,auc)
  
  mean.error = mean(mean.performance$values) # mean of the whole data mean performance
  
  
  p5 = ggplot(data = mean.performance,aes(x = as.factor(ind), y = values))+
    geom_boxplot(notch = T, outlier.color = "red")+
    geom_point(col= "blue", size = 2, alpha = 0.05)+
    xlab(" ")+ylab(" ")+
    ggtitle(paste0("Performance of Test Set for ", scheme))+
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.text.x = element_text(size=10,face="bold"),
          axis.text.y = element_text(size=10,face="bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold"))+
    guides(fill = "black")+
    stat_summary(fun.y="mean", geom="point", shape="x", size=8, fill="Green", col ="Orange")
  
  return(list(p1,p2,p3,p4,p5))
  
}

# plotting mean perofmance of the model for regression problems
plot.mean.performance.reg = function(rmse.train,rmse.test,rsq.train,rsq.test,scheme){
  # input : rsq.train = matrix of rsq for training set for each simulation and each fold
  #       : rsq.test = matrix of rsq for test set for each simulation and each fold
  #       : rsmse.train = matrix of rmse for training set for each simulation and each fold
  #       : rmse.test = matrix of rmse for test set for each simulation and each fold
  # output: plot of correct prediction rate and auc for different simulations
  
  rmse.train = stack(as.data.frame(rmse.train)) # changing the matrix to dataframe and stacking the columns
  
  p1 = ggplot(data = rmse.train, aes(x = as.factor(ind), y = values))+
    geom_boxplot(notch = T,outlier.color = "red")+
    geom_point(col= "blue", size = 2, alpha = 0.2)+
    xlab("Folds")+ylab("RMSE for train set")+
    ggtitle(paste0("Train RMSE for different fold using  ",scheme))+
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.text.x = element_text(size=10,face="bold"),
          axis.text.y = element_text(size=10,face="bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold"))+
    scale_y_continuous(limits = quantile(rmse.train$values, c(0.1, 0.9)))# plot of each fold and their correponding distribution of mean performance
  
  
  rmse.test = stack(as.data.frame(rmse.test)) # changing the matrix to dataframe and stacking the columns
  
  p2 = ggplot(data = rmse.test, aes(x = as.factor(ind), y = values))+
    geom_boxplot(notch = T, outlier.color = "red")+
    geom_point(col= "blue", size = 2, alpha = 0.2)+
    xlab("Folds")+ylab("RMSE for test set")+
    ggtitle(paste0("Test RMSE for different fold using  ",scheme))+
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.text.x = element_text(size=10,face="bold"),
          axis.text.y = element_text(size=10,face="bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold"))+
    scale_y_continuous(limits = quantile(rmse.test$values, c(0.1, 0.9)))# plot of each fold and their correponding distribution of mean performance
  
  
  rsq.train = stack(as.data.frame(rsq.train)) # changing the matrix to dataframe and stacking the columns
  
  p3 = ggplot(data = rsq.train, aes(x = as.factor(ind), y = values))+
    geom_boxplot(notch = T, outlier.color = "red")+
    geom_point(col= "blue", size = 2, alpha = 0.2)+
    xlab("Folds")+ylab("R^2 for train set")+
    ggtitle(paste0("Train R^2 for different folds using  ",scheme))+
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.text.x = element_text(size=10,face="bold"),
          axis.text.y = element_text(size=10,face="bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold")) +
    ylim(c(0,1))# plot of each fold and their correponding distribution of mean performance
  
  
  rsq.test = stack(as.data.frame(rsq.test)) # changing the matrix to dataframe and stacking the columns
  
  p4 = ggplot(data = rsq.test, aes(x = as.factor(ind), y = values))+
    geom_boxplot(notch = T, outlier.color = "red")+
    geom_point(col= "blue", size = 2, alpha = 0.2)+
    xlab("Folds")+ylab("R^2 for test set")+
    ggtitle(paste0("Test R^2 for different fold using  ", scheme))+
    theme(plot.title = element_text(hjust = 0.5,size = 12, face = "bold"),
          axis.text.x = element_text(size=10,face="bold"),
          axis.text.y = element_text(size=10,face="bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold"))+
    ylim(c(0,1))# plot of each fold and their correponding distribution of mean performance
  
  
  # plotting all the data together # combining all the values of each column to one
  rmse.train$ind = "RMSE train"
  rmse.test$ind = "RMSE test"
  rsq.train$ind = "R^2 train"
  rsq.test$ind = "R^2 test"
  mean.performance.rmse =  rbind(rmse.train,rmse.test)
  
  
  
  p5 = ggplot(data = mean.performance.rmse, aes(x = as.factor(ind), y = values))+
    geom_boxplot(notch = T, outlier.color = "red")+
    geom_point(col= "blue", size = 2, alpha = 0.05)+
    xlab(" ")+ylab("psi")+
    ggtitle(paste0("Performance of RMSE for ", scheme))+
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.text.x = element_text(size=10,face="bold"),
          axis.text.y = element_text(size=10,face="bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold"))+
    guides(fill = "black")+
    stat_summary(fun.y="mean", geom="point", shape="x", size=8, fill="Green", col ="Orange")+
    scale_y_continuous(limits = quantile(mean.performance.rmse$values, c(0.1, 0.9)))
  
  mean.performance.rsq =  rbind(rsq.train,rsq.test)
  
  p6 = ggplot(data = mean.performance.rsq, aes(x = as.factor(ind), y = values))+
    geom_boxplot(notch = T, outlier.color = "red")+
    geom_point(col= "blue", size = 2, alpha = 0.05)+
    xlab(" ")+ylab(" ")+
    ggtitle(paste0("Performance of R^2 for ", scheme))+
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.text.x = element_text(size=10,face="bold"),
          axis.text.y = element_text(size=10,face="bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold"))+
    guides(fill = "black")+
    stat_summary(fun.y="mean", geom="point", shape="x", size=8, fill="Green", col ="Orange")+
    scale_y_continuous(limits = quantile(mean.performance.rsq$values, c(0.1, 0.9)), breaks = )
  
  
  
  
  return(list(p1,p2,p3,p4,p5,p6))
  
}

# Simulation of model performance for linear Models
simulation.reg = function(formula.lm, dataset,title = " "){
  # input : formula.lm = formula for linear model
  #       : dataset = data for using in the model
  # output: run simulations
  
  simul = 100 # number of simulations
  jj = 1
  k = 5 # number of folds of sample
  val.RMSE.train = matrix(NA,simul,k, dimnames = list(NULL,paste(1:k))) # predefining a matrix for storing mean performance
  val.RMSE.test = matrix(NA,simul,k, dimnames = list(NULL,paste(1:k))) # predefining a matrix for storing mean performance
  val.rsq.train = matrix(NA,simul,k, dimnames = list(NULL,paste(1:k))) # predefining a matrix for storing mean performance
  val.rsq.test = matrix(NA,simul,k, dimnames = list(NULL,paste(1:k))) # predefining a matrix for storing mean performance
  
  for (jj in 1:simul) {
    ii= 1
    #set.seed(10)
    fold = sample(x = 1:k,size = nrow(dataset), replace = TRUE) # selecting random samples
    for (ii in 1:k) {
      train = dataset[fold!=ii,] # creating a training sample for samples expect folding
      test = dataset[fold==ii,]# creating a test sample for samples 
      lin.fit = lm(formula.lm,data = train)   
      lm.pred.train = predict(lin.fit,newdata = train)
      lm.pred.test = predict(lin.fit,newdata = test)
      
      val.RMSE.train[jj,ii] = sqrt(mean((train$UCS.psi-lm.pred.train)^2))
      val.RMSE.test[jj,ii] = sqrt(mean((test$UCS.psi-lm.pred.test)^2))
      
      val.rsq.train[jj,ii] = rsq.from.data(train$UCS.psi,lm.pred.train)
      val.rsq.test[jj,ii] = rsq.from.data(test$UCS.psi,lm.pred.test)
      
    }
  }
  
  
  # plotting the results using plot.mean.performance function
  for(mm in 5:6){
    plot(plot.mean.performance.reg(val.RMSE.train, val.RMSE.test, val.rsq.train,val.rsq.test, title)[[mm]])
  }
  
}

# Simulation for smoothing splines
simulation.ss = function(tunning.parameter, dataset){
  # input : formula.lm = formula for linear model
  #       : dataset = data for using in the model
  # output: run simulations
  attach(dataset)
  df.s = tunning.parameter
  simul = 100 # number of simulations
  jj = 1
  k = 5 # number of folds of sample
  val.RMSE.train = matrix(NA,simul,k, dimnames = list(NULL,paste(1:k))) # predefining a matrix for storing mean performance
  val.RMSE.test = matrix(NA,simul,k, dimnames = list(NULL,paste(1:k))) # predefining a matrix for storing mean performance
  val.rsq.train = matrix(NA,simul,k, dimnames = list(NULL,paste(1:k))) # predefining a matrix for storing mean performance
  val.rsq.test = matrix(NA,simul,k, dimnames = list(NULL,paste(1:k))) # predefining a matrix for storing mean performance
  
  for (jj in 1:simul) {
    ii= 1
    #set.seed(10)
    fold = sample(x = 1:k,size = nrow(dataset), replace = TRUE) # selecting random samples
    for (ii in 1:k) {
      train = dataset[fold!=ii,] # creating a training sample for samples expect folding
      test = dataset[fold==ii,]# creating a test sample for samples 
      
      lin.fit = gam(UCS.psi~ s(LL, df = df.s)+s(PL, df = df.s)+s(Clay, df = df.s)+s(Sand, df = df.s)+Lime+Cement+Asphalt, data = train)
      
      lm.pred.train = predict(lin.fit,newdata = train)
      lm.pred.test = predict(lin.fit,newdata = test)
      
      val.RMSE.train[jj,ii] = sqrt(mean((raw.data.allsoil$UCS.psi[fold!=ii]-lm.pred.train)^2))
      val.RMSE.test[jj,ii] = sqrt(mean((raw.data.allsoil$UCS.psi[fold==ii]-lm.pred.test)^2))
      
      val.rsq.train[jj,ii] = rsq.from.data(raw.data.allsoil$UCS.psi[fold!= ii],lm.pred.train)
      val.rsq.test[jj,ii] = rsq.from.data(raw.data.allsoil$UCS.psi[fold == ii],lm.pred.test)
      
    }
  }
  
  # plotting the results using plot.mean.performance function
  for(mm in 1:6){
    plot(plot.mean.performance.reg(val.RMSE.train, val.RMSE.test, val.rsq.train,val.rsq.test, "Smoothing Splines")[[mm]])
  }
  
}  

# simulation for finding optimum value of the tuning parameter
simulation.tuning.ss = function(tunning.parameter,dataset){
  df.s = tunning.parameter
  simul = 100 # number of simulations
  jj = 1
  k = 5 # number of folds of sample
  val.RMSE.test = matrix(NA,simul,k, dimnames = list(NULL,paste(1:k))) # predefining a matrix for storing mean performance
  for (jj in 1:simul) {
    ii= 1
    #set.seed(10)
    fold = sample(x = 1:k,size = nrow(dataset), replace = TRUE) # selecting random samples
    for (ii in 1:k) {
      train = dataset[fold!=ii,] # creating a training sample for samples expect folding
      test = dataset[fold==ii,]# creating a test sample for samples 
      
      lin.fit = gam(UCS.psi~ s(LL, df = df.s)+s(PL, df = df.s)+s(Clay, df = df.s)+s(Sand, df = df.s)+Lime+Cement+Asphalt, data = train)
      
      lm.pred.train = predict(lin.fit,newdata = train)
      lm.pred.test = predict(lin.fit,newdata = test)
      val.RMSE.test[jj,ii] = sqrt(mean((raw.data.allsoil$UCS.psi[fold==ii]-lm.pred.test)^2))
      
    }
  }
  val.RMSE.test = round(val.RMSE.test,0)
  return(median(val.RMSE.test))
  
  
}

# simulation for finding optimum value of the tuning parameter
simulation.tuning.ns = function(tunning.parameter,dataset){
  df.ns = tunning.parameter
  simul = 100 # number of simulations
  jj = 1
  k = 5 # number of folds of sample
  val.RMSE.test = matrix(NA,simul,k, dimnames = list(NULL,paste(1:k))) # predefining a matrix for storing mean performance
  for (jj in 1:simul) {
    ii= 1
    #set.seed(10)
    fold = sample(x = 1:k,size = nrow(dataset), replace = TRUE) # selecting random samples
    for (ii in 1:k) {
      train = dataset[fold!=ii,] # creating a training sample for samples expect folding
      test = dataset[fold==ii,]# creating a test sample for samples 
      
      lin.fit = lm(UCS.psi~ns(LL,df = df.ns)+ns(PL, df = df.ns)+ns(Clay, df = df.ns)+ns(Sand, df = df.ns)+Lime+Cement+Asphalt, data = train)
      
      lm.pred.train = predict(lin.fit,newdata = train)
      lm.pred.test = predict(lin.fit,newdata = test)
      val.RMSE.test[jj,ii] = sqrt(mean((raw.data.allsoil$UCS.psi[fold==ii]-lm.pred.test)^2))
      
    }
  }
  val.RMSE.test = round(val.RMSE.test,0)
  return(Mode(val.RMSE.test))
  
  
}


# plotting function for ggplot2 for normality test and distribution of the dataset
plot.ggplot2.pdf = function(dataframe.data, xlab1 = " ", ylab1 = " ", xlab2 = " ", ylab2 = "", title = " "){
  # Input: dataframe.data - dataframe of our dataset
  # Output: a list of plot object of RDH and histogram
  dataframe.data = data.frame(dataframe.data)
  aesx = unlist(dataframe.data)
  x = aesx
  p1 = ggplot(data = dataframe.data, aes(x = aesx))+
    geom_histogram(aes(y = ..density..), alpha = 0.25, fill = "cornflowerblue", color = "black", 
                   bins = round(1+3.3*log10(length(x))),
                   show.legend = TRUE)+  # using formula and calulating the number of bins
    stat_function(fun = function(x){
      dnorm(x, 
            mean = mean(x),
            sd = sd(x)
      )
    },
    color = "darkred", size = 1,
    show.legend = TRUE)+
    geom_density( color = "blue", show.legend = TRUE)+
    xlab(names(dataframe.data))+ ylab(ylab1)+
    ggtitle(paste("RDH, Kernel, and Normal PDF", title ))+
    theme(plot.title = element_text(hjust = 0.5,size = 10, face = "bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold"),
          legend.position = "right")
  
  p2 = ggplot(data = dataframe.data, aes(x = aesx))+
    geom_histogram(alpha = 0.25, fill = "cornflowerblue", color = "black", 
                   bins = round(1+3.3*log10(length(x))),
                   show.legend = TRUE)+
    xlab(names(dataframe.data))+ ylab("No. of Obeservations")+
    ggtitle(paste("Histogram ", title))+
    theme(plot.title = element_text(hjust = 0.5,size = 10, face = "bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold"),
          legend.position = "right")
    
  
  
  return.value = list(p1,p2)
  return(return.value)
}

plot.ggplot2.boxplot = function(dataframe.data,xlab = " ",ylab = " ", title = " "){
  
  # Input  : dataframe.data - dataframe of our dataset
  #        : xlab - label for x-axis
  #        : ylab - label for y-axis
  #        : title - title for the plot
  # Output : Returns plot object for the box plot
  
  data.to.use = stack(dataframe.data)
  p1 = ggplot(data = data.to.use, aes(x = as.factor(ind), y = values))+
    geom_boxplot(notch = T, outlier.color = "red")+
    geom_point(col= "blue", size = 2, alpha = 0.05)+
    theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
          axis.text.x = element_text(size=10,face="bold"),
          axis.text.y = element_text(size=10,face="bold"),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold"))+
    guides(fill = "black")+ ylab(ylab)+xlab(" ")+
    stat_summary(fun.y="mean", geom="point", shape="x", size=8, fill="Green", col ="Orange")+
    ggtitle(title)
  return(p1)
  
}

cleanup.NA.columns = function(data,column){
  # Function to remove the data with NA in their parameters and select the columns 
  #input    : data - dataset for cleaning
  #         : column - columns you want selected
  # output  : cleaned 
  
  data = data[complete.cases(data),] 
  data = data[,column]
  return(data)
  
}

rsq.coff.of.det = function(data.acutal, data.perdicted){
  
  n = length(data.acutal)
  sum.xy = sum(data.acutal*data.perdicted)
  sumx.sumy = sum(data.acutal)*sum(data.perdicted)
    
}

plot.ggplot2.lm = function(dataset, fit.object, title = " "){
  dataset$predictedvalue = fit.object$fitted.value 
  x1 = max(dataset$predictedvalue)
  y1 = 0.5*mean(dataset$UCS.psi)
  limxy = max(dataset$UCS.psi)
  ggplot(data = dataset, aes(y = UCS.psi, x = predictedvalue ))+
    geom_point(aes(col = Clay,size = LL), alpha = 0.5)+
    scale_size(range = c(0,6))+
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.text = element_text(size = 12),
          legend.title = element_text(hjust = 0.5, face = "bold"),
          axis.title =element_text(hjust = 0.5, face = "bold"))+
    scale_color_gradient(low="blue", high="red")+
    xlim(c(0,limxy))+ ylim(c(0,limxy))+
    geom_abline(slope = 1, intercept = 0, size = 1, col = "brown")+
    ggtitle(title)+
    xlab("Predicted UCS values")+ ylab("Actual UCS values")+
    annotate("text", x = x1 , y = y1, 
             label = paste(summary(fit.object)$r.squared))+
    annotate("text", x = x1 , y = y1+1*y1, 
             label = "R Squared")
}




