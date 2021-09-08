# 				Kmeans Script 
# used as an off-the-shelf clustering comparator to unsupervised diva

# prepare R enivironment for running model

## set directories
home_dir <- getwd()
model_dir <- paste0(home_dir, "/model")
data_dir <- paste0(home_dir, "/datasets")
out_dir <- paste0(home_dir, "/output")

## load required packages
library(ggplot2) # for plotting results
library(ClusterR) # for kmeans

## load utilities script
setwd(model_dir)
source('diva_fxns_eval.r') # functions for evaluating kmeans

## load pre-built pysch datasets (or read in custom datasets here)
setwd(data_dir)
source('psych_stimuli.r')

## save output to file?
write_output <- TRUE


#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# prepare data for model

# ## initialize family resemblance structure data
# cases <- fr_cats()

# ## initialize 2 dimensional rule-base category with randomly created continues features
# cases <- continuous_cats(dist = "normal", num_items = 20)

# ## initialize one of the shj datasets (add for loop to this script to get predictions for all structures)
# ### type is any integer from 1-8 (see 'psych_stimuli.r' for a description of each category structure)
# cat_structure <- 1
# cases <- shj_cats (type = cat_structure)

## initialize one of the pothos et al datasets (add for loop to this script to get predictions for all structures)
### type is any integer from 1-9 (see 'psych_stimuli.r' for a description of each category structure)
cat_structure <- 1
cases <- pothos_stim(type = cat_structure)

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# fit model to data

## determine optimal number of clusters
# wcss <- vector()
# for(i in 1:15) wcss[i] <- sum(kmeans(cases$inputs,i, nstart = 20)$withinss)
# plot(1:15, wcss,type = 'b', main = paste('Clusters'), xlab = 'Number of Clusters', ylab = 'WCSS')

## train model
km_fit <- kmeans(cases$inputs, centers = length(unique(as.vector(cases$labels))), nstart = 50)
### append clustering solution to result
#### note: the model assigns classes arbitrarily (see grisearch init script for how to evaluate accuracy).
result <- data.frame(cbind(cases$inputs, c(cases$labels), km_fit$cluster))
names(result)[(length(names(result))-1):length(names(result))] <- c("actual", "predicted")

## generalization gradient (for 2D continuous structures)
### initalize feature space
gen_grid <- create_grid(xrange = c(0,1), yrange = c(0,1), by.x = 0.05, by.y = 0.05)
### get predictions for full range of feature space
gen_grid$pred <- predict_KMeans(gen_grid, km_fit$centers)
### plot result
gen_plot <- ggplot(gen_grid, aes(x = x, y = y, fill = as.numeric(pred)))+
	geom_tile(stat = "identity")+
	geom_text(data = result , aes(x = X1, y = X2, label = as.factor(actual), color = as.factor(actual),fill = NULL), size = 6)+
		labs(x = 'Dim X', y = 'Dim Y', color = "Actual Class", fill = "Predicted Class",title = paste('Pothos Structure',cat_structure) )+
		scale_color_manual(values = rep('yellow',5))+
		theme_bw()
gen_plot


### write results of init to file
if(write_output){
	setwd(out_dir)
	write.csv(result, paste0('KMEANS_Structure_', cat_structure,'.csv'), row.names = FALSE)
	ggsave(paste0('KMEANS_Structure_', cat_structure,'.png'),plot = gen_plot,width = 8, height = 6, units = 'in')
}
