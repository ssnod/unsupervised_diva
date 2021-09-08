# 				Unsupervised DIVA Single Initialization Script

# prepare R enivironment for running model

## set directories
home_dir <- getwd()
model_dir <- paste0(home_dir, "/model")
data_dir <- paste0(home_dir, "/datasets")
out_dir <- paste0(home_dir, "/output")

## load required packages
library(ggplot2) # for plotting results

## load utilities script
setwd(model_dir)
source('diva_fxns_supervised.r') # core functions of DIVA model
source('diva_fxns_unsup.r') # functions to run DIVA in unsupervised mode
source('diva_fxns_eval.r') # functions for evaluating a trained unsupervised DIVA model

## load pre-built pysch datasets (or read in custom datasets here)
setwd(data_dir)
source('psych_stimuli.r')

## save output to file?
write_output <- TRUE


#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# Initialize model parameters
model <- list(num_blocks    = 2,         # number of passes through all training items
			  num_inits     = 1,         # number of diva initializations to run (usually corresponds to number of subjects)
			  wts_range     = 1,         # range of initial weights
			  num_hids      = 3,         # number of hidden units 
			  learning_rate = .3,       # determines size of weight updates after each item
			  beta_val      = 5,         # focusing parameter (cf. attentional weighting)
			  out_rule      = 'linear',  # output activation function ("linear" or "logistic")
			  # Pameters for unsupervised classification
			  constrain_channel = FALSE, # when TRUE, caps the maximum number of classes (i.e., to model instructional manipulations)
			  num_channels = 2,          # if contrain_channel is TRUE, reflects the maximum number of classes allowed
			  train_length = 2,          # how many times backprop is run after channel assignment (i.e., how strong of an impact items has on a channel)
			  spawn_threshold = 0.05)    # range around chance response probabilities to determine whether a new channel is created 
                                         # e.g., 2 classes with spawn_threshold = 0.05 means that any item with a response probability of 0.45 - 0.55 will lead to the creation of a new channel

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

# # Assign inputs / labels to model structure
model$inputs <- cases$inputs
model$labels <- cases$labels

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# train model
setwd(home_dir)
## single category structures
result <- run_diva_unsup(model)

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# evaluate trained model
## note: this to some extent will depend on the category structure being used. 
### Example code here is for a continuous 2d category structure.

## view end of training classification decisions
### note: the model assigns classes arbitrarily (it depends on presentation order of items during training).
#### the output may appear incorrect when all "A" labelled items are placed into the "B" class and vice versa.
#### when % correct < 50, manually reassign predicted class labels.
result$classification

## inspect result structure
str(result, vec.len = 2)

## generalize model to a test set (takes )
test_pred <- diva_predict(inputs = model$inputs, labels = c(model$labels), 
	weights = result$weights[[1]], model = model)
print(test_pred)

## confusion matrix
### can also apply directly to result with a list-based function
table(test_pred$pred_class , test_pred$act_class)

## generalization gradient (for 2D continuous structures)
### initalize feature space
gen_grid <- create_grid(xrange = c(0,1), yrange = c(0,1), by.x = 0.05, by.y = 0.05)
### get predictions for full range of feature space
gen_pred <- diva_predict(inputs = as.matrix(gen_grid), labels = NULL, 
		weights = result$weights[[1]], model = model)
### plot result
gen_plot <- ggplot(gen_pred, aes(x = x, y = y, fill = pred_class))+
	geom_tile(stat = "identity")+
	geom_text(data = test_pred , aes(x = X1, y = X2, label = as.factor(act_class), color = as.factor(act_class ),fill = NULL), size = 6)+
		labs(x = 'Dim X', y = 'Dim Y', color = "Actual Class", fill = "Predicted Class",title = paste('Pothos Structure',cat_structure))+
		scale_color_manual(values = rep('yellow',5))+
		theme_bw()
gen_plot


### write results of init to file
if(write_output){
	setwd(out_dir)
	write.csv(test_pred, paste0('DIVA_UNSUP_Structure_', cat_structure,'.csv'), row.names = FALSE)
	ggsave(paste0('DIVA_UNSUP_Structure_', cat_structure,'.png'),plot = gen_plot,width = 8, height = 6, units = 'in')
}

# print any errors/warnings to screen when executing script
warnings()
