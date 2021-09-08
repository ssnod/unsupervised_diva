# 				Unsupervised DIVA Gridsearch Initialization Script

# prepare R enivironment for running model

## set directories
home_dir <- getwd()
model_dir <- paste0(home_dir, "/model")
data_dir <- paste0(home_dir, "/datasets")
out_dir <- paste0(home_dir, "/output")

## load required packages
library(ggplot2) # for plotting results
library(plyr) # for summarizing gridsearch results

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

# Initialize model parameters to search over
param_list <- list(num_blocks = c(2,3),       # number of passes through all training items
			  num_inits     = c(100),         # number of diva initializations to run (usually corresponds to number of subjects)
			  wts_range     = c(.5,1),        # range of initial weights
			  num_hids      = c(3,6),         # number of hidden units 
			  learning_rate = c(.15,.6),      # determines size of weight updates after each item
			  beta_val      = c(0,10),        # focusing parameter (cf. attentional weighting)
			  out_rule      = 'linear',       # output activation function ("linear" or "logistic")
			  # Pameters for unsupervised classification
			  constrain_channel = FALSE,      # when TRUE, caps the maximum number of classes (i.e., to model instructional manipulations)
			  num_channels = 2,               # if contrain_channel is TRUE, reflects the maximum number of classes allowed
			  train_length = c(1,2),          # how many times backprop is run after channel assignment (i.e., how strong of an impact items has on a channel)
			  spawn_threshold = c(0.05, 0.1)) # range around chance response probabilities to determine whether a new channel is created 
                                              # e.g., 2 classes with spawn_threshold = 0.05 means that any item with a response probability of 0.45 - 0.55 will lead to the creation of a new channel

search_params <- do.call(expand.grid, param_list)

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# prepare data for model

## initialize one of the pothos et al datasets (add for loop to this script to get predictions for all structures)
### type is any integer from 1-9 (see 'psych_stimuli.r' for a description of each category structure)
cat_structure <- 1
cases <- pothos_stim(type = cat_structure)

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# train models

## init result structure
result <- list(rep(NA, nrow(search_params)))

## iterate through each parameterization
for(param in 1:nrow(search_params)){
	# create model
	model <- list(num_blocks    = search_params$num_blocks[[param]],
		  num_inits     = search_params$num_inits[[param]], #100
		  wts_range     = search_params$wts_range[[param]],
		  num_hids      = search_params$num_hids[[param]],
		  learning_rate = search_params$learning_rate[[param]],
		  beta_val      = search_params$beta_val[[param]],
		  out_rule      = search_params$out_rule[[param]], 
		  constrain_channel = search_params$constrain_channel[[param]], 
		  num_channels = search_params$num_channels[[param]], 
		  train_length = search_params$train_length[[param]], 
		  spawn_threshold = search_params$spawn_threshold[[param]])
	## assign inputs/labels to model structure
	model$inputs <- cases$inputs
	model$labels <- cases$labels 

	## train model
	result[[param]] <- run_diva_unsup(model)
}

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# evaluate gridsearch result

# intialize output summary
output <- data.frame(blocks = numeric(), inits = numeric(),
		wtRange = numeric(), hids = numeric(),  lrate = numeric(),
		beta = numeric(), outRule =character(), constrainChannel = logical(),
		numChannel = numeric(), train_length = numeric(),
		spawn = numeric(), numCats = numeric(),
		matchHuman=numeric(), parameterization = numeric())

# set up number of classes for each of the pothos category structures
two_structs <- c(1,2,3,5,6)
three_structs <- 4
four_structs <- 8
five_structs <- c(7,9)


# iterate through each param setting and determine whether it matches human preferred categorization
for(r in 1:length(result)){
	tempResult <- result[[r]]
	# iterate through each model initialization within each parameterization
	for(run in 1:unlist(tempResult$model[2])){
		## get current parameterization
		tempParams <- data.frame(blocks = tempResult$model[1], inits = tempResult$model[2],
			wtRange = tempResult$model[3], hids = tempResult$model[4], lrate = tempResult$model[5],
			beta = tempResult$model[6], outRule = tempResult$model[7], constrainChannel = tempResult$model[8],
			numChannel = tempResult$model[9], train_length = tempResult$model[10],
			spawn = tempResult$model[11])
		names(tempParams) <- c('blocks', 'inits', 'wtRange', 'hids', 'lrate', 'beta', 'outRule',
			'constrainChannel',	'numChannel', 'train_length', 'spawn')

		## get result from current init
    	current_init <- data.frame(tempResult$classifications[,,run][(nrow(tempResult$classifications[,,run])-15):nrow(tempResult$classifications[,,run]),])
    	### number of categories produced during init
    	tempParams$numCats <- length(unique(current_init$predClass))
    	### setup counters to evaluate result against human preferred categories
    	tempParams$matchHuman <- 0
    	tempParams$parameterization <- r

    	## check if correct number of classes was produced
    	if(length(unique(current_init[,3])) == length(unique(current_init[,4]))){
      		prediction <- actual <- list()

      		for(cat in 1:tempParams$numCats){
       			prediction[[cat]] <- current_init[current_init[,4] == unique(current_init[,4])[cat], 1]
        		actual[[cat]] <- current_init[current_init[,3] == unique(current_init[,3])[cat], 1]                     
      		}
      

	      # eval whether two-class category structures match human preferences
	      if(cat_structure %in% two_structs){
	        if((identical(prediction[[1]], actual[[1]]) | identical(prediction[[1]], actual[[2]])) 
	          & (identical(prediction[[2]], actual[[1]]) | identical(prediction[[2]], actual[[2]]))){
	              tempParams$matchHuman <- 1
	          }
	      }

	      # eval whether three-class category structures match human preferences
	      else if(cat_structure %in% three_structs){
	        if((identical(prediction[[1]], actual[[1]]) 
	          | identical(prediction[[1]], actual[[2]]) 
	          | identical(prediction[[1]], actual[[3]])) 
	          & (identical(prediction[[2]], actual[[1]]) 
	          | identical(prediction[[2]], actual[[2]]) 
	          | identical(prediction[[2]], actual[[3]]))
	          & (identical(prediction[[3]], actual[[1]]) 
	          | identical(prediction[[3]], actual[[2]]) 
	          | identical(prediction[[3]], actual[[3]]))){
	              tempParams$matchHuman <- 1
	        }
	        
	      }

	      # eval whether four-class category structures match human preferences
	      else if(cat_structure %in% four_structs){
	        if((identical(prediction[[1]], actual[[1]]) 
	          | identical(prediction[[1]], actual[[2]])
	          | identical(prediction[[1]], actual[[3]]) 
	          | identical(prediction[[1]], actual[[4]])) 
	          & (identical(prediction[[2]], actual[[1]]) 
	          | identical(prediction[[2]], actual[[2]])
	          | identical(prediction[[2]], actual[[3]])  
	          | identical(prediction[[2]], actual[[4]]))
	          & (identical(prediction[[3]], actual[[1]]) 
	          | identical(prediction[[3]], actual[[2]])
	          | identical(prediction[[3]], actual[[3]])  
	          | identical(prediction[[3]], actual[[4]]))
	          & (identical(prediction[[4]], actual[[1]]) 
	          | identical(prediction[[4]], actual[[2]])
	          | identical(prediction[[4]], actual[[3]])  
	          | identical(prediction[[4]], actual[[4]]))){
	              tempParams$matchHuman <- 1
	        }
	          
	      }

	      # eval whether five-class category structures match human preferences
	      else if(cat_structure %in% five_structs){
	        if((identical(prediction[[1]], actual[[1]]) 
	          | identical(prediction[[1]], actual[[2]])
	          | identical(prediction[[1]], actual[[3]])
	          | identical(prediction[[1]], actual[[4]])            
	          | identical(prediction[[1]], actual[[5]])) 
	          & (identical(prediction[[2]], actual[[1]]) 
	          | identical(prediction[[2]], actual[[2]])
	          | identical(prediction[[2]], actual[[3]])
	          | identical(prediction[[2]], actual[[4]])  
	          | identical(prediction[[2]], actual[[5]]))
	          & (identical(prediction[[3]], actual[[1]]) 
	          | identical(prediction[[3]], actual[[2]])
	          | identical(prediction[[3]], actual[[3]])
	          | identical(prediction[[3]], actual[[4]])  
	          | identical(prediction[[3]], actual[[5]]))
	          & (identical(prediction[[4]], actual[[1]]) 
	          | identical(prediction[[4]], actual[[2]])
	          | identical(prediction[[4]], actual[[3]])
	          | identical(prediction[[4]], actual[[4]])  
	          | identical(prediction[[4]], actual[[5]]))
	          & (identical(prediction[[5]], actual[[1]]) 
	          | identical(prediction[[5]], actual[[2]])
	          | identical(prediction[[5]], actual[[3]])
	          | identical(prediction[[5]], actual[[4]])  
	          | identical(prediction[[5]], actual[[5]]))){
	              tempParams$matchHuman <- 1
	        }
	      }
	    }

	    # add current init to result
		output <- rbind(output, tempParams)  
  	}
}

# summarize result
output_summary <- ddply(output, .(parameterization, blocks, wtRange, hids,
	lrate,beta,outRule,constrainChannel,numChannel,train_length, spawn), summarize,
	propMatches = mean(matchHuman), meanCats = mean(numCats), 
	sdCats = sd(numCats), minCats = min(numCats), maxCats = max(numCats))

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# save output

## create plots to visualization how parameters affect performance
p1 <- ggplot(output_summary, aes(x = wtRange, y = propMatches))+
	geom_point(position = position_jitter(width = .05)) +
	coord_cartesian(ylim=c(0,1)) + 
	scale_y_continuous(breaks=seq(0, 1, 0.2), expand = c(0,0))+
	labs(y = "Proportion Matching Human Preference\n", x = "Init. Weight Range") +
	theme_bw()

p2 <- ggplot(output_summary, aes(x = hids, y = propMatches))+
	geom_point(position = position_jitter(width = .3)) +
	coord_cartesian(ylim=c(0,1)) + 
	scale_y_continuous(breaks=seq(0, 1, 0.2), expand = c(0,0))+
	labs(y = "Proportion Matching Human Preference\n", x = "# Hidden Units") +
	theme_bw()

p3 <- ggplot(output_summary, aes(x = lrate, y = propMatches))+
	geom_point(position = position_jitter(width = .05)) +
	coord_cartesian(ylim=c(0,1)) + 
	scale_y_continuous(breaks=seq(0, 1, 0.2), expand = c(0,0))+
	labs(y = "Proportion Matching Human Preference\n", x = "Learn Rate") +
	theme_bw()

p4 <- ggplot(output_summary, aes(x = beta, y = propMatches))+
	geom_point(position = position_jitter(width = 1)) +
	coord_cartesian(ylim=c(0,1)) + 
	scale_y_continuous(breaks=seq(0, 1, 0.2), expand = c(0,0))+
	labs(y = "Proportion Matching Human Preference\n", x = "Focusing") +
	theme_bw()

p5 <- ggplot(output_summary, aes(x = spawn, y = propMatches))+
	geom_point(position = position_jitter(width = .005)) +
	coord_cartesian(ylim=c(0,1)) + 
	scale_y_continuous(breaks=seq(0, 1, 0.2), expand = c(0,0))+
	labs(y = "Proportion Matching Human Preference\n", x = "Spawn Modifier") +
	theme_bw()

p6 <- ggplot(output_summary)+
	geom_density(aes(x = maxCats), color = 'blue', size = 2)+
	geom_density(aes(x = meanCats), color = 'black', size = 2)+
	geom_density(aes(x = minCats), color = 'orange', size = 2)+
	coord_cartesian(xlim=c(0,10)) + 
	scale_x_continuous(breaks=seq(0, 10, 1), expand = c(0,0))+
	labs(y = "Density\n", x = "Max (Blue), Mean (Black), & Min (Orange) Categories Produced") +
	theme_bw()

### write results of init to file 
if(write_output){
	setwd(out_dir)
	write.csv(output, paste0('DIVA_Pothos_Structure_', cat_structure,'_Full.csv'), row.names = FALSE)
	write.csv(output_summary, paste0('DIVA_UNSUP_Structure_', cat_structure,'_Summary.csv'), row.names = FALSE)
	jpeg(filename = "Gridsearch Results.jpg", pointsize =12, quality = 200, bg = "white", res = NA, restoreConsole = TRUE)
		multiplot(plotlist = list(p1,p2,p3,p4,p5,p6), cols = 3)
	dev.off()
}else{
	multiplot(plotlist = list(p1,p2,p3,p4,p5,p6), cols = 3)
}


# print any errors/warnings to screen when executing script
warnings()
