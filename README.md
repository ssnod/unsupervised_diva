# Unsupervised DIVA
This repository includes a collection of functions that implement a version of the **DIV**ergent **A**utoencoder (Kurtz, 2007, *Psychonomic Bulletin & Review*) artificial neural network classification architecture that is capable of unsupervised learning in R. See the following links for [R](https://github.com/ghonk/divaR) and [Matlab](https://github.com/nolanbconaway/DIVA) implementations of the supervised version of the architecture. For further reading on the unsupverised version, see Pothos et al. (2011, *Cognition*). 

## Running the model
The top level folder of this repository contains two initialization scripts for DIVA&mdash;`unsup_diva_init.r` and `unsup_diva_init_pothos_gridsearch.r`. The former executes a single paramterization of the model whereas the latter conducts a gridsearch across multiple parameterizations. A version of the K-means clustering algorithm is included in `kmeans_init.r` to compare against categorizations produced unsupervised DIVA. These initialization scripts contain comments providing a tutorial for how to use and evaluate the model.

Executing one of the initialization scripts with runs an version of the model using the first set of items from Pothos et al. (2011). 

```r
source('unsup_diva_init.r')
```

Each initialization script can be edited to change the model parameters. Within each script, comments are provided to explain each parameter.

```r

# Set parameters for a single set of initializations
model <- list(num_blocks        = 2,      
              num_inits         = 1,        
              wts_range         = 1,         
              num_hids          = 3,        
              learning_rate     = .3,       
              beta_val          = 5,        
              out_rule          = 'linear', 
              constrain_channel = FALSE, 
              num_channels      = 2,          
              train_length      = 2,          
              spawn_threshold   = 0.05)

# OR Create then unpack list of search paramters for a gridsearch
param_list <- list(num_blocks        = c(2,3),       
                   num_inits         = c(100),
                   wts_range         = c(.5,1),
                   num_hids          = c(3,6),
                   learning_rate     = c(.15,.6),
                   beta_val          = c(0,10),
                   out_rule          = 'linear',
                   constrain_channel = FALSE,
                   num_channels      = 2,
                   train_length      = c(1,2),
                   spawn_threshold   = c(0.05, 0.1))                                           
search_params <- do.call(expand.grid, param_list)

```

The gridsearch script contains evaluation code specifically tailored to the sets of items included in Pothos et al. (2011). 

Executing a script will export:
<ul>
<li> Single parameterization script: a .csv file containing the class assignments produced for each item and a visualization of how the model partitioned the feature space into classes. </li>
<li> Gridsearch script: .csv files that provide a measure of fit for each paramterization and a visualization of how each parameter relates to model performance.</li>
</ul>
