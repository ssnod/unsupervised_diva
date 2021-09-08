# 4 dimensional binary family resemblance data
## uni-dimensional sort bias from Medin, Wattenmaker, & Hampson (1987)
fr_cats <- function(){
  
  in_pattern <- 
    matrix(c( 1,  1,  1,  1, 
             -1,  1,  1,  1,
              1, -1,  1,  1,
              1,  1, -1,  1,
              1,  1,  1, -1,
             -1, -1, -1, -1,
              1, -1, -1, -1,
             -1,  1, -1, -1,
             -1, -1,  1, -1,
             -1, -1, -1,  1), 
      nrow = 10, ncol = 4, byrow = TRUE)   

  cat_assignment <- 
    matrix(c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
      ncol = 10, byrow = TRUE)

return(list(inputs = in_pattern, 
            labels = cat_assignment))

}

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# Automatically generated rule-based continues categories
continuous_cats <- function(dist, num_items){
  
  if(dist == "normal"){
    in_pattern <- matrix(c(rnorm(n=num_items*2, mean=-.5,sd =.2), 
      rnorm(n=num_items*2,mean = .5,sd=.2)),nrow = num_items*2, ncol = 2, byrow = TRUE)
  }else{
    ## default to uniform distribution
    in_pattern <- matrix(c(runif(num_items*2,-1,0), 
      runif(num_items*2,0,1)),nrow = num_items*2, ncol = 2, byrow = TRUE)
  }
  
  cat_assignment <- 
    matrix(c(rep(1, num_items), rep(2,num_items)),
      ncol = num_items*2, byrow = TRUE)

return(list(inputs = in_pattern, 
            labels = cat_assignment))

}

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# Shepard, Hovland, and Jenkins (1961) elemental category structures 
shj_cats <- function(type){
  
  in_pattern <- 
    matrix(c(-1, -1, -1,
             -1, -1,  1,
             -1,  1, -1,
             -1,  1,  1,
              1, -1, -1,
              1, -1,  1,
              1,  1, -1,
              1,  1,  1), 
      nrow = 8, ncol = 3, byrow = TRUE)   

  cat_assignment <- 
    matrix(c(1, 1, 1, 1, 2, 2, 2, 2,  # type I
             1, 1, 2, 2, 2, 2, 1, 1,  # type II
             1, 1, 2, 1, 1, 2, 2, 2,  # type III
             1, 1, 1, 2, 1, 2, 2, 2,  # type IV
             2, 1, 1, 1, 1, 2, 2, 2,  # type V
             1, 2, 2, 1, 2, 1, 1, 2,  # type VI
             1, 1, 2, 2, 3, 3, 4, 4), # type II multiclass  
      ncol = 8, byrow = TRUE)

return(list(inputs = in_pattern, 
            labels = cat_assignment[type,]))

}

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

# Pothos et al. (2011) stimuli for unsupervised categorization
pothos_stim <- function(type){
  
  # Two clusters, equal number of points
  if(type == 1){
    in_pattern <- matrix(c(.1, .1,
                         .1, .2,
                         .2, .1,
                         .2, .2,
                         .1, .3,
                         .3, .1,
                         .3, .2,
                         .2, .3,
                         1.0, 1.0,
                         .9, 1.0,
                         1.0, .9,
                         .9, .9,
                         1.0, .8,
                         .8, 1.0,
                         .8, .9,
                         .9, .8), 
          nrow = 16, ncol = 2, byrow = TRUE)   

      cat_assignment <- 
          matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2),
          ncol = 16, byrow = TRUE)
  } 

  # Two clusters, unequal number of points
  else if(type == 2){
    in_pattern <- matrix(c(.1, .1,
                           .1, .2,
                           .2, .1,
                           .2, .2,
                           .1, .3,
                           .3, .1,
                           .3, .2,
                           .2, .3,
                           .3, .3,
                           .1, .4,
                           1.0, .9,
                           .9, .9,
                           1.0, .8,   
                           .8, 1.0,
                           .9, 1.0,
                           1.0, 1.0),
          nrow = 16, ncol = 2, byrow = TRUE) 

    cat_assignment <- 
      matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
          ncol = 16, byrow = TRUE)
  } 

  # Two clusters, equal number of points but more spread out
  else if(type == 3){
    in_pattern <- matrix(c(.1, .1,
                           .1, .3,
                           .3, .1,
                           .4, .4,
                           .1, .4,
                           .4, .1,
                           .4, .2,
                           .2, .4,
                           1.0, 1.0,
                           .9, 1.0,
                           1.0, .9,
                           .7, .7,
                           1.0, .7,
                           .7, 1.0,
                           .7, .9,
                           .9, .7),
          nrow = 16, ncol = 2, byrow = TRUE) 

    cat_assignment <- 
      matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2),
          ncol = 16, byrow = TRUE)
  } 

  # three clusters
  else if(type == 4){
    in_pattern <- matrix(c(.1, 1.0,
                           .2, 1.0,
                           .1, .9,
                           .2, .9,
                           .1, .8,
                           1.0, 1.0,
                           .9, 1.0,
                           1.0, .9,
                           .9, .9,
                           .8, 1.0,
                           .4, .1,
                           .5, .1,
                           .6, .1,
                           .4, .2,
                           .5, .2,
                           .6, .2),
          nrow = 16, ncol = 2, byrow = TRUE) 

    cat_assignment <- 
      matrix(c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3),
          ncol = 16, byrow = TRUE)
  } 
  
  # two clusters with some noise (basically the two clusters are a little more
  # spread out so that the boundaries are not as clear cut as in the other
  # cases
  else if(type == 5){
    in_pattern <- matrix(c(.1, .1,
                           .1, .3,
                           .3, .1,
                           .5, .5,
                           .1, .4,
                           .4, .2,
                           .4, .4,
                           .2, .4,
                           1.0, 1.0,
                           .9, 1.0,
                           1.0, .9,
                           .7, .7,
                           .9, .7,
                           .7, 1.0,
                           .7, .9,
                           .7, .6),
          nrow = 16, ncol = 2, byrow = TRUE) 

    cat_assignment <- 
      matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2),
          ncol = 16, byrow = TRUE)
  } 

  # Very unequal clusters closer together
  else if(type == 6){
    in_pattern <- matrix(c(.1, .1,
                           .1, .2,
                           .2, .1,
                           .2, .2,
                           .1, .3,
                           .3, .1,
                           .3, .2,
                           .2, .3,
                           .3, .3,
                           .1, .4,
                           .2, .4,
                           .3, .4,
                           .5, .5,
                           .6, .5,
                           .5, .6,
                           .6, .6),
          nrow = 16, ncol = 2, byrow = TRUE) 

    cat_assignment <- 
      matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2),
          ncol = 16, byrow = TRUE)
  } 

  # Five clusters, four in the corners, one in the middle
  else if(type == 7){
    in_pattern <- matrix(c(.1, .1,
                           .1, .2,
                           .2, .1,
                           1.0, 1.0,
                           1.0, .9,
                           .9, 1.0,
                           .1, 1.0,
                           .2, 1.0,
                           .1, .9,
                           1.0, .1,
                           .9, .1,
                           1.0, .2,
                           .5, .5,
                           .5, .6,
                           .6, .5,
                           .6, .6),
          nrow = 16, ncol = 2, byrow = TRUE) 

    cat_assignment <- 
      matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5),
          ncol = 16, byrow = TRUE)
  }

  # Noisy pattern
  else if(type == 8){
    in_pattern <- matrix(c(.5, .5,
                           .6, 1.0,
                           .1, .7,
                           .2, .9,
                           .5, .6,
                           1.0, 1.0,
                           .7, .8,
                           1.0, .8,
                           .9, .8,
                           .8, 1.0,
                           .3, .4,
                           .2, .2,
                           .8, .4,
                           .9, .2,
                           .3, .1,
                           .9, .3),
          nrow = 16, ncol = 2, byrow = TRUE) 

    cat_assignment <- 
      matrix(c(1, 2, 3, 3, 1, 2, 2, 2, 2, 2, 1, 1, 4, 4, 1, 4),
          ncol = 16, byrow = TRUE)
  } 

  # Embedded category
  else if(type == 9){
    in_pattern <- matrix(c(.1, .1,
                           .3, .1,
                           .7, .1,
                           1.0, .1,
                           1.0, .4,
                           1.0, .8,
                           .8, 1.0,
                           .6, 1.0,
                           .3, 1.0,
                           .1, .7,
                           .4, .5,
                           .5, .4,
                           .5, .5,
                           .5, .6,
                           .6, .6,
                           .6, .5),
          nrow = 16, ncol = 2, byrow = TRUE) 

    cat_assignment <- 
      matrix(c(1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5),
          ncol = 16, byrow = TRUE)
  } 

  else{
    print('Error in category structure selection')
  }

  return(list(inputs = in_pattern, 
              labels = cat_assignment))
}

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
