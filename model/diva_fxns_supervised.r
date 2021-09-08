# core diva functions from supervised model
## thanks to garrett honke (github: ghonk) for coding up these functions in R

# # # backprop
# backpropagate error and update weights
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
backprop <- function(out_wts, in_wts, out_activation, current_target, 
                     hid_activation, hid_activation_raw, ins_w_bias, learning_rate){

  # # # calc error on output units
  out_delta <- 2 * (out_activation - current_target)
  
  # # # calc error on hidden units
  hid_delta <- out_delta %*% t(out_wts)
  hid_delta <- hid_delta[,2:ncol(hid_delta)] * sigmoid_grad(hid_activation_raw)
  
  # # # calc weight changes
  out_delta <- learning_rate * (t(hid_activation) %*% out_delta)
  hid_delta <- learning_rate * (t(ins_w_bias) %*% hid_delta)

  # # # adjust wts
  out_wts <- out_wts - out_delta
  in_wts <- in_wts - hid_delta

  return(list(out_wts = out_wts, 
              in_wts  = in_wts))

}

# # # forward_pass
# conduct forward pass
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
forward_pass <- function(in_wts, out_wts, inputs, out_rule) {
  # # # init needed vars
  num_feats <- ncol(out_wts)
  num_cats  <- dim(out_wts)[3]
  num_stims <- nrow(inputs)
  if (is.null(num_stims)) {num_stims <- 1}

  
  # # # add bias to ins
  bias_units <- matrix(rep(1, num_stims), ncol = 1, nrow = num_stims)
  ins_w_bias <- cbind(bias_units,
    matrix(inputs, nrow = num_stims, ncol = num_feats, byrow = TRUE))

  # # # ins to hids propagation
  hid_activation_raw <- ins_w_bias %*% in_wts
  hid_activation <- sigmoid(hid_activation_raw)

  # # # add bias unit to hid activation
  hid_activation <- cbind(bias_units, hid_activation)  

  # # # hids to outs propagation
  out_activation <- array(rep(0, (num_stims * num_feats * num_cats)), 
    dim = c(num_stims, num_feats, num_cats))
  
  # # NEED VECTORIZED HERE?
  # # # get output activation
  for (category in 1:num_cats) {
  	out_activation[,,category] <- hid_activation %*% out_wts[,,category]
  }
  
  # # # apply output activatio rule
  if(out_rule == 'sigmoid') {
  	out_activation <- sigmoid(out_activation)
  }

  return(list(out_activation     = out_activation, 
              hid_activation     = hid_activation,
              hid_activation_raw = hid_activation_raw, 
              ins_w_bias         = ins_w_bias))

}

# # # get_wts
# generate net weights
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
get_wts <- function(num_feats, num_hids, num_cats, wts_range, wts_center) {
  # # # set bias
  bias <- 1
  
  # # # generate wts between ins and hids
  in_wts <- 
    (matrix(runif((num_feats + bias) * num_hids), ncol = num_hids) - 0.5) * 2 
  in_wts <- wts_center + (wts_range * in_wts)

  # # # generate wts between hids and outs
  out_wts <- 
    (array(runif((num_hids + bias) * num_feats * num_cats), 
      dim = c((num_hids + bias), num_feats, num_cats)) - 0.5) * 2
  out_wts <- wts_center + (wts_range * out_wts)   
  
  return(list(in_wts  = in_wts, 
              out_wts = out_wts))

}

# # # global_scale
# scale inputs to 0/1
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
global_scale <- function(x) { x / 2 + 0.5 }

# # # train_plot
# function to produce line plot of training
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
train_plot <- function(training) {
  # # # get dimensions
  n_cats <- dim(training)[2]
  xrange <- c(1, dim(training)[1])
  yrange <- range(training)

  # # # open plot device
  pdf('training_plot.pdf')

  # # # create frame
  plot(xrange, c(.01, yrange[2]), xlab = 'Block Number', ylab = 'Accuracy')

  # # # aesthetics 
  colors <- rainbow(n_cats)
  line_type <- c(1:n_cats)
  plot_char <- seq(18, 18 + n_cats, 1)

  # # # plot lines
  for (i in 1:n_cats) {
    target_cat <- training[,i]
    lines(seq(1, xrange[2], 1), target_cat, type = 'b', lwd = 1.5, 
      lty = line_type[i], col = colors[i],  pch = plot_char[i])
  }

  # # # title and legend
  title('DIVA Training Accuracy across Blocks')
  legend('bottomright', y = NULL, 1:n_cats, cex = 0.8, col = colors, 
    pch = plot_char, lty = line_type, title = 'SHJ Categories')

  # # # produce plot
  dev.off()

}

# response_rule
# convert output activations to classification
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
response_rule <- function(out_activation, target_activation, beta_val){
  num_feats <- ncol(out_activation)
  num_cats  <- dim(out_activation)[3]
  num_stims <- nrow(target_activation)
  if (is.null(num_stims)) {num_stims <- 1}

  # # # calc error  
  ssqerror <- array(as.vector(
    apply(out_activation, 3, function(x) {x - target_activation})),
      c(num_stims, num_feats, num_cats))
  ssqerror <- ssqerror ^ 2
  ssqerror[ssqerror < 1e-7] <- 1e-7

  # # # if focusing is on:
  if (beta_val > 0) {
    # # # get list of channel comparisons
    pairwise_comps <- combn(1:num_cats, 2)
    
    # # # get differences for each feature between categories
    diff_matrix <- 
      abs(apply(pairwise_comps, 2, function(x) {
        out_activation[,,x[1]] - out_activation[,,x[2]]}))

    # # # reconstruct activation array and get feature diversity means
    diff_array <- array(diff_matrix, dim = c(num_stims, num_feats, num_cats))
    feature_diffs <- apply(diff_array, 2, mean)

    # # # calculate diversities
    diversities <- exp(beta_val * feature_diffs)
    diversities[diversities > 1e+7] <- 1e+7

    # # # divide diversities by sum of diversities
    fweights = diversities / sum(diversities)

    # # # apply focus weights; then get sum for each category
    ssqerror <- t(apply(ssqerror, 3, function(x) sum(x * fweights))) 

  # # # otherwise, set focus weights to NULL
  } else {
    fweights <- NULL
    # # # sum error within channel
    ssqerror <- apply(ssqerror, 3,function(x) sum(x))     
  }

  # # # calculate inverse sse
  ssqerror <- 1 / ssqerror

return(list(ps = ssqerror / sum(ssqerror),
            fweights = fweights, 
            ssqerror = ssqerror))

}


# sigmoid
# returns sigmoid evaluated elementwize in X
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
sigmoid <- function(x) {
  g = 1 / (1 + exp(-x))

return(g)

}

# sigmoid gradient
# returns the gradient of the sigmoid function evaluated at x
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
sigmoid_grad <- function(x) {
  
return(g = ((sigmoid(x)) * (1 - sigmoid(x))))

}
