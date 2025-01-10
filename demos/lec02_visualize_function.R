
visualize_chain_and_compute_estimates_and_cr = function(
    samples, 
    plot_mmse = TRUE, 
    plot_mmae = TRUE, 
    true_value = NULL, 
    alpha = NULL, 
    bins = 30,
    colors = c("blue", "orange", "green", "red")){
  ggplot_obj = ggplot(data.frame(samples = samples)) +
    geom_histogram(aes(x = samples), bins = bins)
  
  mmse = mean(samples)
  mmae = median(samples)
  if (plot_mmse){
    ggplot_obj = ggplot_obj + geom_vline(xintercept = mmse, col = colors[1])
  }
  
  if (plot_mmae){
    ggplot_obj = ggplot_obj + geom_vline(xintercept = mmae, col = colors[2])
  }
  
  if (!is.null(true_value)){
    ggplot_obj = ggplot_obj + 
      geom_vline(xintercept = true_value, col = colors[3]) 
  }
  if (!is.null(alpha)){
    ggplot_obj = ggplot_obj + 
      geom_vline(xintercept = quantile(samples, .025), col = colors[4]) + 
      geom_vline(xintercept = quantile(samples, .975), col = colors[4])
  }
  plot(ggplot_obj)
  
  ret = list(
    mmse = mmse,
    mmae = mmae,
    theta = true_value
  )
  if (!is.null(alpha)){
    ret$cr_one_minus_alpha_theta = c(
        quantile(samples, alpha / 2), 
        quantile(samples, 1 - alpha / 2)
      )
  }
  ret
}
