### make contour plot
### grid: output of make_grid
### par: list of all model parameters

make_contour = function(grid,par) {
  
  theta = par_2_theta(par)
  
  true = data.frame(x = theta[[grid$flex$par[1]]][grid$flex$index[1]],
                    y = theta[[grid$flex$par[2]]][grid$flex$index[2]])
  
  plot = ggplot() +
    theme_minimal() +
    geom_contour_filled(data = grid$values, aes(x=x,y=y,z=z,fill=..level..)) +
    geom_point(data = true, aes(x=x,y=y)) +
    geom_text(data = true, aes(x=x,y=y), label="true", vjust=1) +
    labs(fill="log-likelihood",
         x=bquote(.(grid$flex$par[1])[.(grid$flex$index[1],)]),
         y=bquote(.(grid$flex$par[2])[.(grid$flex$index[2],)]))
  
  
  return(plot)
}