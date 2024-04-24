plot_matrix <- function(horizon, name_vector, results){
  no_years<-abs(horizon[1])+horizon[2]+1
  no_models<-length(name_vector)
  estimators<-c(rep(c(name_vector), each=no_years))
  term<-c(rep(seq(horizon[1],horizon[2]), no_models))
  
  
  estimate<-vector(mode = "numeric", length = no_models* no_years)
  for (i in 1:no_models){
    estimate[(1+(i-1)*no_years):(i*no_years)]<- results[[i]][,1]
  }
  
  std.error<-vector(mode = "numeric", length = no_models* no_years)
  for (j in 1:no_models){
    std.error[(1+(j-1)*no_years):(i*no_years)]<- results[[j]][,2]
  }
  
  out1<-bind_cols(estimators, term, estimate, std.error)
  
  out1<-as.data.frame(out1) 
  colnames(out1)<- c("estimator", "term","estimate","std.error")
  out1$std.error <- as.numeric(out1$std.error) 
  out1$estimate<- as.numeric(out1$estimate) 
  out1$term<- as.numeric(out1$term)
  out1
}

event_plotter<-function (out,names, separate = FALSE, horizon = NULL, add_y_limit = NULL, color_pairs = TRUE) 
{
  no_models<- length(names)
  estimators = unique(out$estimator)
  levels = names
  levels = levels[levels %in% estimators]
  out$estimator = factor(out$estimator, levels = levels)
  
  if (!color_pairs)
  {colors <- c("#374E55", "#DF8F44", "#00A1D5", "#AE123A", "#79AF97","#6A6599","#9F3632", "#FF00CC" )}
  if (color_pairs)
  {colors <- c( "#B9DDF1","#2A5783","#FFC685","#EA6D20","#B3E0A6","#24693D","#FFBEB2","#AE123A") } 
  color_scale <- list()
  
  for (i in 1:length(names)) {
    color_scale[[names[i]]] <- colors[i]
  }
  
  color_scale = color_scale[names(color_scale) %in% estimators]
  out$ci_lower = out$estimate - 1.96 * out$std.error
  out$ci_upper = out$estimate + 1.96 * out$std.error
  
  position = ggplot2::position_dodge(width = 0.5) #distance between different estimators
  if (!is.null(horizon)) {
    out = out[out$term >= horizon[1] & out$term <= horizon[2], 
    ]
  }
  y_lims = c(min(out$ci_lower), max(out$ci_upper)) * 1.05
  x_lims = c(min(out$term) - 1, max(out$term) + 1)
  ggplot2::ggplot(data = out, mapping = ggplot2::aes(x = .data$term, 
                                                     y = .data$estimate, color = .data$estimator, ymin = .data$ci_lower, 
                                                     ymax = .data$ci_upper)) + {
                                                       if (separate) 
                                                         ggplot2::facet_wrap(~estimator, scales = "free")
                                                     } + ggplot2::geom_point(position = position) + ggplot2::geom_pointrange(position = position) + 
    ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed") + 
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") + 
    ggplot2::labs(y = "Point Estimate and 95% Confidence Interval", 
                  x = "Event Time", color = "Estimator") + {
                    if (separate) 
                      ggplot2::scale_y_continuous(limits = y_lims)
                  } + {
                    if (separate) 
                      ggplot2::scale_x_continuous(limits = x_lims)
                  } + ggplot2::theme_minimal(base_size = 16) + ggplot2::scale_color_manual(values = color_scale) + 
    ggplot2::guides(color = ggplot2::guide_legend(title.position = "top", 
                                                  nrow = 2)) + ggplot2::theme(legend.position = "bottom")+
    if (!is.null(add_y_limit)) {ggplot2::ylim(add_y[1],add_y[2])}
}
