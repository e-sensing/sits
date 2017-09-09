plotClassification = function(x, timeseries.labels=NULL, patterns.labels=NULL, attr, ...){
  
  if(length(timeseries.labels)>6) timeseries.labels = timeseries.labels[1:6]
  x = subset(x, timeseries.labels, patterns.labels)
  if(length(list(...))>0) x = twdtwClassify(x, ...)
  
  ## Get data
  if(missing(attr)) attr = names(getTimeSeries(x,1)[[1]])
  df.x = do.call("rbind", lapply(as.list(x), function(xx){
    ts = getTimeSeries(xx)[[1]][,attr,drop=FALSE]
    data.frame(Time=index(ts), ts, Series=labels(xx)$timeseries)
  }))
  df.x = melt(df.x, id.vars=c("Time","Series"))
  
  y.labels = pretty_breaks()(range(df.x$value, na.rm = TRUE))
  y.breaks = y.labels
  
  df.pol = do.call("rbind", lapply(as.list(x), function(xx){
    best_class = xx[[1]]
    df.pol = do.call("rbind", lapply(1:nrow(best_class), function(i){
        data.frame(
          Time = c(best_class$from[i], best_class$to[i], best_class$to[i], best_class$from[i]),
          Group = rep(i, 4),
          Class = rep(as.character(best_class$label[i]), 4),
          value = rep(range(y.breaks, na.rm = TRUE), each=2))
    }))
    df.pol$Group = factor(df.pol$Group)
    df.pol$Class = factor(df.pol$Class)
    df.pol$Series = rep(as.character(labels(xx)$timeseries), length(df.pol$Time))
    df.pol
  }))
    
  I = min(df.pol$Time, na.rm = TRUE)-30 <= df.x$Time & 
    df.x$Time <= max(df.pol$Time, na.rm = TRUE)+30
  
  df.x = df.x[I,,drop=FALSE]
  
  gp = ggplot() +
    facet_wrap(~Series, scales = "free_x", ncol=1) + 
    geom_polygon(data=df.pol, aes_string(x='Time', y='value', group='Group', fill='Class'), alpha=.7) +
    scale_fill_brewer(palette="Set3") + 
    geom_line(data=df.x, aes_string(x='Time', y='value', colour='variable')) +
    scale_y_continuous(expand = c(0, 0), breaks=y.breaks, labels=y.labels) +
    scale_x_date(breaks=waiver(), labels=waiver()) +
    theme(legend.position = "bottom") + 
    guides(colour = guide_legend(title = "Bands")) + 
    ylab("Value") + 
    xlab("Time")
  gp
}
