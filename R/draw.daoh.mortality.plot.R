##' Draw a plot of DAOH with mortal cases highlighted.
##'
##' @title
##' @param input.dt
draw.daoh.mortality.plot <- function(input.dt) {

  
  y.scale.labels = function(x)
    sprintf("%0.2g%%", round(x * 100, digits = 5))
  ybreaks =  c(0,
               0.001,
               0.0025,
               0.005,
               0.01,
               0.02,
               0.03,
               0.04,
               0.05,
               0.075,
               0.1,
               0.15,
               0.20,
               0.25)
  xbreaks =  seq(0,120,by=15)
  mysqrt_trans <- function() {
    scales::trans_new(
      "mysqrt",
      transform = base::sqrt,
      inverse = function(x)
        ifelse(x < 0, 0, x ^ 2),
      domain = c(0, Inf)
    )
  }

  transformedYScale = scale_y_sqrt(
    labels = y.scale.labels,
    minor_breaks = NULL,
    breaks = ybreaks,
    expand = c(0.001, 0.001),
    limits = c(0, .25)
  )
  
  xScale = scale_x_continuous(breaks = xbreaks, 
                              expand = c(0.01, 0.1),
                              limits = c(-1, 91))
  
  # mort.time.series.figure.dt = copy(time.series.figure.dt)
  # mort.time.series.figure.dt[mort.90.day == FALSE, daoh := -10]
  
  # mortData = time.series.figure.dt[mort.90.day==TRUE,]
  # mortData[,mort := FALSE]
  # mortData = rbind(mortData,figureDaohData)
  # NrowOrig = nrow(figureDaohData)
  
  p = ggplot() +
    #Now there are false entries! Divide by original number of rows.
    geom_histogram(data = input.dt,
                   aes(x=daoh, 
                       y=c(..count..[..group..==1]/sum(..count..) +
                             ..count..[..group..==2]/sum(..count..),
                           ..count..[..group..==2]/sum(..count..)),
                       fill = mort.90.day),
                   alpha=.75,
                   position="identity",
                   binwidth=1) +
    gghighlight() + 
    # geom_histogram(data = mort.time.series.figure.dt,
    #                aes(x=daoh, 
    #                    y=..density..),
    #                alpha=.75,
    #                position="identity",
    #                bins=90) +
    # gghighlight(mort.90.day == TRUE) +
    # labs(title=paste("Frequency for Overall DAOH (both groups, n=", nGroup1+nGroup2, ")", sep="")) +
    labs(x=~"DAOH"[90], y = element_blank()) +
    # labs(x="Days alive and out of hospital", y="Percentage") +
    transformedYScale +
    xScale +
    # theme(plot.title = element_text(hjust = 0.5)) +
    # theme(text = element_text(size=figureTextSize)) +
    scale_fill_grey(start=0.45, end=0.1,
                    name="90-day",
                    breaks=c(0, 1),
                    labels=c("Alive", "Dead")) + 
    theme(legend.position = "none") 
    # guides(fill=guide_legend()) 

}
