#' @import ggplot2
#' @import gridExtra
#' @import data.table
NULL
#------------------------------------------------------------------------------

#Changes used when combining separate plots
# combined_graph_width  = 8.27;
# combined_graph_height = 11.69;
# title_size            = (combined_graph_width+combined_graph_height)/21*12 ;
# text_size             = (combined_graph_width+combined_graph_height)/21*8;
# label_size            = (combined_graph_width+combined_graph_height)/21*5;
# plot_ratio            = 1/2; #Height/Width ratio (when combining plots)

#------------------------------------------------------------------------------
# grab the legend of a graph
#' @export
GrabLegend<-function(a.gplot){
  tmp    <- ggplot_gtable(ggplot_build(a.gplot))
  leg    <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#------------------------------------------------------------------------------
#' Fix the margins of a graph
#'
#' Deprectaed because it does not work well
#'
#' @export
#' @param plot gg.plot object
#' @param plot_margin_x the length of the x margin
#' @param plot_margin_y the length of the y margin
#' @return a gg.plot with the specificied margin
FixMargins <- function(plot,  plot_margin_x = 1, plot_margin_y = 2, plot_ratio = 1/2) {
  gg_built     <- ggplot_build(plot)
  x_range      <- max(gg_built[[1]][[1]]$xmax, na.rm=TRUE) -
    min(gg_built[[1]][[1]]$xmax, na.rm=TRUE)
  y_range      <- max(gg_built[[1]][[1]]$ymax, na.rm=TRUE)
  base_ratio   <- x_range/y_range
  plot <- plot + coord_fixed(ratio=base_ratio*plot_ratio) +
    theme(axis.title.x = element_text(vjust = -1),
          plot.margin = unit(rep(c(plot_margin_y, plot_margin_x), 2)))
  return(plot)
}
#------------------------------------------------------------------------------
# Generate x axis of a histogram
#' @export
GenerateAxisX <- function(pdf_x,
                          x_full_range = 0,
                          max_x = NULL,
                          min_x = NULL,
                          x_quantile = 0.05,
                          include_0 = 1,
                          xtitle = NULL){
  if(x_full_range) return(scale_x_continuous(name = xtitle))

  if(is.null(max_x)|is.null(min_x)){
    max_x        = max(pdf_x, na.rm =TRUE)*x_full_range +
      (1-x_full_range)*quantile(pdf_x, 1 - x_quantile, na.rm = TRUE)
    min_x        = min(pdf_x, na.rm =TRUE)*x_full_range +
      (1-x_full_range)*quantile(pdf_x, x_quantile, na.rm = TRUE)
    max_x        = max((1-include_0)*max_x + 0.01,max_x)
    min_x        = min((1-include_0)*min_x - 0.01,min_x)
    cur_limit = c(min_x, max_x)

  } else {
    cur_limit = c(min_x, max_x)
  }
  cur_limit <- PrettyAxis(cur_limit)
  if(!is.null(xtitle)){
    scalex = scale_x_continuous(name = xtitle, limits = cur_limit)
  } else {
    scalex = scale_x_continuous(limits = cur_limit)
  }
  return(scalex)

}

#' Decide Bin Width and Axis Range
#'
#' Given a proposed x-axis range, expand it to make both end rational number
#'  also make binwidth interpretable
#'
#' @export
PrettyAxis <- function(range){
  if(any(is.na(range))) return(range)
  range_val <- range[[2]] - range[[1]]
  min_binwidth <- range_val/40
  max_binwidth <- range_val/20
  binwidth_list <- c(50, 25, 20, 10)
  while(max(binwidth_list) < max_binwidth){
    binwidth_list <- binwidth_list * 10
  }
  potential_candidate <- list()
  while(TRUE){
    for(try_width in binwidth_list){
      if((max_binwidth >= try_width) & (min_binwidth <= try_width)){
        potential_candidate <- c(potential_candidate, try_width)
      }
    }
    if(length(potential_candidate) > 0){
      break
    } else {
      binwidth_list <- binwidth_list * 0.1
    }
  }
  potential_candidate <- unlist(potential_candidate)
  select_min <- which.min(abs(range_val/potential_candidate - 30))
  binwidth <- potential_candidate[select_min]
  range_max <- ceiling(range[[2]]/binwidth) * binwidth
  range_min <- floor(range[[1]]/binwidth) * binwidth
  range <- c(range_min, range_max)
  attr(range, 'binwidth') <- binwidth
  return(range)
}


#' plot a histogram
#'
#' Thi function tries to generate a 'publishable' colored histogram.
#'
#' @export
#' @param dt the data.table to be plotted
#' @param x the value column to be plotted
#'
#' @return a ggplot
PrettyPlot <- function(dt,
                       x,
                       # font and text
                       xtitle         = 'title',
                       ytitle         = 'Frequency',
                       label_size     = 10,
                       # x axis options
                       scalex         = NULL,
                       x_full_range   = 0,
                       max_x          = NULL,
                       min_x          = NULL,
                       x_quantile     = 0.05,
                       include_0      = 1,
                       bins           = NULL,
                       # weight options
                       weight         = NULL,
                       # color by factors
                       color_legend   = NULL,
                       color_identity = 'stack',
                       hide_legend    = FALSE,
                       # table options
                       table          = FALSE,
                       grob           = FALSE
){

  if(is.null(scalex)){
    scalex <- GenerateAxisX(dt[, get(x)],
                            x_full_range = x_full_range,
                            max_x = max_x,
                            min_x = min_x,
                            include_0 = include_0,
                            x_quantile = x_quantile,
                            xtitle = xtitle)
  }
  x_range <- scalex$limits
  if(is.null(x_range)){
    if(x_full_range) x_quantile = 0
    x_range <- quantile(dt[, get(x)], c(x_quantile, 1- x_quantile), na.rm = TRUE)
  }
  binwidth <- attr(x_range, 'binwidth')
  if(is.null(binwidth)){
    bin_breaks <- seq(x_range[[1]], x_range[[2]], length.out = 31)
  } else {
    bin_breaks <- seq(x_range[[1]], x_range[[2]], by = binwidth)
  }
  dt <- dt[, c(x, weight, color_legend), with = FALSE]
  dt[, VALUE_TO_PLOT := get(x)]
  dt[VALUE_TO_PLOT == min(bin_breaks), VALUE_TO_PLOT := VALUE_TO_PLOT + 1e-5]
  dt[VALUE_TO_PLOT == max(bin_breaks), VALUE_TO_PLOT := VALUE_TO_PLOT - 1e-5]
  myplot <- ggplot(data = dt, aes_string(x = 'VALUE_TO_PLOT'))

  if(is.null(color_legend)){
    hist_aes <- geom_histogram(breaks = bin_breaks,
                               fill = 'lightsteelblue2',
                               color = 'grey60',
                               size = 0.1,
                               aes_string(weight = weight))
    hist_fill <- NULL
  } else {
    hist_aes <- geom_histogram(breaks = bin_breaks,
                               color = 'grey60',
                               size = 0.1,
                               aes_string(fill = color_legend,
                                          weight = weight))
    hist_fill <- scale_fill_manual(
      values=c("FALSE" = "grey90", "TRUE" = "lightsteelblue2"),
      labels = c("FALSE" = "No", "TRUE" = "Yes"),
      guide = guide_legend(title = ConvertName(color_legend)))
  }

  if(is.null(weight)){
    median_val <- dt[, median(get(x), na.rm = TRUE)]
  } else {
    median_val <- dt[, weightedMedian(get(x), get(weight), na.rm= TRUE)]
  }

  myplot <- myplot + hist_aes + hist_fill +
    # editted
    scale_x_continuous(name = scalex$name) + coord_cartesian(scalex$limits) +
    scale_y_continuous(name = ytitle) +
    geom_vline(xintercept = median_val, colour= "red") +
    theme(axis.line.x      = element_line(),
          axis.line.y      = element_line(),
          panel.background = element_blank(),
          panel.grid.minor = element_line(colour = "grey95"),
          panel.grid.major = element_line(colour = "grey90")
    )
  # myplot_build <- ggplot_build(myplot)
  # top_y        <- ggplot_build(myplot)$panel$ranges[[1]]$y.range[2]
  if(hide_legend){
    mylegend <- GrabLegend(myplot)
    myplot <- myplot + theme(legend.position="none")
  }


  hjust_direction <- ifelse(x_range[[2]] - median_val > median_val - x_range[[1]], 0, 1)

  myplot       <- myplot +   annotate('text',
                                      x     = median_val,
                                      y     = Inf,
                                      vjust = 1,
                                      label = round(median_val, 3),
                                      hjust = hjust_direction)
  if(grob){
    myplot <- ggplotGrob(myplot)
  }
  if(hide_legend) attr(myplot, 'hidden_legend') <- mylegend
  return(myplot)
}

#------------------------------------------------------------------------------

#' Arrange a list of plots
#'
#' This function takes in a list of graphs and split them into a list of graph
#' lists. Each list of graphs is meant to be printed on one pdf pages.
#'
#' @export
#' @param graphs a list of ggplot.
#' @param number of columns on each page of the pdf.
#' @param number of rows on each page of the pdf.
#' @return a list of printable graph lists
ArrangeGraphs <- function(graphs, ncol = 1, nrow = 3){
  graph_ids <- 1:length(graphs)
  page_ids <- (graph_ids - 1)%/%(ncol * nrow) + 1
  graphs_by_page <- list()
  for(cur_page in page_ids){
    graphs_by_page[[cur_page]] <- do.call("arrangeGrob", list(
                                          grobs = graphs[page_ids == cur_page],
                                          ncol = ncol,
                                          nrow = nrow))
  }
  return(graphs_by_page)
}
