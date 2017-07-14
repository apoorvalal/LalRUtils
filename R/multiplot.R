#' Stitches together multiple ggplot objects for export-ready graphs
#' @param ggplot objects
#' @keywords graphs plots
#' @export
#' @examples
#' plot1 = ggplot(data,aes(xvar1,yvar)) + geom_point()
#' plot2 = ggplot(data,aes(xvar2,yvar)) + geom_point()
#' multiplot(plot1,plot2,cols=2)

#######################################################################
# Multiple plot function - stitches together multiple ggplot objects
#######################################################################
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
   library(grid)
   # Make a list from the ... arguments and plotlist
   plots <- c(list(...), plotlist)
   numPlots = length(plots)
   # If layout is NULL, then use 'cols' to determine layout
   if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
   }
   if (numPlots==1) {
      print(plots[[1]])
   } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
         # Get the i,j matrix positions of the regions that contain this subplot
         matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

         print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                         layout.pos.col = matchidx$col))
      }
   }
}
