# Collection of R functions.

# Scales data to have range from 0 to 1.
# Author: Mario A. Gutierrez (mag262@txstate.edu)
# Date: 08/23/2015   Version: 1.00
#	 x - the data to scale.
# Returns: the scaled data object.
rescale_01 <- function(x) {
  minx <- min(x)
  result <- (x - minx) / (max(x) - minx)
  return (result)
}

# Draws a scatterplot with a curve over it, and a line plot in the background.
# Author: Mario A. Gutierrez (mag262@txstate.edu)
# Date: 08/23/2015   Version: 1.02
#	 data - is the data.frame with the data
#	 back - the data for the line plot in the back.
#	 front - the data for the scatterplot in the front.
#	 use_linear - fit a line to the 'front' data if TRUE, fits a curve otherwise
# 		(default: TRUE).
# Returns: the resulting ggplot2 object.
fadoodle <- function(data, bac, front, front.label="", use_linear=TRUE) {
	require('ggplot2')
	
	ifelse(front.label == "", front.label <- toupper(front), front.label <- toupper(front.label))
	ylab <- paste("BAC vs.", front.label)
	
	result <- ggplot(data, aes_string(x = "timestamp", y = bac)) +
		geom_line() +
		geom_point(aes_string(x = "timestamp", y = front), color = "red", size = 1.5) +
		theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) +
		ylab(ylab) +
		xlab("TIME")
		
	if (use_linear == FALSE) {
			result <- result + geom_smooth(aes_string(x = "timestamp", y = front))
	} else {
		result <- result + geom_smooth(aes_string(x = "timestamp", y = front), method="lm")
	}
	
	return(result)
}


