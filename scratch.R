library(plyr)

if (!exists('fitts')) { # Lazily fetch data
  fitts <- read.csv('http://fitts-law-experiment.herokuapp.com/data.csv')
}

# Clean tablet data
fitts <- subset(fitts, pointer != 'tablet')

# Get only the normal data
fitts.normal <- ddply(fitts, .(subject), .fun = function(df) {
  normal <- shapiro.test(df$time)
  
  if (normal$p.value > 0.05) {
    return(df)
  }
  
  return(NULL)
})

shapiro.test(fitts.normal$time)