po <- read.csv("~/Dropbox/FSU/OCP5930/Fortran/points.csv", header=TRUE,sep=",")
delta <- 0.2

setwd('~/Dropbox/FSU/OCP5930/Fortran/IMG')

frames = 10000

for(i in 0:frames){
  if((i %% 10 == 0) || (i/frames < 0.01)){
    # creating a name for each plot file with leading zeros
    if (i < 10) {name = paste('0000',i,'plot.png',sep='')}
    if (i < 100 && i >= 10) {name = paste('000',i,'plot.png', sep='')}
    if (i<1000 && i >= 100) {name = paste('00', i,'plot.png', sep='')}
    if (i<10000 && i >= 1000) {name = paste('0', i,'plot.png', sep='')}
    if (i<100000 && i >= 10000) {name = paste('', i,'plot.png', sep='')}
  
  #saves the plot as a .png file in the working directory
    png(name)
    l <- subset(po, po$t < delta*(i+0.5) & po$t > delta*(i-0.5)) 
  
    plot(l$X, l$Y, type='l', xlim = c(-1,1), ylim = c(0,2), ylab ='Y Value', xlab='X Value',  main = paste('Signal at timestep ', i), col = 'red')
    dev.off()
  }
}

