quartz(family="Avenir LT Std",width=10,height=6)
x = 1:10
y = c(1,3,2,3,4,2,1,1,1,2)
ny =c(-1,1/3,0,1/3,2/3,0,-1,-1,-1,0)
plot(x, y, xlab="", ylab="", xlim=c(1,10), ylim=c(1,5), main="",  
bg=c(rgb(0,0.25,0.5),rgb(0,0.25,0.5),rgb(0,0.25,0.5),rgb(0,0.25,0.5),rgb(1,1,1),rgb(0,0.25,0.5),rgb(0,0.25,0.5),rgb(0,0.25,0.5),rgb(0,0.25,0.5),rgb(0,0.25,0.5)), 
col=rgb(0,0,0.5),lty="dotted", lwd=1.8,font=3,cex=c(2,2,2,2,2.5,2,2,2,2,2),cex.main=1.5,cex.lab=1.3,
pch=c(22,22,22,22,21,22,22,22,22,22),
mgp=c(2.4,1,0), type="o",xaxp=c(1,10,9))
segments(0,2,11,2,lty="dashed",lwd=0.3)
quartz.save("plot_usage_behaviour_u.pdf",type="pdf")

plot(x, ny, xlab="", ylab="", xlim=c(1,10), ylim=c(-1,1), main="",  
bg=c(rgb(0,0.25,0.5),rgb(0,0.25,0.5),rgb(0,0.25,0.5),rgb(0,0.25,0.5),rgb(1,1,1),rgb(0,0.25,0.5),rgb(0,0.25,0.5),rgb(0,0.25,0.5),rgb(0,0.25,0.5),rgb(0,0.25,0.5)), 
col=rgb(0,0,0.5),lty="solid", lwd=1.8,font=3,cex=c(2,2,2,2,2.5,2,2,2,2,2),cex.main=1.5,cex.lab=1.3,
pch=c(22,22,22,22,21,22,22,22,22,22),
mgp=c(2.4,1,0), type="o",xaxp=c(1,10,9))
segments(0,0,11,0)
quartz.save("plot_normalised_behaviour_u.pdf",type="pdf")
 
z = c(1,5,5,3,4,2,5,5,1,4)
nz =c(-1,1,1,-1/3,0,-2/3,1,1,-1,0)
plot(x, z, xlab="", ylab="", xlim=c(1,10), ylim=c(1,5), main="",  
bg=c(rgb(0.75,0,0.25),rgb(0.75,0,0.25),rgb(0.75,0,0.25),rgb(0.75,0,0.25),rgb(1,1,1),rgb(0.75,0,0.25),rgb(0.75,0,0.25),rgb(0.75,0,0.25),rgb(0.75,0,0.25),rgb(0.75,0,0.25)), 
col=rgb(0.5,0,0.25),lty="dotted", lwd=1.8,font=3,cex=c(2,2,2,2,2.5,2,2,2,2,2),cex.main=1.5,cex.lab=1.3,
pch=c(22,22,22,22,21,22,22,22,22,22),
mgp=c(2.4,1,0), type="o",xaxp=c(1,10,9))
segments(0,4,11,4,lty="dashed",lwd=0.3)
quartz.save("plot_usage_behaviour_v.pdf",type="pdf")

plot(x, nz, xlab="", ylab="", xlim=c(1,10), ylim=c(-1,1), main="",  
bg=c(rgb(0.75,0,0.25),rgb(0.75,0,0.25),rgb(0.75,0,0.25),rgb(0.75,0,0.25),rgb(1,1,1),rgb(0.75,0,0.25),rgb(0.75,0,0.25),rgb(0.75,0,0.25),rgb(0.75,0,0.25),rgb(0.75,0,0.25)), 
col=rgb(0.5,0,0.25),lty="solid", lwd=1.8,font=3,cex=c(2,2,2,2,2.5,2,2,2,2,2),cex.main=1.5,cex.lab=1.3,
pch=c(22,22,22,22,21,22,22,22,22,22),
mgp=c(2.4,1,0), type="o",xaxp=c(1,10,9))
segments(0,0,11,0)
quartz.save("plot_normalised_behaviour_v.pdf",type="pdf")

# Plot for normalisation
y <- function(avg) (log((x*(min(x)+max(x)-2*avg)+avg*avg-min(x)*max(x))/((max(x)-avg)*(avg-min(x))))/log((max(x)-avg)/(avg-min(x))))
x = seq(1.0,5.0,by=0.1)
# x = min
quartz(family="Avenir LT Std",width=10,height=6)
plot(x, rep(-1,length(x)), xlim=c(min(x)-0.05*(max(x)+min(x)),max(x)+0.05*(max(x)+min(x))), ylim=c(-1-0.1,1+0.1), xlab="", ylab="", col=rgb(0,0,0.5),  pch=21, lwd=1.5,font=3,cex=1.3,cex.main=1.5,cex.lab=1.3, type="l",lty="dotted", yaxp=c(-1,1,2), xaxp=c(min(x),max(x),2));
    par(new=TRUE);
# x = max
plot(x, rep(1,length(x)), xlim=c(min(x)-0.05*(max(x)+min(x)),max(x)+0.05*(max(x)+min(x))), ylim=c(-1-0.1,1+0.1), xlab="", ylab="", col=rgb(0,0,0.5),  pch=21, lwd=1.5,font=3,cex=1.3,cex.main=1.5,cex.lab=1.3, type="l",lty="dotted", yaxp=c(-1,1,2), xaxp=c(1,5,1), yaxt="n", xaxt="n", bty="n")
par(new=TRUE);
# x = avg
plot(x, ((x-min(x)) - (max(x)-x))/(max(x)-min(x)), xlim=c(min(x)-0.05*(max(x)+min(x)),max(x)+0.05*(max(x)+min(x))), ylim=c(-1-0.1,1+0.1), xlab="", ylab="", col=rgb(0,0.25,0),  pch=21, lwd=1.5,font=3,cex=1.3,cex.main=1.5,cex.lab=1.3, type="l",lty="dashed", yaxp=c(-1,1,2), xaxp=c(1,5,1), yaxt="n", xaxt="n", bty="n")
par(new=TRUE);
# x < avg
plot(x, y(1.6), xlim=c(min(x)-0.05*(max(x)+min(x)),max(x)+0.05*(max(x)+min(x))), ylim=c(-1-0.1,1+0.1), xlab="", ylab="", col=rgb(0.5,0,0),  pch=21, lwd=1.5,font=3,cex=1.3,cex.main=1,cex.lab=1.3, type="l",lty="solid", yaxp=c(-1,1,2), xaxp=c(1,5,1), yaxt="n", xaxt="n", bty="n")
par(new=TRUE);
# x > avg
plot(x, y(4.3), xlim=c(min(x)-0.05*(max(x)+min(x)),max(x)+0.05*(max(x)+min(x))), ylim=c(-1-0.1,1+0.1), xlab="", ylab="", col=rgb(0.5,0,0),  pch=21, lwd=1.5,font=3,cex=1.3,cex.main=1,cex.lab=1.3, type="l",lty="solid", yaxp=c(-1,1,2), xaxp=c(1,5,1), yaxt="n", xaxt="n", bty="n")
par(new=TRUE);
segments(0,0,6,0,lwd=1)
quartz.save("normalised_usage_behaviour.pdf",type="pdf")

# Plot to show how individual preference affects individual satisfaction
runif(1:1000)
save.seed <- .Random.seed
h1 <- function(t, alpha=0.9) (alpha*t) %% 2 - 1 # or whatever rnd function
h2 <- function(t) {set.seed(save.seed, kind = "default"); (runif(1:1000)*2 - 1)[t+1]}
h  <- function(t) {c(-0.39099219, -0.54975863,  0.64780866, -0.69143442, -0.47034046, 0.40887579, 0.26055271, -0.75978313, -0.64293005, 0.71700292,  0.07379019, -0.09667739, -0.51370321, 0.29128101, 0.96727560,  0, -0.02900041, -0.08757081, -0.01967315, -0.13351452, -0.13674167,  -0.79307541, -0.23672249, -0.08238743, -0.73240963)[t]}

q <- function(t,chi=0.5) { sum((chi**(0:t))*h(t:0))/sum(chi**(0:t)) }

# to print
quartz(family="Avenir LT Std",width=10,height=7)
plot(1:25,h(1:25),  ylim=c(-1,1),xlim=c(1,25),type="l",bg=rgb(0.5,0,0.25), col=rgb(0.5,0,0),lty="solid", pch=21, lwd=0.8,font=3,cex=1,cex.main=1.5,cex.lab=1.3, xlab="",ylab="")
par(new=TRUE); segments(0,0,30,0,lwd=1)
par(new=TRUE); plot(1:25,h(1:25)       ,ylim=c(-1,1),xlim=c(1,25),type="p",bg=rgb(0.5,0,0.25), col=rgb(0.5,0,0),lty="solid", pch=21, lwd=1,font=3,cex=1,cex.main=1.5,cex.lab=1.3, xlab="",ylab="", yaxt="n", xaxt="n", bty="n")
par(new=TRUE); plot(c(17,18),c(0.9,0.9),ylim=c(-1,1),xlim=c(1,25),type="l",bg=rgb(0.5,0,0.25), col=rgb(0.5,0,0),lty="solid", pch=21, lwd=0.8,font=3,cex=1,cex.main=1.5,cex.lab=1.3, xlab="",ylab="", yaxt="n", xaxt="n", bty="n")
par(new=TRUE); plot(c(17,18),c(0.9,0.9),ylim=c(-1,1),xlim=c(1,25),type="p",bg=rgb(0.5,0,0.25), col=rgb(0.5,0,0),lty="solid", pch=21, lwd=1,font=3,cex=1,cex.main=1.5,cex.lab=1.3, xlab="",ylab="", yaxt="n", xaxt="n", bty="n")

par(new=TRUE); plot(1:25,sapply(1:25,function(x) q(x,chi=0.2)),      ylim=c(-1,1),xlim=c(1,25),type="l",bg=rgb(0,0.25,0.5), col=rgb(0,0,0.5),lty="dotted", pch=22, lwd=1.8,font=3,cex=1,cex.main=1.5,cex.lab=1.3, xlab="",ylab="", yaxt="n", xaxt="n", bty="n")
par(new=TRUE); plot(1:25,sapply(1:25,function(x) q(x,chi=0.2))      ,ylim=c(-1,1),xlim=c(1,25),type="p",bg=rgb(0,0.25,0.5), col=rgb(0,0,0.5),lty="dotted", pch=22, lwd=1,font=3,cex=1,cex.main=1.5,cex.lab=1.3, xlab="",ylab="", yaxt="n", xaxt="n", bty="n")
par(new=TRUE); plot(c(17,18),c(0.7,0.7)                           ,ylim=c(-1,1),xlim=c(1,25),type="l",bg=rgb(0,0.25,0.5), col=rgb(0,0,0.5),lty="dotted", pch=22, lwd=1.8,font=3,cex=1,cex.main=1.5,cex.lab=1.3, xlab="",ylab="", yaxt="n", xaxt="n", bty="n")
par(new=TRUE); plot(c(17,18),c(0.7,0.7)                           ,ylim=c(-1,1),xlim=c(1,25),type="p",bg=rgb(0,0.25,0.5), col=rgb(0,0,0.5),lty="dotted", pch=22, lwd=1,font=3,cex=1,cex.main=1.5,cex.lab=1.3, xlab="",ylab="", yaxt="n", xaxt="n", bty="n")

par(new=TRUE); plot(1:25,sapply(1:25,function(x) q(x,chi=0.8)),       ylim=c(-1,1),xlim=c(1,25),type="l",bg=rgb(0,0.5,0), col=rgb(0,0.25,0),lty="dashed", pch=23, lwd=1.8,font=3,cex=1,cex.main=1.5,cex.lab=1.3, xlab="",ylab="", yaxt="n", xaxt="n", bty="n")
par(new=TRUE); plot(1:25,sapply(1:25,function(x) q(x,chi=0.8))       ,ylim=c(-1,1),xlim=c(1,25),type="p",bg=rgb(0,0.5,0), col=rgb(0,0.25,0),lty="dashed", pch=23, lwd=1,font=3,cex=1,cex.main=1.5,cex.lab=1.3, xlab="",ylab="", yaxt="n", xaxt="n", bty="n")
par(new=TRUE); plot(c(17,18),c(0.5,0.5)                              ,ylim=c(-1,1),xlim=c(1,25),type="l",bg=rgb(0,0.5,0), col=rgb(0,0.25,0),lty="dashed", pch=23, lwd=1.8,font=3,cex=1,cex.main=1.5,cex.lab=1.3, xlab="",ylab="", yaxt="n", xaxt="n", bty="n")
par(new=TRUE); plot(c(17,18),c(0.5,0.5)                              ,ylim=c(-1,1),xlim=c(1,25),type="p",bg=rgb(0,0.5,0), col=rgb(0,0.25,0),lty="dashed", pch=23, lwd=1,font=3,cex=1,cex.main=1.5,cex.lab=1.3, xlab="",ylab="", yaxt="n", xaxt="n", bty="n")
quartz.save("satisfaction_decay.pdf",type="pdf")

# now add the legend with a point-line-point and the space to write the function


# New Plot for normalisation
y <- function(avg) (log((x*(min(x)+max(x)-2*avg)+avg*avg-min(x)*max(x))/((max(x)-avg)*(avg-min(x))))/log((max(x)-avg)/(avg-min(x))))
x = seq(1.0,5.0,by=0.1)
quartz(family="Avenir LT Std",width=10,height=5)
# x = avg
plot(x, ((x-min(x)) - (max(x)-x))/(max(x)-min(x)), xlim=c(min(x)-0.05*(max(x)+min(x)),max(x)+0.05*(max(x)+min(x))), ylim=c(-1-0.1,1+0.1), xlab="", ylab="", col=rgb(0,0.25,0),  pch=21, lwd=1.8,font=3,cex=1.3,cex.main=1.5,cex.lab=1.3, type="l",lty="dashed", yaxp=c(-1,1,2), xaxp=c(1,5,1), yaxt="n", xaxt="n", bty="n")
par(new=TRUE);
# x < avg
plot(x, y(1.6), xlim=c(min(x)-0.05*(max(x)+min(x)),max(x)+0.05*(max(x)+min(x))), ylim=c(-1-0.1,1+0.1), xlab="", ylab="", col=rgb(0.5,0,0),  pch=21, lwd=1.8,font=3,cex=1.3,cex.main=1,cex.lab=1.3, type="l",lty="dotted", yaxp=c(-1,1,2), xaxp=c(1,5,1), yaxt="n", xaxt="n", bty="n")
par(new=TRUE);
# x > avg
plot(x, y(4.3), xlim=c(min(x)-0.05*(max(x)+min(x)),max(x)+0.05*(max(x)+min(x))), ylim=c(-1-0.1,1+0.1), xlab="", ylab="", col=rgb(0.5,0,0),  pch=21, lwd=1.8,font=3,cex=1.3,cex.main=1,cex.lab=1.3, type="l",lty="dotted", yaxp=c(-1,1,2), xaxp=c(1,5,1), yaxt="n", xaxt="n", bty="n")
par(new=TRUE);
segments(0,0,6,0,lwd=1)
# x = med
par(new=TRUE);
plot(x, rep(0,length(x)), xlim=c(min(x)-0.05*(max(x)+min(x)),max(x)+0.05*(max(x)+min(x))), ylim=c(-1-0.1,1+0.1), xlab="", ylab="", col=rgb(0,0,0.5),  pch=21, lwd=1.8,font=3,cex=1.3,cex.main=1.5,cex.lab=1.3, type="l",lty="solid", yaxp=c(-1,1,2), xaxp=c(min(x),max(x),2));
quartz.save("normalised_usage_behaviour_2.pdf",type="pdf")


######### Evaluation about scalability ##################

setwd("/Users/claudiob/Coding/R/scalability")
# Changing the number of users
max_size = 16
for (size in 1:max_size) {
    data <- read.table(paste(size, ".txt", sep=""))
    # quartz family
    xmin = 1  # min(data$V2)
    xmax = 40 # max(data$V2) 
    quartz(family="Avenir LT Std",width=10,height=6)
    plot(data$V2[data$V1 == 1], ylim=c(0,1), xlim=c(xmin, xmax), main=paste("Satisfaction degrees for a channel with", size, "random listeners"), type="n",xlab="Songs",ylab="Degree", font=3,cex=1.3,cex.main=1.5,cex.lab=1.3, mgp=c(2.4,1,0))
    for (i in 1:size) {
    	values = data[data$V1 == i,]
    	par(new=TRUE)
    	plot(values$V2, values$V3, ylim=c(0,1), xlim=c(xmin, xmax), col=rainbow(16)[i], lwd=0.5, lty=22, type="l", xlab="",  ylab="", xaxt="n",  yaxt="n", axes=FALSE, font=3,cex=1.3,cex.main=1.5) 
    	par(new=TRUE)
    	plot(values$V2, values$V4, ylim=c(0,1), xlim=c(xmin, xmax), col=rainbow(16)[i], lwd=3, type="l", xlab="",  ylab="", xaxt="n",  yaxt="n", axes=FALSE, font=3,cex=1.3,cex.main=1.5)	
    }
    quartz.save(paste("scalability", size, "users.png", sep="_"),type="png")    
}

# Changing the retrieval size
setwd("/Users/claudiob/Coding/R/scalability")
max_size = 25
for (size in 1:max_size) {
    data <- read.table(paste("5_", size, ".txt", sep=""))
    # quartz family
    xmin = 1  # min(data$V2)
    xmax = 40 # max(data$V2) 
    quartz(family="Avenir LT Std",width=10,height=6)
    plot(data$V2[data$V1 == 1], ylim=c(0,1), xlim=c(xmin, xmax), main=paste("Satisfaction degrees for a channel with retrieval size = ", size), type="n",xlab="Songs",ylab="Degree (5 random listeners)", font=3,cex=1.3,cex.main=1.5,cex.lab=1.3, mgp=c(2.4,1,0))
    for (i in 1:5) {
    	values = data[data$V1 == i,]
    	par(new=TRUE)
    	plot(values$V2, values$V3, ylim=c(0,1), xlim=c(xmin, xmax), col=rainbow(16)[i], lwd=0.5, lty=22, type="l", xlab="",  ylab="", xaxt="n",  yaxt="n", axes=FALSE, font=3,cex=1.3,cex.main=1.5) 
    	par(new=TRUE)
    	plot(values$V2, values$V4, ylim=c(0,1), xlim=c(xmin, xmax), col=rainbow(16)[i], lwd=3, type="l", xlab="",  ylab="", xaxt="n",  yaxt="n", axes=FALSE, font=3,cex=1.3,cex.main=1.5)	
    }
    quartz.save(paste("scalability", size, "retrieval.png", sep="_"),type="png")    
}

# Changing the misery
setwd("/Users/claudiob/Coding/R/scalability")
for (misery in seq(-1.0,1.0,by=0.1)) {
    size = 5
    retrieval = 15
    data <- read.table(paste(size, "_", retrieval, "_", misery, ".txt", sep=""))
    # quartz family
    xmin = 1  # min(data$V2)
    xmax = 40 # max(data$V2) 
    quartz(family="Avenir LT Std",width=10,height=6)
    plot(data$V2[data$V1 == 1], ylim=c(-1,1), xlim=c(xmin, xmax), main=paste("Group preference for 5 listeners, 15 candidates and misery = ", misery), type="n",xlab="Songs",ylab="Group Preference", font=3,cex=1.3,cex.main=1.5,cex.lab=1.3, mgp=c(2.4,1,0))
	par(new=TRUE)
    if(any(data$V2 < misery)) boxplot(V2 ~ V1, data = data, range=0, na.action = na.pass, ylim=c(-1,1), xlim=c(xmin, xmax), col=rainbow(16)[1], type="l", xlab="",  ylab="", xaxt="n",  yaxt="n", axes=FALSE, font=3,cex=1.3,cex.main=1.5, subset = data$V2 < misery, at = unique(data$V1[data$V2 < misery]))
    data$V2[data$V2 == -1] = NA
    boxplot(V2 ~ V1, data = data, range=0, na.action = na.pass, ylim=c(-1,1), xlim=c(xmin, xmax), col=rainbow(16)[1], type="l", xlab="",  ylab="", xaxt="n",  yaxt="n", axes=FALSE, font=3,cex=1.3,cex.main=1.5, add=TRUE)
	par(new=TRUE)
    plot(c(-5,45),c(misery,misery), xlim=c(xmin, xmax), ylim=c(-1,1), lwd=0.5, lty=22, type="l", xlab="",  ylab="", xaxt="n",  yaxt="n", axes=FALSE, font=3,cex=1.3,cex.main=1.5)
    quartz.save(paste("scalability", size, misery, "misery.png", sep="_"),type="png")    
}


### Distances between artists

library(affinity)
data(mystrandsGenres)
runExamples() # o carica cio' che serve

firstArtist = 1024# which(instances() == 1024) # Metallica
secondArtist = 862#which(instances() == 862) # Bjork
usetable = centralities
step0 = usetable(firstArtist,)
step8 = usetable(secondArtist,)
step4 = apply(cbind(step0,step8),1,mean)
step2 = apply(cbind(step0,step4),1,mean)
step1 = apply(cbind(step0,step2),1,mean)
step3 = apply(cbind(step2,step4),1,mean)
step6 = apply(cbind(step4,step8),1,mean)
step5 = apply(cbind(step4,step6),1,mean)
step7 = apply(cbind(step6,step8),1,mean)

arts = array(dim=7)
tmp = as.matrix(dist(rbind(usetable(), step1)))
arts[1] = which(tmp[4001,] == min(tmp[4001,-4001]))[1]
tmp = as.matrix(dist(rbind(usetable(), step2)))
arts[2] = which(tmp[4001,] == min(tmp[4001,-4001]))[1]
tmp = as.matrix(dist(rbind(usetable(), step3)))
arts[3] = which(tmp[4001,] == min(tmp[4001,-4001]))[1]
tmp = as.matrix(dist(rbind(usetable(), step4)))
arts[4] = which(tmp[4001,] == min(tmp[4001,-4001]))[1]
tmp = as.matrix(dist(rbind(usetable(), step5)))
arts[5] = which(tmp[4001,] == min(tmp[4001,-4001]))[1]
tmp = as.matrix(dist(rbind(usetable(), step6)))
arts[6] = which(tmp[4001,] == min(tmp[4001,-4001]))[1]
tmp = as.matrix(dist(rbind(usetable(), step7)))
arts[7] = which(tmp[4001,] == min(tmp[4001,-4001]))[1]
print(instances(arts))



### Occurrences per song (from mysql..)

# 4. Occurrences again
quartz(family="Avenir LT Std",width=10,height=7.5,bg="white")

setwd("/Users/claudiob/Coding/Mysql/datamining/mystrands")
data <- read.table("mystrands_04_occurrences_per_song.txt")
plot(data,main="Occurrences per song",xlab="Number of occurrences in playlists",ylab="Number of playlists", col="red",pch=21,lwd=1.5,font=3,cex=1.3,cex.main=1.5,cex.lab=1.25,cex.axis=1.2, mgp=c(2.4,0.75,0), type="o",bg="pink",yaxp=c(0,240000,3))
quartz.save("mystrands_occurrences_per_song.pdf",type="pdf")
plot(data[2:20,],main="Occurrences per song",xlab="Number of occurrences in playlists [limited to 2~20]",ylab="Number of playlists", col="red",pch=21,lwd=1.5,font=3,cex=1.3,cex.main=1.5,cex.lab=1.25,cex.axis=1.2, mgp=c(2.4,0.75,0), type="o",bg="pink",yaxp=c(0,100000,4))
quartz.save("mystrands_occurrences_per_song_limited.pdf",type="pdf")
