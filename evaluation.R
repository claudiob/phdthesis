##############################################################################
#    Default values for parameters                                           #
##############################################################################

turns      = 25    # Length of the sequence of items to select
iterations = 20    # Number of times each experiment is repeated
PERSONS    = 5     # Number of persons in the group
RETRIEVAL  = 15    # Number of candidate items selected at each turn
WEIGHTED   = 1     # Whether past satisfaction affects next selections
CHI        = 0.8   # Measure by which past satisfaction affects satisfaction
IOTA       = 0.4   # Initial satisfaction of each person
MISERY     = -0.75 # Threshold of minimum preference acceptable
UNIFORM    = TRUE  # Whether preferences follow a uniform distribution
EXPERTISE  = 1     # Percentage of items for which persons have preferences
BUMP       = -1    # Factor of bump in a bimodal distribution of preferences
SAVE       = FALSE  # Whether to save plotted graphs to hard disk

##############################################################################
#    Generation of random individual preferences                             #
##############################################################################

generate <- function(persons=PERSONS, retrieval=RETRIEVAL, misery=MISERY, iota=IOTA, weighted=WEIGHTED, uniform=UNIFORM, expertise=EXPERTISE, bump=BUMP, ratings=NULL, chi=CHI) {

 combine_preferences <- function(current_preferences, past_satisfactions, misery, weighted)
  if (any(current_preferences < misery, na.rm=TRUE)) NA else 
  if(all(past_satisfactions == 1, na.rm=TRUE)) 0 else # rep(0, length(current_preferences)) else 
  sum((1-past_satisfactions)**weighted*current_preferences)/(sum((1-past_satisfactions)**weighted))
 
 combine_satisfactions <- function(pref, sat, step, chi=CHI)
  sat + (pref  - sat)/sum(chi**(0:step))
#  sat + (0.5*(pref + 1)-sat)/sum(chi**(0:step))
  
 create_preferences  <- function(rows, cols, uniform=UNIFORM, bump=BUMP, expertise=EXPERTISE) {
  pref = array(runif(rows*cols), dim=c(rows, cols))*(1-bump) + bump
  if(!uniform) {
   pref[(1:ceiling(rows/2)),(1:ceiling(cols/2))]   = -pref[(1:ceiling(rows/2)),(1:ceiling(cols/2))]
   pref[-(1:ceiling(rows/2)),-(1:ceiling(cols/2))] = -pref[-(1:ceiling(rows/2)),-(1:ceiling(cols/2))]
  }
  if(expertise < 1)
   for(i in 1:cols) pref[sample(1:rows)[1:round((1-expertise)*rows)],i] <- 0
  #pref
  (pref+1)/2   
 }

 preferences       <<- array(dim=c(persons, turns, iterations)) 
 satisfactions     <<- array(dim=c(persons, turns, iterations))
 group_preferences <<- array(dim=c(1, turns, iterations))
 
 for (iteration in 1:iterations) {
  for (song in 1:turns) {
   past_satisfactions <- if(song == 1) rep(iota, persons) else satisfactions[, song-1,iteration]
   preferences_matrix <- if(is.null(ratings)) create_preferences(retrieval, persons, uniform, bump, expertise) else array(ratings[song], dim=c(retrieval, persons))  
   group_pref_matrix  <- apply(preferences_matrix, 1, combine_preferences, past_satisfactions=past_satisfactions, misery=misery, weighted=weighted)
   if(all(is.na(group_pref_matrix))) { # If every preference is below misery
    # Take the candidate with the less worse misery
    less_worse = apply(preferences_matrix, 1, function(x) max(x[x < misery]))
    best_index <- which(less_worse == max(less_worse, na.rm = TRUE))[1]
    group_preferences[1, song, iteration] <<- mean(preferences_matrix[best_index,])
   }
   else {
    best_pref = max(group_pref_matrix, na.rm = TRUE)  
    best_index <- which(group_pref_matrix == best_pref)[1]
    group_preferences[1, song, iteration] <<- best_pref    
   }
   preferences[,song,iteration] <<- preferences_matrix[best_index,]
   satisfactions[,song,iteration] <<- combine_satisfactions(preferences_matrix[best_index,], past_satisfactions, iteration, chi)
  }
 }
}  

##############################################################################
#    Plotting functions                                                      #
##############################################################################

plot_values <- function(values, plot_box=FALSE, plot_title="", plot_save=SAVE, plot_add=FALSE, plot_axis=FALSE, plot_misery=FALSE, persons=PERSONS, retrieval=RETRIEVAL, misery=MISERY, iota=IOTA, weighted=WEIGHTED, uniform=UNIFORM,  expertise=EXPERTISE, bump=BUMP, chi=CHI, ylim=c(-1,1), plot_aggregate=NULL, ...) {
 means = apply(values, c(1,2), mean, na.rm=TRUE)
 if(!is.null(plot_aggregate))
    means = array(apply(means, 2, plot_aggregate, na.rm=TRUE), dim=c(1,ncol(means)))
 ylab = if(nrow(means) == 1) plot_title else paste(plot_title, "s", sep="")
 for (l in 1:nrow(means)) {
  #main = paste("Satisfaction degrees of 5 listeners with minimal initial satisfaction")
  #main = plot_main if plot_main != "" else create_title(ylab, plot_box, which = if(nrow(means) > 1) l, persons, retrieval, misery, iota, weighted, uniform, expertise, bump, chi)
  main = create_title(ylab, plot_box, which = if(nrow(means) > 1) l, persons, retrieval, misery, iota, weighted, uniform, expertise, bump, chi)
  if(l > 1 && plot_box && plot_save) save_plot(main)
  if((l == 1 || plot_box) && !plot_add)
   plot(1:ncol(means), family="AvenirLTStd-Roman", xlim=c(1,ncol(means)), main=main, type="n",xlab="",ylab="", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(2.4,1,0), ylim=ylim)
  par(new=TRUE)
  plot_colors = rep(c("#cc0000ff","#dd9900ff", "#00cc33ff", "#0033aaff", "#9900aaff"),10) # was rainbow(nrow(means))
  if(plot_box) {
   data = lapply(as.data.frame(aperm(values[l,,])), function(x) {x[x==-1] = NA; x}) # I'm sure there's a better way to write this
   boxplot(data, range=0, family="AvenirLTStd-Roman", xlim=c(1,ncol(means)), na.action = na.pass, boxcol=plot_colors[l], whiskcol=plot_colors[l], col="transparent", lwd=0.75, xlab="",  ylab="", xaxt="n",  yaxt="n", axes=FALSE, font=1,cex=1.3,cex.main=1, ylim=ylim)
   par(new=TRUE)
  }
  plot(means[l,], family="AvenirLTStd-Roman", xlim=c(1,ncol(means)), col=plot_colors[l], type="l", xlab="",  ylab="", xaxt="n",  yaxt="n", axes=FALSE, font=1, cex=1.6, ylim=ylim, ...) 
  if(plot_misery && !plot_add) {
   par(new=TRUE)
   plot(c(1-5,ncol(means)+5),c(misery,misery), family="AvenirLTStd-Roman", xlim=c(1,ncol(means)), lwd=0.4, lty="12", type="l", xlab="",  ylab="", xaxt="n",  yaxt="n", axes=FALSE, font=1,cex=1.6,cex.main=1, ylim=ylim)     
  }
 }
 if(plot_axis) {
  axis(4)
  mtext(ylab, family="AvenirLTStd-Roman", side=4, line=2.5, font=1,cex=1.6, cex.lab=1.6)    
 }
 if(plot_save) save_plot(main)
}

create_title <- function(plot_title="", plot_box=FALSE, which=NULL, persons=PERSONS, retrieval=RETRIEVAL, misery=MISERY, iota=IOTA, weighted=WEIGHTED, uniform=UNIFORM,  expertise=EXPERTISE, bump=BUMP, chi=CHI) {
 title=paste(plot_title, ", ", 
        if(!weighted)                   "unweighted, ",
        if(!uniform)                    "bimodal, ", 
        if(persons != PERSONS)      paste(persons, 
         if(persons == 1) "person, " else "persons, "), 
        if(retrieval != RETRIEVAL)      paste(retrieval, "retrieved, "), 
        if(misery != MISERY)            paste("misery ", misery, ", ", sep=""),
        if(expertise != EXPERTISE)      paste("expertise ", expertise, ", ", sep=""),
        if(bump != BUMP)                paste("bump ", bump, ", ", sep=""),
        if(chi != CHI)                  paste("chi ", chi, ", ", sep=""),
        if(iota != IOTA)                paste("iota ", iota, ", ", sep=""),
        if(plot_box && !is.null(which)) paste("person ", which, ", ", sep=""),
        sep="")
 substring(title, 0, nchar(title)-2)
 "" 
}

save_plot <- function(name) {
 name = sapply(strsplit(tolower(name), ", "), paste, collapse="_")
 name = sapply(strsplit(tolower(name), " "), paste, collapse="")
 quartz.save(paste(name, ".png", sep=""),type="png")    
}


plot_preferences <- function(...) {
 plot_values(preferences, plot_title="Preference", ylim=c(0,1), lty="12", lwd=2, ...)   
}

plot_satisfactions <- function(...) {
 plot_values(satisfactions, plot_title="Satisfaction", ylim=c(0,1), lwd=2, ...)   
}

plot_group_preferences <- function(...) {
 plot_values(group_preferences, plot_title="Group preference", ylim=c(-1,1), plot_box=TRUE, ...)   
}

plot_preferences_and_satisfactions <- function(...) {
 # Plots only for the first listener
 par(mar=c(5, 4, 4, 4) + 0.1)
 plot_values(array(preferences[1,,], dim=c(1,dim(preferences)[2:3])), plot_save = FALSE, plot_title="Preference and satisfaction", ylim=c(-1,1), lty="12", lwd=1.5, ...)   
 plot_values(array(satisfactions[1,,], dim=c(1,dim(satisfactions)[2:3])), plot_add = TRUE, plot_axis=TRUE, plot_title="Satisfaction", ylim=c(0,1), lwd=2, ...)   
 par(mar=c(5, 4, 4, 2) + 0.1)
}

plot_satisfactions_deviation <- function(...) {
 plot_values(satisfactions, plot_title="Satisfaction (std dev)", plot_aggregate="sd", ylim=c(0,0.5), lwd=2, ...)   
}

plot_assessed_preferences <- function(...) {
 assessed = apply(preferences, c(1,2), function(x) length(which(x == 0))/iterations)
 par(mar=c(5, 4, 4, 4) + 0.1)
 plot_values(assessed, plot_title="Discovered item", ylim=c(0,1), lty="12", plot_save = FALSE, lwd=1.5, ...)   
 plot_values(satisfactions, plot_title="Satisfaction", ylim=c(0,1), lwd=2, plot_add = TRUE, plot_axis=TRUE, ...)   
 par(mar=c(5, 4, 4, 2) + 0.1)
}

##############################################################################
#    Setup: Draw plotting area                                               #
##############################################################################

# quartz(family="Avenir LT Std",width=10,height=8) # ,bg="white")

##############################################################################
#    Experiment 0: Basic preferences and satisfactions plots                 #
##############################################################################

exp0 <- function() {
 params = list()
 do.call("generate", params) 
 do.call("plot_preferences", params) 
 do.call("plot_preferences", c(params, plot_box=TRUE)) 
 do.call("plot_satisfactions", params) 
 do.call("plot_satisfactions", c(params, plot_box=TRUE)) 
 do.call("plot_group_preferences", params) 
}

##############################################################################
#    Experiment 1: How the number users affects the satisfaction             #
##############################################################################

exp1 <- function(persons_range = 1:20) {
 for (persons in persons_range) {

  params = list(persons=persons)
  do.call("generate", params) 
  do.call("plot_satisfactions", params)
 }
}

##############################################################################
#    Experiment 2: How the retrieval size affects the satisfaction           #
##############################################################################

exp2 <- function(retrieval_range = 1:30) {
 for (retrieval in retrieval_range) {

  params = list(retrieval=retrieval)
  do.call("generate", params) 
  do.call("plot_satisfactions", params) 
 }
}

##############################################################################
#    Experiment 2: How the retrieval size affects the average preference     #
##############################################################################

exp2b <- function(retrieval_range = 1:40) {
 avg_pref = sapply(retrieval_range, function(retrieval) {
     params = list(retrieval=retrieval)
     do.call("generate", params)
     mean(apply(preferences, c(1,2), mean, na.rm=TRUE))
 })
 main = paste("Average individual preference by the retrieval size of the channel")
 plot(retrieval_range, avg_pref, xlim=c(1, max(retrieval_range)),  main=main, type="l",xlab="",ylab="", font=1,cex=1.3, cex.lab=1.3, cex.main=1.5, mgp=c(2.4,1,0), ylim=c(-1,1))
}


##############################################################################
#    Experiment 3: How the misery threshold affects the preferences          #
##############################################################################

exp3 <- function(misery_range = seq(-1,1,by=0.1)) {
 for (misery in misery_range) {

  params = list(misery=misery)
  do.call("generate", params) 
  do.call("plot_preferences", c(params, plot_misery=TRUE)) 
#  do.call("plot_group_preferences", c(params, plot_misery=TRUE))
 }
}

##############################################################################
#    Experiment 4: How past satisfaction affects the satisfaction            #
##############################################################################

exp4 <- function(bump_range = seq(-1,1,by=0.1)) { # nice example with 0.5
 for (bump in bump_range) {

  # Generate heterogeneous persons, NOT WEIGHTING with past satisfactions
  params = list(uniform=FALSE, weighted=0, bump=bump)
  do.call("generate", params) 
  do.call("plot_preferences", params) 
  do.call("plot_satisfactions", params) 
  
  # Generate heterogeneous persons, WEIGHTING with past satisfactions
  params = list(uniform=FALSE, weighted=1, bump=bump)
  do.call("generate", params) 
  do.call("plot_preferences", params) 
  do.call("plot_satisfactions", params) 
 }
}

##############################################################################
#    Experiment 5: How chi affects the satisfaction                          #
##############################################################################

# i will plot pref and sat on the same graph for one user, 
# and in case also the avg group pref and sat?

exp5 <- function(chi_range = seq(0,1,by=0.1)) {
 ratings = runif(turns)*2-1
 for (chi in chi_range) {
  params = list(chi=chi)
  do.call("generate", c(params, ratings = list(ratings))) 
  do.call("plot_preferences_and_satisfactions", params) 
 }
}

##############################################################################
#    Experiment 6: How the initial satisfaction affects the satisfaction     #
##############################################################################

exp6 <- function(iota_range = seq(0,1,by=0.1)) {
 for (iota in iota_range) {

  params = list(iota=iota)
  do.call("generate", params) 
  do.call("plot_satisfactions", params) 
 }
}


##############################################################################
#    Experiment 7: How memory influences standard deviation of satisfaction  #
##############################################################################

exp7 <- function(persons_range = seq(3,20,by=2)) {
 for (persons in persons_range) {
  # Generate heterogeneous persons, WITH memory of past satisfaction
  params = list(persons=persons, uniform=FALSE, weighted=1, bump=0)
  do.call("generate", params) 
  do.call("plot_satisfactions_deviation", c(params, plot_save = FALSE))

  # Generate heterogeneous persons, WITHOUT memory of past satisfaction
  params = list(persons=persons, uniform=FALSE, weighted=0, bump=0)
  do.call("generate", params) 
  do.call("plot_satisfactions_deviation", c(params, plot_add = TRUE, lty="12"))
 }
}

##############################################################################
#    Experiment 8: How group affinity affects satisfaction                   #
##############################################################################

exp8 <- function(persons_range = c(5,20), bump_range = seq(-1,1,by=0.1)) {
 for (persons in persons_range) {
  for (bump in bump_range) {
   # Generate heterogeneous persons, WITH memory of past satisfaction
   params = list(persons=persons, weighted=1, bump=bump)
   do.call("generate", params) 
   do.call("plot_satisfactions", params) 
  }
 }
}

exp8b <- function(bump_range = seq(-1,1,by=0.1)) {
 avg_pref = sapply(bump_range, function(bump) {
     params = list(bump=bump, weighted=1)
     do.call("generate", params)
     mean(apply(preferences, c(1,2), mean, na.rm=TRUE))
 })
 main = paste("Average individual preference by the concordance of 5 listeners")
 plot(bump_range, avg_pref, xlim=c(-1,1), main=main, type="l",xlab="",ylab="", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, mgp=c(2.4,1,0), ylim=c(-1,1))
}


##############################################################################
#    Experiment 9: How the quantity of ind. preferences affects satisfaction #
##############################################################################

exp9 <- function(expertise_range = seq(1,0,by=-0.1)) {
 for (expertise in expertise_range) {
  # Generate heterogeneous persons, WITH memory of past satisfaction
  params = list(expertise=expertise)
  do.call("generate", params) 
  do.call("plot_assessed_preferences", params) 
 }
}


####### Thesis

quartz(family="Avenir LT Std",width=10,height=8) # ,bg="white")


# Figure 6.1

params = list()
do.call("generate", params) 
do.call("plot_preferences", params) 
# quartz.save("fig6_1_left")
do.call("plot_satisfactions", params) 
# quartz.save("fig6_1_right")

# Figure 6.2

## Random distribution of preferences for 5 listeners
plot_colors = c("#cc0000ff","#dd9900ff", "#00cc33ff", "#0033aaff", "#9900aaff")
n = 50
a = matrix(runif(n),ncol=5)*2-1
plot(1:10,a[,1],type="n",family="AvenirLTStd-Roman",ylim=c(-1,1),xlab="",ylab="", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(2.4,1,0), xaxp=c(1,10,9)); }
sapply(1:5,function(i) {
    par(new=TRUE); 
    plot(1:10,a[,i],type="p",family="AvenirLTStd-Roman",pch=paste(i),col=plot_colors[i],cex=2.2,ylim=c(-1,1),axes=FALSE,xlab="",ylab="");
})
lines(c(-1,11),c(0,0), col="#999999ff")
sapply(1:10,function(i) {
    lines(c(i,i),c(-2,2),col="#bbbbbbff",lty="dotted")
})
# quartz.save("fig6_2_left")

## With a majority and a minority

d = matrix(runif(n),ncol=5)*2-1 # a
d[1:5,1:3] = d[1:5,1:3]/2-0.5
d[1:5,4:5] = d[1:5,4:5]/2+0.5
d[6:10,1:3] = d[6:10,1:3]/2+0.5
d[6:10,4:5] = d[6:10,4:5]/2-0.5
plot(1:10,d[,1],type="n",family="AvenirLTStd-Roman",ylim=c(-1,1),xlab="",ylab="", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(2.4,1,0), xaxp=c(1,10,9)); }
sapply(1:5,function(i) {
    par(new=TRUE); 
    plot(1:10,d[,i],type="p",family="AvenirLTStd-Roman",pch=paste(i),col=plot_colors[i],cex=2.2,ylim=c(-1,1),axes=FALSE,xlab="",ylab="");
})
lines(c(-1,11),c(0,0), col="#999999ff")
lines(c(5.5,5.5),c(-2,2), col="#999999ff")
sapply(1:10,function(i) {
    lines(c(i,i),c(-2,2),col="#bbbbbbff",lty="dotted")
})
# quartz.save("fig6_2_right")


# Figure 6.3 

params = list(uniform=FALSE, weighted=1, bump=0)
do.call("generate", params) 
do.call("plot_preferences", params) 
# quartz.save("fig6_3_left")
do.call("plot_satisfactions", params) 
# quartz.save("fig6_3_right")

# Figure 6.4 

params = list(uniform=FALSE, weighted=0, bump=0)
do.call("generate", params) 
do.call("plot_preferences", params) 
# quartz.save("fig6_4_left")
do.call("plot_satisfactions", params) 
# quartz.save("fig6_4_right")

# Figure 6.5 

# With a concordance of 0.5
diameter = 0.5
tmp = matrix(rep(runif(10, min=-1,max=1-diameter), 5), ncol=5)
tmp2 = matrix(runif(n, min=0, max=diameter),ncol=5)
e = tmp + tmp2
plot(1:10,a[,1],type="n",family="AvenirLTStd-Roman", ylim=c(-1,1),xlab="",ylab="", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(2.4,1,0), xaxp=c(1,10,9)); }
sapply(1:5,function(i) {
    par(new=TRUE); 
    plot(1:10,e[,i],type="p",family="AvenirLTStd-Roman", pch=paste(i),col=plot_colors[i],cex=2.2,ylim=c(-1,1),axes=FALSE,xlab="",ylab="");
})
lines(c(-1,11),c(0,0), col="#999999ff")
sapply(1:10,function(i) {
    lines(c(i,i),c(-2,2),col="#bbbbbbff",lty="dotted")
})
# quartz.save("fig6_5_left")

# With a concordance of 1
diameter = 1
tmp = matrix(rep(runif(10, min=-1,max=1-diameter), 5), ncol=5)
tmp2 = matrix(runif(n, min=0, max=diameter),ncol=5)
e = tmp + tmp2
plot(1:10,a[,1],type="n",family="AvenirLTStd-Roman", ylim=c(-1,1),xlab="",ylab="", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(2.4,1,0), xaxp=c(1,10,9)); }
sapply(1:5,function(i) {
    par(new=TRUE); 
    plot(1:10,e[,i],type="p",family="AvenirLTStd-Roman", pch=paste(i),col=plot_colors[i],cex=2.2,ylim=c(-1,1),axes=FALSE,xlab="",ylab="");
})
lines(c(-1,11),c(0,0), col="#999999ff")
sapply(1:10,function(i) {
    lines(c(i,i),c(-2,2),col="#bbbbbbff",lty="dotted")
})
# quartz.save("fig6_5_right")

# Figure 6.6 

params = list(persons=5, weighted=1, bump=0)
do.call("generate", params) 
do.call("plot_preferences", params) 
# quartz.save("fig6_6_left")

bump_range = seq(-1,1,by=0.2)
avg_pref = sapply(bump_range, function(bump) {
     params = list(bump=bump, weighted=1)
     do.call("generate", params)
     mean(apply(preferences, c(1,2), mean, na.rm=TRUE))
})
plot(bump_range+1, rev(avg_pref), family="AvenirLTStd-Roman", xlim=c(0,2),ylim=c(0,1),xlab="",ylab="", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(2.4,1,0), type="o", col=plot_colors[1])
# quartz.save("fig6_6_right")

# Figure 6.7

# # With a concordance of 2
# diameter = 2
# tmp = matrix(rep(runif(10, min=-1,max=1-diameter), 5), ncol=5)
# tmp2 = matrix(runif(n, min=0, max=diameter),ncol=5)
# e = tmp + tmp2
# plot(1:10,a[,1],type="n",family="AvenirLTStd-Roman", ylim=c(-1,1),xlab="",ylab="", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(2.4,1,0), xaxp=c(1,10,9)); }
# sapply(1:5,function(i) {
#     par(new=TRUE); 
#     plot(1:10,e[,i],type="p",family="AvenirLTStd-Roman", pch=paste(i),col=plot_colors[i],cex=2.2,ylim=c(-1,1),axes=FALSE,xlab="",ylab="");
# })
# lines(c(-1,11),c(0,0), col="#999999ff")
# sapply(1:10,function(i) {
#     lines(c(i,i),c(-2,2),col="#bbbbbbff",lty="dotted")
# })
# par(new=TRUE); 
# plot(1:10,diag(e[,rep(1:5,2)]),type="p",family="AvenirLTStd-Roman", pch=21,col=plot_colors[rep(1:5,2)],bg="#aaaaaa33",cex=4.5,ylim=c(-1,1),axes=FALSE,xlab="",ylab="");
# # quartz.save("fig6_7a_left")
# 
# params = list()
# do.call("generate", params) 
# 
#sapply(1:25,function(i) {
#    preferences[(i-1)%%5+1,i,1] <<- NA
#})




# Figure 6.8

params = list(persons=2)
do.call("generate", params) 
do.call("plot_satisfactions", params)
# quartz.save("fig6_8_left")

params = list(persons=20)
do.call("generate", params) 
do.call("plot_satisfactions", params)
# quartz.save("fig6_8_left")

# Figure 6.9

params = list(retrieval=30)
do.call("generate", params) 
do.call("plot_preferences", params) 
# quartz.save("fig6_9_left")

retrieval_range = 1:40
avg_pref = sapply(retrieval_range, function(retrieval) {
    params = list(retrieval=retrieval)
    do.call("generate", params)
    mean(apply(preferences, c(1,2), mean, na.rm=TRUE))
})
plot(retrieval_range, avg_pref, family="AvenirLTStd-Roman", xlim=c(1, max(retrieval_range)),ylim=c(0,1),xlab="",ylab="", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(2.4,1,0), type="l", col=plot_colors[1])
# quartz.save("fig6_9_right")

# Figure 6.10

params = list(misery=0)
do.call("generate", params) 
do.call("plot_preferences", params) 
# quartz.save("fig6_10_left")

params = list(misery=0.5)
do.call("generate", params) 
do.call("plot_preferences", params) 
# quartz.save("fig6_10_right")

# Figure 6.10

params = list(iota=0)
do.call("generate", params) 
do.call("plot_satisfactions", params) 
# quartz.save("fig6_11_left")

params = list(iota=1)
do.call("generate", params) 
do.call("plot_satisfactions", params) 
# quartz.save("fig6_11_right")


# Figure 2.2 

data <- read.table("mystrands_03_sorted_songs_per_playlist.txt")
plot(data[1:15,], xlab="",ylab="", xlim=c(1,15), ylim=c(0,320000), pch=21, font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0), type="o", col=plot_colors[1],lwd=2, xaxp=c(1,15,14),yaxt="n")
par(new=TRUE)
data <- read.table("mystrands_03_sorted_artists_per_playlist.txt")
plot(data[1:15,], xlab="",ylab="", xlim=c(1,15), ylim=c(0,320000), pch=22, font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0), type="o", col=plot_colors[4],lwd=2, xaxp=c(1,15,14),yaxt="n", axes=FALSE)
axis(2, at=0,labels="0", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
axis(2, at=100000,labels="100,000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
axis(2, at=200000,labels="200,000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
axis(2, at=300000,labels="300,000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
# quartz.save("fig2_2_left")

plot(c(10,11),rep(280000,2), xlab="",ylab="", xlim=c(1,15), ylim=c(0,320000), pch=21, font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0), type="o", col=plot_colors[1],lwd=2, xaxp=c(1,15,14),axes=FALSE)
par(new=TRUE)
plot(c(10,11),rep(250000,2), xlab="",ylab="", xlim=c(1,15), ylim=c(0,320000), pch=22, font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0), type="o", col=plot_colors[4],lwd=2, xaxp=c(1,15,14),axes=FALSE)

data <- read.table("mystrands_02_songs_per_playlist.txt")
plot(data[1:40,], xlab="",ylab="", xlim=c(1,40), ylim=c(0,50000), pch=21, font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0), type="o", col=plot_colors[1],lwd=2, xaxp=c(0,40,10),yaxt="n")
axis(2, at=0,labels="0", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
#axis(2, at=12500,labels="12.500", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
#axis(2, at=25000,labels="25.000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
#axis(2, at=37500,labels="37.500", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
axis(2, at=10000,labels="10,000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
axis(2, at=20000,labels="20,000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
axis(2, at=30000,labels="30,000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
axis(2, at=40000,labels="40,000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
axis(2, at=50000,labels="50,000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
# quartz.save("fig2_2_right")

# Figure 2.3

data <- read.table("mystrands_04_occurrences_per_song.txt")
plot(data, xlab="",ylab="", xlim=c(1,52000), ylim=c(0,260000), pch=21, font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0), type="o", col=plot_colors[1],lwd=2, xaxt="n",yaxt="n")
axis(2, at=0,labels="0", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
#axis(2, at=50000, labels="50,000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
axis(2, at=100000,labels="100,000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
#axis(2, at=150000,labels="150,000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
axis(2, at=200000,labels="200,000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
#axis(2, at=250000,labels="250,000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
axis(1, at=0,labels="0", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0))
axis(1, at=10000,labels="10,000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0))
axis(1, at=20000,labels="20,000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0))
axis(1, at=30000,labels="30,000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0))
axis(1, at=40000,labels="40,000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0))
axis(1, at=50000,labels="50,000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0))
# quartz.save("fig2_3_left")

plot(data[2:20,], xlab="",ylab="", xlim=c(2,20), ylim=c(0,110000), pch=21, font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0), type="o", col=plot_colors[1],lwd=2, xaxp=c(2,20,9), yaxt="n")
axis(2, at=0,labels="0", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
axis(2, at=25000,labels="25,000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
axis(2, at=50000,labels="50,000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
axis(2, at=75000,labels="750,000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
axis(2, at=100000,labels="100,000", font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6)
# quartz.save("fig2_3_righ")

# Figure 3.5

quartz(family="AvenirLTStd-Roman",width=10,height=5)
y <- function(avg) (log((x*(min(x)+max(x)-2*avg)+avg*avg-min(x)*max(x))/((max(x)-avg)*(avg-min(x))))/log((max(x)-avg)/(avg-min(x))))
x = seq(1.0,5.0,by=0.1)

plot(x, ((x-min(x)) - (max(x)-x))/(max(x)-min(x)),family="AvenirLTStd-Roman", xlim=c(min(x)-0.05*(max(x)+min(x)),max(x)+0.05*(max(x)+min(x))), xlab="",ylab="", ylim=c(-1-0.1,1+0.1),  pch=21, lty="dashed",font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0), type="l", col="#006600ff",lwd=2, xaxt="n", yaxp=c(-1,1,2))
par(new=TRUE);
plot(x, y(1.6),family="AvenirLTStd-Roman", xlim=c(min(x)-0.05*(max(x)+min(x)),max(x)+0.05*(max(x)+min(x))), ylim=c(-1-0.1,1+0.1), xlab="", ylab="", pch=21, lty="dotted",font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0), type="l", col=plot_colors[1],lwd=2, xaxp=c(1,5,4), axes=FALSE)
par(new=TRUE);
plot(x, y(4.3),family="AvenirLTStd-Roman", xlim=c(min(x)-0.05*(max(x)+min(x)),max(x)+0.05*(max(x)+min(x))), ylim=c(-1-0.1,1+0.1), xlab="", ylab="", pch=21, lty="dotted",font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0), type="l", col=plot_colors[1],lwd=2, xaxp=c(1,5,4), axes=FALSE)
lines(c(-1,11),c(0,0), col="#999999ff")
par(new=TRUE);
plot(x,rep(0,length(x)),family="AvenirLTStd-Roman", xlim=c(min(x)-0.05*(max(x)+min(x)),max(x)+0.05*(max(x)+min(x))), ylim=c(-1-0.1,1+0.1), xlab="", ylab="", pch=21, font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0), type="l", col=plot_colors[4],lwd=2.5, xaxp=c(1,5,4), axes=FALSE)
axis(1,at=1,labels="")
axis(1,at=3,labels="")
axis(1,at=5,labels="")
#quartz.save("fig3_5left.pdf",type="pdf")




# Figure 3.6


quartz(family="AvenirLTStd-Roman",width=10,height=5) # ,bg="white")
x = 1:10
y = c(1,3,2,3,4,2,1,1,1,2)
ny =c(-1,1/3,0,1/3,2/3,0,-1,-1,-1,0)
plot(x,y,family="AvenirLTStd-Roman", xlab="",ylab="", xlim=c(1,10), ylim=c(1,5), pch=21, bg="white", lty="dotted",font=1,cex=c(1.6,1.6,1.6,1.6,3,1.6,1.6,1.6,1.6,1.6), cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0), type="o", col=plot_colors[1],lwd=2, xaxp=c(1,10,9))
segments(0,2,11,2,lty="dashed",lwd=0.3)
quartz.save("fig3_6a_left.pdf",type="pdf")

plot(x,ny,family="AvenirLTStd-Roman", xlab="",ylab="", xlim=c(1,10), ylim=c(-1,1), pch=22, bg=plot_colors[1],font=1,cex=c(1.6,1.6,1.6,1.6,3,1.6,1.6,1.6,1.6,1.6), cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0), type="o", col=plot_colors[1],lwd=2, xaxp=c(1,10,9))
lines(c(-1,11),c(0,0), col="#999999ff")
quartz.save("fig3_6a_right.pdf",type="pdf")

z = c(1,5,5,3,4,2,5,5,1,4)
nz =c(-1,1,1,-1/3,0,-2/3,1,1,-1,0)
plot(x,z,family="AvenirLTStd-Roman", xlab="",ylab="", xlim=c(1,10), ylim=c(1,5), pch=21, bg="white", lty="dotted",font=1,cex=c(1.6,1.6,1.6,1.6,3,1.6,1.6,1.6,1.6,1.6), cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0), type="o", col=plot_colors[4],lwd=2, xaxp=c(1,10,9))
segments(0,4,11,4,lty="dashed",lwd=0.3)
quartz.save("fig3_6b_left.pdf",type="pdf")

plot(x,nz,family="AvenirLTStd-Roman", xlab="",ylab="", xlim=c(1,10), ylim=c(-1,1), pch=22, bg=plot_colors[4],font=1,cex=c(1.6,1.6,1.6,1.6,3,1.6,1.6,1.6,1.6,1.6), cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0), type="o", col=plot_colors[4],lwd=2, xaxp=c(1,10,9))
lines(c(-1,11),c(0,0), col="#999999ff")
quartz.save("fig3_6b_right.pdf",type="pdf")



# Figure 4.4

quartz(family="AvenirLTStd-Roman",width=10,height=6)
h1 <- function(t, alpha=0.9) (alpha*t) %% 2 - 1 # or whatever rnd function
h2 <- function(t) {set.seed(save.seed, kind = "default"); (runif(1:1000)*2 - 1)[t+1]}
h  <- function(t) {0.5+0.5*c(-0.39099219, -0.54975863,  0.64780866, -0.69143442, -0.47034046, 0.40887579, 0.26055271, -0.75978313, -0.64293005, 0.71700292,  0.07379019, -0.09667739, -0.51370321, 0.29128101, 0.96727560,  0, -0.02900041, -0.08757081, -0.01967315, -0.13351452, -0.13674167,  -0.79307541, -0.23672249, -0.08238743, -0.73240963)[t]}
q <- function(t,chi=0.5) { sum((chi**(0:t))*h(t:0))/sum(chi**(0:t)) }

# to print
plot(1:25,h(1:25),family="AvenirLTStd-Roman", xlim=c(1,25), xlab="",ylab="", ylim=c(0,1),  pch=21, lty="solid",font=1,cex=1, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0), type="o", col="#006600ff", bg="#006600ff",lwd=1.5, xaxp=c(0,25,5), yaxp=c(0,1,2))
axis(1,at=1,labels="1",family="AvenirLTStd-Roman",font=1,cex=1.6, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0))
par(new=TRUE)
plot(1:25,sapply(1:25,function(x) q(x,chi=0.2)), xlim=c(1,25),family="AvenirLTStd-Roman", xlab="",ylab="", ylim=c(0,1),  pch=22, lty="dotted",font=1,cex=1, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0), type="o", col=plot_colors[1], bg=plot_colors[1],lwd=2, xaxp=c(0,25,5), yaxp=c(0,1,2), axes=FALSE)
par(new=TRUE)
plot(1:25,sapply(1:25,function(x) q(x,chi=0.8)), xlim=c(1,25),family="AvenirLTStd-Roman", xlab="",ylab="", ylim=c(0,1),  pch=23, lty="dashed",font=1,cex=1, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0), type="o", col=plot_colors[4], bg=plot_colors[4],lwd=2, xaxp=c(0,25,5), yaxp=c(0,1,2), axes=FALSE)

par(new=TRUE)
plot(c(20,21),c(0.95,0.95),family="AvenirLTStd-Roman", xlim=c(1,25), xlab="",ylab="", ylim=c(0,1),  pch=21, lty="solid",font=1,cex=1, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0), type="o", col="#006600ff", bg="#006600ff",lwd=1.5, xaxp=c(0,25,5), yaxp=c(0,1,2), axes=FALSE)
par(new=TRUE)
plot(c(20,21),c(0.85,0.85),family="AvenirLTStd-Roman", xlim=c(1,25), xlab="",ylab="", ylim=c(0,1),  pch=22, lty="dotted",font=1,cex=1, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0), type="o", col=plot_colors[1], bg=plot_colors[1],lwd=2, xaxp=c(0,25,5), yaxp=c(0,1,2), axes=FALSE)
par(new=TRUE)
plot(c(20,21),c(0.75,0.75),family="AvenirLTStd-Roman", xlim=c(1,25), xlab="",ylab="", ylim=c(0,1),  pch=23, lty="dashed",font=1,cex=1, cex.lab=1.6, cex.main=1.6, cex.axis=1.6, mgp=c(3,1.5,0), type="o", col=plot_colors[4], bg=plot_colors[4],lwd=2, xaxp=c(0,25,5), yaxp=c(0,1,2), axes=FALSE)
#quartz.save("fig4_4left.pdf",type="pdf")
