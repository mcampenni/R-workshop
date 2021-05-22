
indDF <- data.frame(id=1:2, strategy=NA, num_wins=0)
indDF

chooseStrategy <- function(ind){
    strats <- sample(x=1:3, size=nrow(ind)) # 1:Paper, 2:Scissors, 3:Rock
    ind$strategy <- strats
    return(ind)
}

playStrategy <- function(ind){
    if(ind$strategy[1]==ind$strategy[2]) {} else{
        #in the case that one chose Rock and the other paper:
        if(any(ind$strategy == 3) && any(ind$strategy == 1)){ 
            tmp <- ind[ind$strategy==1, "id"]
            ind[tmp,"num_wins"] <- ind[tmp,"num_wins"]+1
        }else{
            #for the two other cases, the better weapon wins:
            tmp <- which(ind[,"strategy"]==max(ind[,"strategy"]))
            ind[tmp,"num_wins"] <- ind[tmp,"num_wins"]+1 
        }
    }
    return(ind)
}


# Third: Let's loop it
for(i in 1:1000){
    indDF <- chooseStrategy(indDF)#; indDF
    indDF <- playStrategy(indDF)#; indDF
    i <- i+1
}
indDF

setup <- function(){
    return(data.frame(id=1:2, strategy=NA, num_wins=0))
}

rounds <- 1000
indDF <- setup()
dat <- matrix(NA, rounds, 2)
for(i in 1:rounds){
    indDF <- chooseStrategy(indDF)
    indDF <- playStrategy(indDF)
    dat[i,] <- indDF$num_wins
    i <- i+1
}

plot(dat[,1], type='l', col='#EA2E49', lwd=3, xlab='time', ylab='number of rounds won')
lines(dat[,2], col='#77C4D3', lwd=3)


chooseStrategy2 <- function(ind){
	strats <- sample(x=1:3, size=1) # 1:Paper, 2:Scissors, 3:Rock
	ind$strategy[2] <- strats
	return(ind)
}


rounds <- 1000
repetitions <- 100
dat <- matrix(NA, rounds, 2)
res2 <- c()
for(j in 1:repetitions){
	indDF <- setup()
	indDF[1,"strategy"] <- sample(1:3,1)
	for(i in 1:rounds){
		indDF <- chooseStrategy2(indDF)
		indDF <- playStrategy(indDF)
		dat[i,] <- indDF$num_wins
		i <- i+1
	}
	res2 <- c(res2, which(indDF[,"num_wins"]==max(indDF[,"num_wins"])))
	j <- j+1
}

plot(dat[,1], type='l', col='blue', lwd=3, xlab='time', ylab='number of rounds won')
lines(dat[,2], col='red', lwd=3)
# for comparisson let's calculate the winning vector for both players switch strategies:

res1 <- c()
for(j in 1:repetitions){
    indDF <- setup()
    for(i in 1:rounds){
        indDF <- chooseStrategy(indDF)
        indDF <- playStrategy(indDF)
        dat[i,] <- indDF$num_wins
        i <- i+1
    }
    res1 <- c(res1, which(indDF[,"num_wins"]==max(indDF[,"num_wins"])))
    j <- j+1
}
# and the winner is:
t.test(res1,res2)


# The game on a Lattice network

require(igraph)
require(reshape)
require(ggplot2)

# size of the lattice
sidelength<-10 
# creating an empty data.frame to store data
stat<-data.frame()
# creating a lattice network using the igraph package
l<-graph.lattice(length=sidelength,dim=2)
# now every individual chooses a strategy at random
V(l)$weapon<-sample(c(1:3), size=length(V(l)), replace=T)
# for a nicer visualisation lets colour the different options
V(l)[weapon==1]$color<-'blue' # Paper
V(l)[weapon==2]$color<-'yellow' # Scissors
V(l)[weapon==3]$color<-'green' # Rock
# and this is what it looks like:
plot(l, layout=as.matrix(expand.grid(1:sidelength, 1:sidelength)), vertex.label=NA)

for(t in 1:1000){
	# pick a random agent ...
	from <- as.numeric(sample(V(l), 1)) # or sample(sidelength^2,1)
	# who are its neighbours?
	nei<-neighbors(l, v=from, mode='all')
	
	# if there is only one weapon type left
	if(length(unique(V(l)$weapon))==1) {
		# we can either stop the simulation
		stop(paste(c('Paper','Scissors','Rock')[unique(V(l)$weapon)], 'has won the game after',t,'rounds!'))
		# or we let the selected individual choose a different strategy to let the dynamics go on
		# V(l)$weapon[from]<-sample((1:3)[1:3!=as.numeric(V(l)$weapon[from])], 1)
	} else {
		# ... and one of its neighbours
		to <- sample(nei, 1) 
		fromto<-c(from,to)
		w<-as.numeric(V(l)$weapon[fromto])
		# if both choices are equal, nothing happens:
		if(w[1]==w[2]) {} else{ 
			# in the case that one chooses Rock and the other Paper, Paper wins:
			if(max(w) == 3 && min(w) ==1) { 
				V(l)$weapon[fromto[w==3]] <- "1" 
			} 
			else{
				# for the two other cases, the better weapon wins:
				V(l)$weapon[fromto[w==min(w)]] <- V(l)$weapon[fromto[w==max(w)]]
			}
		} 
		
	}
	# let's record the individual abundance of each strategy
    stat <- rbind(stat, c(sum(V(l)$'weapon'=="1"), sum(V(l)$'weapon'=="2"), sum(V(l)$'weapon'=="3")))
    # plot(l, layout=as.matrix(expand.grid(1:sidelength, 1:sidelength)), vertex.label=NA)
}

names(stat)<-c("Paper","Scissors","Rock")
s<-melt(stat)
s$time<-1:nrow(stat)

ggplot(data=s, mapping = aes(x = time, 
                             y = value, 
                             col = variable)) +
    geom_line() + 
    labs(colour = "strategy") +
    theme_bw() 

