#Test the monotonicity  (cukup ganti system name untuk method)

#Test the monotonicity when AV winner is A, B, C
testMono1 <- function(currentWinner,m,sim,systemname,percentage){
  people <-  c("A","B","C")
  winnercolumn <- which(people==currentWinner)
  trytominus <- which(m[,winnercolumn]!=1)
  trytoadd <- which(m[,winnercolumn]==1)
  for(q in trytoadd){
    for(z in trytominus){
      nsim=sim
      nsim[z,i]=sim[z,i]-round(percentage/100*sim[z,i])
      nsim[q,i]=nsim[q,i]+round(percentage/100*sim[z,i])#make new sim/ percentage% of particularpreference order voters in favor of winner
      newwinner=systemname(nsim[,i],m)
      if(newwinner != currentWinner) {return(z)}#stop when violate monotonicity
    }
  }
  # If we get here... none have failed!
  return(0)
}

#Test the monotonicity when AV winner is AB,AC,BC
testMono2 <- function(currentWinner,m,sim,systemname,percentage){
  people <-  c("BC","AC","AB")
  losercolumn <- which(people==currentWinner)
  trytominus <- which(m[,losercolumn]==1)
  trytoadd <- which(m[,losercolumn]!=1)
  for(q in trytoadd){
    for(z in trytominus){
      nsim=sim
      nsim[z,i]=sim[z,i]-round(percentage/100*sim[z,i])
      nsim[q,i]=nsim[q,i]+round(percentage/100*sim[z,i])#make new sim/ percentage% of particularpreference order voters in favor of winner
      newwinner=systemname(nsim[,i],m)
      if(newwinner != currentWinner) {return(z)}#stop when violate monotonicity
    }
  }
  # If we get here... none have failed!
  return(0)
}

results <- c()
for(i in 1:n){
  current = AVwinner(sim[,i],m) #GANTI FUNGSI METHOD
  if (current=="A"|current=="B"|current=="C"){
    results[i] <- testMono1(current,m,sim,AVwinner,percent)#GANTI FUNGSI METHOD
  }
  if (current=="AB"|current=="BC"|current=="AC"){
    results[i] <- testMono2(current,m,sim,AVwinner,percent)#GANTI FUNGSI METHOD
  }
}
results
return(length(which(results!=0))/n)
