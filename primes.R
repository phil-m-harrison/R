primecounter <- function(q){
  i <- 1
  primecount<--1
  for(i in 1:q){
    count<-0
    for(j in 1:i){
      if(i%%j==0){
        count<-count +1
      }
    }
    if(count<3){
      primecount<-primecount+1
    }
  }
  print(primecount)
}
primecounter(100000)