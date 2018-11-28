
adder <- function(x,y,z){
  if(x==0 | y==0){
    ans <- x+y
  }else if(z==TRUE){
    ans <- x+y
  }else{
    ans <- x*y
  }
  return(ans)
}

x<-3 
for(i in x:x+10){
  print(adder(i,4,2>1))
}
