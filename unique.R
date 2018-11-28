unique <- function(x,y,z){
  if(x==y & y==z){
    ans <- 0
  }else if(x==y){
    ans <- z
  }else if(x==z){
    ans <- y
  }else if(y==z){
    ans  <- x
  }else{
    ans <- x+y+z
  }
  return(ans)
}
print(unique(32,35,32))