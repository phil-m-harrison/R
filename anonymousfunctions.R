#a <- c(3,78,1,5)
#print(a[1])

anonfunc <- function(func, anumber){
  func(anumber)
}

print(anonfunc(function(x){return (x[1])}, c(9,78,1,5)))