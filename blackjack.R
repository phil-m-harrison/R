blackjack <- function(x,y){
  if(x>0 & y>0){
    if(x>21 & y>21){
      ans <- 0
    }else if(x>y){
      if(x>21){
        ans <- y 
      }else{
        ans <- x
      }
    }else{
      if(y>21){
        ans <- x
      }else{
        ans <- y
      }
    }
    return (ans)
  }
  else{
    return("Invalid")
  }
}

print(blackjack(21,14))