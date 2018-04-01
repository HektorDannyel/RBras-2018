buycards <- function(x, ...){
  UseMethod("buycards", x)
}

buycards.kid <- function(kids){
  for(i in 1:length(kids)){
    pack <- sort(sample(10, 5, TRUE))
    for(j in pack){
      if(kids[[i]][[1]][j] == 0){
        kids[[i]][[1]][j] <- TRUE
      } 
      else {
        kids[[i]][[2]][j] <- kids[[i]][[2]][j] + 1
      }
    }
  }
  return(kids)
}

kids <- buycards(kids)

getcards <- function(x, ...){
  UseMethod("getcards", x)
}

getcards.kid <- function(kids, x, get){
  if(x > length(kids)){
    stop("There is no kid of index ", x, ".")
  }
  
  if(get == "stock"){
    kids[[x]][[2]]
  }
  else if(get == "album"){
    kids[[x]][[1]]
  }
  else{
    stop(paste0("There is no parameter called ", get))
  }
}

trade <- function(kid, ...){
  UseMethod("trade", kid)
}

trade.kid <- function(kid, behaviour){
  if(length(kid) == 1){
    return(kid)
  }
  else{meet <- matrix(combn(1:length(kid), 2), nrow = 2)
    if(dim(meet)[2] > 1){
      meet <- meet[,sample(1:dim(meet)[2], dim(meet)[2], FALSE)]
    }
    for(i in 1:dim(meet)[2]){
      k1 <- meet[1, i]
      k2 <- meet[2, i]
      k1.stock <- getcards(kid, k1, "stock")
      k1.album <- which(getcards(kid, k1, "album") == 0)
      k2.stock <- getcards(kid, k2, "stock")
      k2.album <- which(getcards(kid, k2, "album") == 0)
      k1.stock <- which(k1.stock > 0)[which(which(k1.stock > 0) %in% k2.album)]
      k2.stock <- which(k2.stock > 0)[which(which(k2.stock > 0) %in% k1.album)]
      if(behaviour == "bi"){
        if(!length(k1.stock) == 0 & !length(k2.stock) == 0){
          k <- min(length(k1.stock), length(k2.stock))
          if(length(k1.stock) > 1){
            k1.stock <- sample(k1.stock, k, FALSE)
          }
          if(length(k2.stock) > 1){
            k2.stock <- sample(k2.stock, k, FALSE)
          }
          kid[[k1]][[1]][k2.stock] <- TRUE
          kid[[k1]][[2]][k1.stock] <- kid[[k1]][[2]][k1.stock] - 1
          kid[[k2]][[1]][k1.stock] <- TRUE
          kid[[k2]][[2]][k2.stock] <- kid[[k2]][[2]][k2.stock] - 1
        }
      }
      if(behaviour == "uni"){
        if((!length(k1.stock) == 0 | !length(k2.stock) == 0) & (length(getcards(kid, k1, "stock") > 0) > 0 & length(getcards(kid, k2, "stock") > 0) > 0)){
          if(length(k1.stock) > length(k2.stock)){
            kid[[k2]][[2]][k2.stock] <- kid[[k2]][[1]][k2.stock] - 1
            kid[[k1]][[1]][k2.stock] <- TRUE
            k.extra <- min(length(k1.stock) - length(k2.stock), sum(getcards(kid, k2, "stock")))
            if(length(k1.stock) > 1){
              k1.stock <- sample(k1.stock, length(k2.stock) + k.extra, FALSE)
            }
            i <- k.extra
            k2.remove <- numeric(1)
            while(i > 0){
              if(length(which(getcards(kid, k2, "stock") > 0)) > 1){
                k2.remove <- sample(which(getcards(kid, k2, "stock") > 0), 1)
              }
              else if(length(which(getcards(kid, k2, "stock") > 0)) == 1){
                k2.remove <- which(getcards(kid, k2, "stock") > 0)
              }
              kid[[k2]][[2]][k2.remove] <- kid[[k2]][[2]][k2.remove] - 1
              kid[[k1]][[2]][k2.remove] <- kid[[k1]][[2]][k2.remove] + 1
              i <- i - 1
            }
            kid[[k2]][[1]][k1.stock] <- TRUE
            kid[[k1]][[2]][k1.stock] <- kid[[k1]][[2]][k1.stock] - 1
          }
          else if(length(k1.stock) < length(k2.stock)){
            kid[[k1]][[2]][k1.stock] <- kid[[k1]][[1]][k1.stock] - 1
            kid[[k2]][[1]][k1.stock] <- TRUE
            k.extra <- min(length(k2.stock) - length(k1.stock), sum(getcards(kid, k1, "stock")))
            if(length(k2.stock) > 1){
              k2.stock <- sample(k2.stock, length(k1.stock) + k.extra, FALSE)
            }
            i <- k.extra
            k1.remove <- numeric(1)
            while(i > 0){
              if(length(which(getcards(kid, k1, "stock") > 0)) > 1){
                k1.remove <- sample(which(getcards(kid, k1, "stock") > 0), 1)
              }
              else if(length(which(getcards(kid, k1, "stock") > 0)) == 0){
                k1.remove <- which(getcards(kid, k1, "stock") > 0)
              }
              kid[[k1]][[2]][k1.remove] <- kid[[k1]][[2]][k1.remove] - 1
              kid[[k2]][[2]][k1.remove] <- kid[[k2]][[2]][k1.remove] + 1
              i <- i - 1
            }
            kid[[k1]][[1]][k2.stock] <- TRUE
            kid[[k2]][[2]][k2.stock] <- kid[[k2]][[2]][k2.stock] - 1
          }
          else if(length(k1.stock) == length(k2.stock)){
            kid[[k1]][[1]][k2.stock] <- TRUE
            kid[[k2]][[1]][k1.stock] <- TRUE
            kid[[k1]][[2]][k1.stock] <- kid[[k1]][[2]][k1.stock] - 1
            kid[[k2]][[2]][k2.stock] <- kid[[k2]][[2]][k2.stock] - 1
          }
        }
      }
    }
  return(kid)
  }
}

kids <- trade(kids, "uni")

kid1 <- list(album <- logical(10),
             collection <- numeric(10))

kid2 <- list(album <- logical(10),
             collection <- numeric(10))

kid3 <- list(album <- logical(10),
             collection <- numeric(10))

class(kid1) <- "kid"
class(kid2) <- "kid"
class(kid3) <- "kid"

kids <- list(kid1, kid2, kid3)
class(kids) <- "kid"

len <- length(kids)
i <- 0

while(len > 0){
  m <- len + 1
  kids <- buycards(kids)
  kids <- trade(kids, "uni")
  
  for(j in 1:length(kids)){
    if(sum(getcards(kids, j, "album")) == sum(getcards(kids, j, "album"), !getcards(kids, j, "album"))){
      m <- j
    }
  }
  
  kids <- kids[-m]
  class(kids) <- "kid"
  
  i <- i + 1
  
  len <- length(kids)
  
  if(len == 0){
    print(paste0("O total de iterações necessárias para completar toda a coleção foi: ", i))
  }
  
}
