putcards <- function(x, ...){
  UseMethod("putcards", x)
}

putcards.kids <- function(kids, cards){
  for(card in cards){
    if(kids[[1]][card] == FALSE){
      kids[[1]][card] <- TRUE
    }
    else{
      kids[[2]][card] <- kids[[2]][card] + 1
    }
  }
  return(kids)
}

removecards <- function(x, ...){
  UseMethod("removecards", x)
}

removecards.kids <- function(kids, cards){
  for(card in cards){
    if(kids[[2]][card] > 0){
      kids[[2]][card] <- kids[[2]][card] - 1
    }
    else{
      stop("The specified cards are not in stock here.")
    }
  }
  return(kids)
}

buycards <- function(x, ...){
  UseMethod("buycards", x)
}

buycards.kids <- function(kids){
  for(i in 1:length(kids)){
    pack <- sort(sample(10, 5, TRUE))
    putcards(kids[[i]], pack)
  }
  return(kids)
}

getcards <- function(x, ...){
  UseMethod("getcards", x)
}

getcards.kids <- function(kids, x, get){
  if(x > length(kids)){
    stop(paste0("There is no kid of index ", x, "."))
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

encounter <- function(x, ...){
  UseMethod("encounter", x)
}

encounter.kids <- function(kids){
  meet <- matrix(combn(1:length(kids), 2), nrow = 2)
  if(dim(meet)[2] > 1){
    meet <- meet[,sample(1:dim(meet)[2], dim(meet)[2], FALSE)]
  }
  return(meet)
}

stock <- function(x, ...){
  UseMethod("stock", x)
}

stock.kids <- function(kids, k1, k2){
  k1.stock <- getcards(kids, k1, "stock")
  k2.album <- which(getcards(kids, k2, "album") == 0)
  k1.stock <- which(k1.stock > 0)[which(which(k1.stock > 0) %in% k2.album)]
  return(k1.stock)
}

trade <- function(kids, ...){
  UseMethod("trade", kids)
}

swap <- function(x, ...){
  UseMethod("swap", x)
}

swap.kids <- function(kids, k1, k2, k1.stock, k2.stock, extra){
  if(length(min(k1, k2)) == 1){
    eval(parse(text = paste0("k.least <- k", which(c(length(k1.stock), length(k2.stock)) == min(length(k1.stock), length(k2.stock))))))
    eval(parse(text = paste0("k.most <- k", which(c(length(k1.stock), length(k2.stock)) == max(length(k1.stock), length(k2.stock))))))
    eval(parse(text = paste0("k.least.stock <- k", which(c(length(k1.stock), length(k2.stock)) == min(length(k1.stock), length(k2.stock))), ".stock")))
    eval(parse(text = paste0("k.most.stock <- k", which(c(length(k1.stock), length(k2.stock)) == max(length(k1.stock), length(k2.stock))), ".stock")))
    kids[[k.least]] <- removecards(kids[[k.least]], k.least.stock)
    kids[[k.most]] <- putcards(kids[[k.most]], k.least.stock)
    k.extra <- min(length(k.most.stock) - length(k.least.stock), sum(extra))
    if(length(k.most.stock) > 1){
      k.most.stock <- sample(k.most.stock, length(k.least.stock) + k.extra, FALSE)
    }
    ex <- k.extra
    k.least.remove <- numeric(1)
    while(ex > 0){
      if(length(which(k.least.extra > 0)) > 1){
        k.least.remove <- sample(which(extra > 0), 1)
      }
      else if(length(which(extra > 0)) == 1){
        k.remove <- which(extra > 0)
      }
      kids[[k.least]] <- removecards(kids[[k.least]], k.remove)
      kids[[l.most]] <- putcards(kids[[k.most]], k.remove)
      ex <- ex - 1
    }
    kids[[k.least]] <- putcards(kids[[k.least]], k.most.stock)
    kids[[k.most]] <- removecards(kids[[k.most]], k.most.stock)
  }
  else{
    kids[[k1]] <- putcards(kids[[k1]], k2.stock)
    kids[[k2]] <- putcards(kids[[k2]], k1.stock)
    kids[[k1]] <- removecards(kids[[k1]], k1.stock)
    kids[[k2]] <- removecards(kids[[k2]], k2.stock)
  }
  
  return(kids)
  
}

trade.bi <- function(kids){
  
  meet <- encounter(kids)
  
  for(i in 1:dim(meet)[2]){
    k1 <- meet[1, i]
    k2 <- meet[2, i]
    k1.stock <- stock(kids, k1, k2)
    k2.stock <- stock(kids, k2, k1)
    
    if(!length(k1.stock) == 0 & !length(k2.stock) == 0){
      k <- min(length(k1.stock), length(k2.stock))
      if(length(k1.stock) > 1){
        k1.stock <- sample(k1.stock, k, FALSE)
      }
      if(length(k2.stock) > 1){
        k2.stock <- sample(k2.stock, k, FALSE)
      }
      kids <- swap(kids, k1, k2, k1.extra, k2.extra)
    }
  }
  return(kids)
}

trade.uni <- function(kids){
  
  meet <- encounter(kids)
  
  for(i in 1:dim(meet)[2]){
    k1 <- meet[1, i]
    k2 <- meet[2, i]
    k1.stock <- stock(kids, k1, k2)
    k2.stock <- stock(kids, k2, k1)
    
    k1.extra <- getcards(kids, k1, "stock")
    k2.extra <- getcards(kids, k2, "stock")
  
    if((!length(k1.stock) == 0 | !length(k2.stock) == 0) & (length(k1.extra > 0) > 0 & length(k2.extra > 0) > 0)){
      kids <- swap(kids, k1, k2, k1.extra, k2.extra)
    }
  }
  return(kids)
}


create_universe <- function(kids, album){
  for(i in 1:kids){
    eval(parse(text = paste0("kid", i, " <- structure(list(album <- logical(", album, "), collection <- numeric(", album, ")), class = 'kids')")))
  }
}

create_universe(3, 10)
  
kid1 <- list(album <- logical(10),
             collection <- numeric(10))

kid2 <- list(album <- logical(10),
             collection <- numeric(10))

kid3 <- list(album <- logical(10),
             collection <- numeric(10))

class(kid1) <- "kids"
class(kid2) <- "kids"
class(kid3) <- "kids"

kids <- list(kid1, kid2, kid3)
class(kids) <- "kids"

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
  class(kids) <- "kids"
  
  i <- i + 1
  
  len <- length(kids)
  
  if(len == 0){
    print(paste0("O total de iterações necessárias para completar toda a coleção foi: ", i))
  }
  
}
