create_universe <- function(kids_total = 10, album_size = 100, kid_class = "bi"){
  for(i in 1:kids_total){
    assign(paste0("kid", i), eval(parse(text = paste0("structure(list(album <- logical(", album_size, "), collection <- numeric(", album_size, ")), class = 'kids')"))), envir = .GlobalEnv)
  }
  assign("kids", eval(parse(text = paste0("structure(list(", paste0("kid", 1:kids_total, collapse = ", "), "), class = c('kids', '", kid_class,"'))"))), envir = .GlobalEnv)
  assign("kid_class", c("kids", kid_class), envir = .GlobalEnv)
  assign("album_size", album_size, envir = .GlobalEnv)
}

putcards <- function(x, ...){
  UseMethod("putcards", x)
}

putcards.kids <- function(kids_p, cards_p){
  for(card in cards_p){
    if(kids_p[[1]][card] == FALSE){
      kids_p[[1]][card] <- TRUE
    }
    else{
      kids_p[[2]][card] <- kids_p[[2]][card] + 1
    }
  }
  return(kids_p)
}

removecards <- function(x, ...){
  UseMethod("removecards", x)
}

removecards.kids <- function(kids_r, cards_r){
  for(card_r in cards_r){
    if(kids_r[[2]][card_r] > 0){
      kids_r[[2]][card_r] <- kids_r[[2]][card_r] - 1
    }
    else{
      stop("The specified cards are not in stock here.")
    }
  }
  return(kids_r)
}

buycards <- function(x, ...){
  UseMethod("buycards", x)
}

buycards.kids <- function(kids_b, total_cards = album_size, pack_size = 5){
  for(i in 1:length(kids_b)){
    pack <- sort(sample(total_cards, pack_size, TRUE))
    kids_b[[i]] <- structure(putcards(kids_b[[i]], pack), class = kid_class)
  }
  return(kids_b)
}

getcards <- function(x, ...){
  UseMethod("getcards", x)
}

getcards.kids <- function(kids_g, x, get){
  if(x > length(kids_g)){
    stop(paste0("There is no kid of index ", x, "."))
  }
  if(get == "stock"){
    kids_g[[x]][[2]]
  }
  else if(get == "album"){
    kids_g[[x]][[1]]
  }
  else{
    stop(paste0("There is no parameter called ", get))
  }
}

encounter <- function(x, ...){
  UseMethod("encounter", x)
}

encounter.kids <- function(kids_e){
  
  meet <- matrix(combn(1:length(kids_e), 2), nrow = 2)
  if(dim(meet)[2] > 1){
    meet <- meet[,sample(1:dim(meet)[2], dim(meet)[2], FALSE)]
  }
  return(meet)
}

stock <- function(x, ...){
  UseMethod("stock", x)
}

stock.kids <- function(kids_s, k1, k2){
  k1.stock <- getcards(kids_s, k1, "stock")
  k2.album <- which(getcards(kids_s, k2, "album") == 0)
  k1.stock <- which(k1.stock > 0)[which(which(k1.stock > 0) %in% k2.album)]
  return(k1.stock)
}

swap <- function(x, ...){
  UseMethod("swap", x)
}

swap.kids <- function(kids_sw, k1, k2, k1.stock, k2.stock){
  if(!length(k1.stock) == length(k2.stock)){
    eval(parse(text = paste0("k.least <- k", which(c(length(k1.stock), length(k2.stock)) == min(length(k1.stock), length(k2.stock))))))
    eval(parse(text = paste0("k.most <- k", which(c(length(k1.stock), length(k2.stock)) == max(length(k1.stock), length(k2.stock))))))
    eval(parse(text = paste0("k.least.stock <- k", which(c(length(k1.stock), length(k2.stock)) == min(length(k1.stock), length(k2.stock))), ".stock")))
    eval(parse(text = paste0("k.most.stock <- k", which(c(length(k1.stock), length(k2.stock)) == max(length(k1.stock), length(k2.stock))), ".stock")))
    kids_sw[[k.least]] <- removecards(kids_sw[[k.least]], k.least.stock)
    kids_sw[[k.most]] <- putcards(kids_sw[[k.most]], k.least.stock)
    extra <- getcards(kids_sw, k.least, "stock")
    k.extra <- min(length(k.most.stock) - length(k.least.stock), sum(extra))
    if(length(k.most.stock) > 1){
      k.most.stock <- sample(k.most.stock, length(k.least.stock) + k.extra, FALSE)
    }
    ex <- k.extra
    k.least.remove <- numeric(1)
    while(ex > 0){
      if(length(which(extra > 0)) > 1){
        k.least.remove <- sample(which(extra > 0), 1)
      }
      else if(length(which(extra > 0)) == 1){
        k.least.remove <- which(extra > 0)
      }
      kids_sw[[k.least]] <- removecards(kids_sw[[k.least]], k.least.remove)
      kids_sw[[k.most]] <- putcards(kids_sw[[k.most]], k.least.remove)
      extra[k.least.remove] <- extra[k.least.remove] - 1
      ex <- ex - 1
    }
    kids_sw[[k.least]] <- putcards(kids_sw[[k.least]], k.most.stock)
    kids_sw[[k.most]] <- removecards(kids_sw[[k.most]], k.most.stock)
  }
  else{
    kids_sw[[k1]] <- putcards(kids_sw[[k1]], k2.stock)
    kids_sw[[k2]] <- putcards(kids_sw[[k2]], k1.stock)
    kids_sw[[k1]] <- removecards(kids_sw[[k1]], k1.stock)
    kids_sw[[k2]] <- removecards(kids_sw[[k2]], k2.stock)
  }
  
  return(kids_sw)
  
}

trade <- function(x, ...){
  UseMethod("trade", x)
}

trade.bi <- function(kids_tb){
  
  if(length(kids_tb) > 1){
  
    meet <- encounter(kids_tb)
  
    for(i in 1:dim(meet)[2]){
      k1 <- meet[1, i]
      k2 <- meet[2, i]
      k1.stock <- stock(kids_tb, k1, k2)
      k2.stock <- stock(kids_tb, k2, k1)
      
      if(!length(k1.stock) == 0 & !length(k2.stock) == 0){
        k <- min(length(k1.stock), length(k2.stock))
        # if(length(k1.stock) > 1){
        #   k1.stock <- sample(k1.stock, k, FALSE)
        # }
        # if(length(k2.stock) > 1){
        #   k2.stock <- sample(k2.stock, k, FALSE)
        # }
        kids_tb <- swap.kids(kids_tb, k1, k2, k1.stock, k2.stock)
      }
    }
  }
  return(kids_tb)
}

trade.uni <- function(kids_tu){
  
  if(length(kids_tu) > 1){
    meet <- encounter(kids_tu)
    
    for(i in 1:dim(meet)[2]){
      k1 <- meet[1, i]
      k2 <- meet[2, i]
      k1.stock <- stock(kids_tu, k1, k2)
      k2.stock <- stock(kids_tu, k2, k1)
      
      k1.extra <- getcards(kids_tu, k1, "stock")
      k2.extra <- getcards(kids_tu, k2, "stock")
    
      if((!length(k1.stock) == 0 | !length(k2.stock) == 0) & (length(k1.extra > 0) > 0 & length(k2.extra > 0) > 0)){
        kids_tu <- swap(kids_tu, k1, k2, k1.stock, k2.stock)
      }
    }
  }  
  return(kids_tu)
}


create_universe(3, 30, "bi")
  
len <- length(kids)
i <- 0

while(len > 0){
  m <- len + 1
  kids <- buycards(kids)
  kids <- trade(kids)
  
  for(j in 1:length(kids)){
    if(sum(getcards(kids, j, "album")) == sum(getcards(kids, j, "album"), !getcards(kids, j, "album"))){
      m <- j
    }
  }
  
  kids <- structure(kids[-m], class = kid_class)
  
  i <- i + 1
  
  len <- length(kids)
  
  if(len == 0){
    print(paste0("O total de iterações necessárias para completar toda a coleção foi: ", i))
  }
  
}