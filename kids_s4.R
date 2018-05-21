# Hello! Welcome to the S4 guide for R users.
# 
# Before you go through with this tutorial, I highly recommend you to at least get the basics on Object Oriented Programming (OOP) and S3. 
# It's not mandatory to understand these concepts but it surely does help a lot. 
# 
# So far, I do not have available materials on OOP but there is a S3 guide in my GitHub repository, in the same place this file can be found. Feel free to explore
# it as much as you want.
# 
# In this guide, we will be using a simulation to exemplify our work. The simulation is part of a project of Professor Walmes Zeviani, from the Universidade Federal
# do Paraná, who oriented me through this project and suggested that I used his work here.
# 
# It involves two universes of children. In both universes, we have a given number of kids who are collecting cards for an album. What makes each universe so unique is
# in how these children behave when trading. In one universe, we will have a unilateral environment, where when two kids meet to trade their cards with each other, at
# least one of them must benefit from the trade. In the other universe, a bilateral environment will exist, where both kids must benefit from the trade. A quick
# illustration: 
# 
# Unilateral environment: Kids k1 and k2 meet to trade. k1 has 3 cards that k2 needs, but k2 only has 2 cards that k1 wants. So, they trade the cards they want from
# each other and k1 gives the third card to k2, who gives k1 a random card from its collection, if there is any extra.
# 
# Bilateral environment: Kids k1 and k2 meet to trade. k1 has 3 cards that k2 needs, but k2 only has 2 cards that k1 wants. Each one gives the other the two cards of
# interest. No other cards are traded.
# 
# One last thing: any bugs that you find or any questions that you have may be addressed to my e-mail. I will answer you with maximum effort.
# 
# Without further ado, let's get started!


# The first thing we need to set when working with OOP is the object classes we will be working with. 

kid <- setClass(
  Class = "kid",

  slots = c(
    album = "logical",
    collection = "numeric"
  ),

  prototype = list(
    album = logical(10),
    collection = numeric(10)
  ),

  validity = function(object){
    if(!is.logical(object@album)){
      return("kid@album is not logical.")
    }
    else if(!is.numeric(object@collection)){
      return("kid@collection is not numeric.")
    }
    else if(!length(object@album) == length(object@collection)){
      return("Album and collection must be of same length.")
    }
  }
)

bi <- setClass(
  "bi",

  contains = "kid"
)

uni <- setClass(
  "uni",

  contains = "kid"
)

create_universe <- function(kids_total = 10, album_size = 100, kid_class = "bi"){
  if(kid_class == "bi"){
    for(i in 1:kids_total){
      assign(paste0("kid", i), bi(album = logical(album_size), collection = numeric(album_size)), envir = .GlobalEnv)
    }
  }
  else if(kid_class == "uni"){
    for(i in 1:kids_total){
      assign(paste0("kid", i), bi(album = logical(album_size), collection = numeric(album_size)), envir = .GlobalEnv)
    }
  }
  assign("kids", eval(parse(text = paste0("list(", paste0("kid", 1:kids_total, collapse = ", "), ")"))), envir = .GlobalEnv)
  assign("kid_class", c("kids", kid_class), envir = .GlobalEnv)
  assign("album_size", album_size, envir = .GlobalEnv)
}

meet <- setClass(
  "meet",

  slots = c(
    order = "matrix"
  ),

  prototype = list(
    order = matrix(0, nrow = 2, ncol = length(kids))
  ),

  validity = function(object){
    if(!is.matrix(object@order)){
      return("Object should be a matrix.")
    }
    else if(!dim(object@order)[2] == choose(length(kids), 2)){
      return("All children must meet exactly once.")
    }
  }
)

setGeneric(
  name = "putcards",
  def = function(kids_p, cards_p){
    standardGeneric("putcards")
  }
)

setMethod(
  f = "putcards",
  signature = "kid",
  definition = function(kids_p, cards_p){
    for(card in cards_p){
      if(kids_p@album[card] == FALSE){
        kids_p@album[card] <- TRUE
      }
      else{
        kids_p@collection[card] <- kids_p@collection[card] + 1
      }
    }
    return(kids_p)
  }
)

setGeneric(
  name = "removecards",
  def = function(kids_r, cards_r){
    standardGeneric("removecards")
  }
)

setMethod(
  f = "removecards",
  signature = "kid",
  definition = function(kids_r, cards_r){
    for(card_r in cards_r){
      if(kids_r@collection[card_r] > 0){
        kids_r@collection[card_r] <- kids_r@collection[card_r] - 1
      }
      else{
        stop("The specified cards are not in stock here")
      }
    }
    return(kids_r)
  }
)

setGeneric(
  name = "buycards",
  def = function(kids_b, total_cards = album_size, pack_size = 5){
    standardGeneric("buycards")
  }
)

setMethod(
  f = "buycards",
  signature = "kid",
  definition = function(kids_b, total_cards, pack_size){
    pack <- sort(sample(total_cards, pack_size, TRUE))
    kids_b <- putcards(kids_b, pack)
    return(kids_b)
  }
)

setGeneric(
  name = "getcards",
  def = function(kids_g, what){
    standardGeneric("getcards")
  }
)

setMethod(
  f = "getcards",
  signature = "kid",
  definition = function(kids_g, what){
    tryCatch(eval(parse(text = paste0("kids_g@", what))),
             error = function(e) print(paste0("'", what, "' is not a valid parameter.")))
  }
)

setGeneric(
  name = "encounter",
  def = function(kids_e){
    standardGeneric("encounter")
  }
)

setMethod(
  f = "encounter",
  signature = "meet",
  definition = function(kids_e){
    enc <- meet()
    enc@order <- combn(length(kids), 2)
    enc_order <- sample(ncol(enc@order), replace = FALSE)
    enc@order <- enc@order[,enc_order]
    return(enc)
  }
)

setGeneric(
  name = "stock",
  def = function(k1, k2){
    standardGeneric("stock")
  }
)

setMethod(
  f = "stock",
  signature = "kid",
  definition = function(k1, k2){
    k1.stock <- getcards(k1, "stock")
    k2.album <- which(getcards(k2, "album") == 0)
    k1.stock <- which(k1.stock > 0)[which(which(k1.stock > 0) %in% k2.album)]
    return(k1.stock)
  }
)

setGeneric(
  name = "swap",
  def = function(kids_s, k1, k2, k1.stock, k2.stock){
    standardGeneric("swap")
  }
)

setMethod(
  f = "swap",
  signature = "kid",
  definition = function(kids_s, k1, k2, k1.stock, k2.stock){
    if(!length(k1.stock) == length(k2.stock)){
      if(length(k1) < length(k2)){
        k.least <- k1
        k.most <- k2
        k.least.stock <- k1.stock
        k.most.stock <- k2.stock
      }
      else{
        k.least <- k2
        k.most <- k1
        k.least.stock <- k2.stock
        k.most.stock <- k1.stock
      }
      kids_s[[k.least]] <- removecards(kids_s[[k.least]], k.least.stock)
      kids_s[[k.most]] <- putcards(kids_s[[k.most]], k.least.stock)
      extra <- getcards(kids_s[[k.least]], "stock")
      k.extra <- min(length(k.most.stock) - lenght(k.least.stock), sum(extra))
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
        kids_s[[k.least]] <- removecards(kids_s[[k.least]], k.least.remove)
        kids_s[[k.most]] <- putcards(kids_s[[k.most]], k.least.remove)
        extra[k.east.remove] <- extra[k.least.remove] - 1
        ex <- ex - 1
      }
      kids_s[[k.least]] <- putcards(kids_s[[k.least]], k.most.stock)
      kids_s[[k.most]] <- removecards(kids_s[[k.most]], k.most.stock)
    }
    else{
      kids_s[[k1]] <- putcards(kids_s[[k1]], k2.stock)
      kids_s[[k2]] <- putcards(kids_s[[k2]], k1.stock)
      kids_s[[k1]] <- removecards(kids_s[[k1]], k1.stock)
      kids_s[[k2]] <- removecards(kids_s[[k2]], k2.stock)
    }
    return(kids_s)
  }
)

setGeneric(
  name = "trade",
  def = function(kids_bu){
    standardGeneric("trade")
  }
)

setMethod(
  f = "trade",
  signature = "bi",
  definition = function(kids_bu){
    if(length(kids_bu) > 1){
      enc <- encounter(meet())
      for(i in 1:nrow(enc@order)){
        k1 <- enc@order[1, i]
        k2 <- enc@order[2, i]
        k1.stock <- stock(kids_bu, k1, k2)
        k2.stock <- stock(kids_bu, k2, k1)

        if(!length(k1.stock) == 0 & !length(k2.stock) == 0){
          k <- min(length(k1.stock), length(k2.stock))
          kids_bu <- swap.kids(kids_bu, k1, k2, k1.stock, k2.stock)
        }
      }
    }
    return(kids_bu)
  }
)

setMethod(
  f = "trade",
  signature = "uni",
  definition = function(kids_bu){
    if(length(kids_bu) > 1){
      enc <- encounter(meet())

      for(i in 1:dim(enc@order)[2]){
        k1 <- enc@order[1, i]
        k2 <- enc@order[2, i]
        k1.stock <- stock(kids_bu, k1, k2)
        k2.stock <- stock(kids_bu, k2, k1)

        k1.extra <- getcards(kids_tu[[k1]], "stock")
        k2.extra <- getcards(kids_tu[[k2]], "stock")

        if((!length(k1.stock) == 0 | !lenght(k2.stock) == 0) & (length(k1.extra > 0) > 0 & length(k2.extra > 0) > 0)){
          kids_bu <- swap(kids_bu, k1, k2, k1.stock, k2.stock)
        }
      }
    }
    return(kids_bu)
  }
)
