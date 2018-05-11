kid <- setClass(
  "kid",
  
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
    tryCatch(eval(parse(text = (paste0("kids_g@", what)))),
             error = function(e) print(paste0("'", what, "' is not a valid parameter.")))
  }
)

