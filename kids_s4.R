# Hello! Welcome to my S4 example for R users.
# 
# Before you go through with this tutorial, I highly recommend you to at very least get the basics on Object Oriented Programming (OOP) and S3 and if possible the  
# basics from S4. It's not mandatory to understand these concepts but it surely does help a lot, since this will not go through all the whats and ifs of S4. 
# 
# So far, I do not have available materials on OOP but there is a S3 example in my GitHub repository, in the same place this file can be found. Feel free to explore
# it as much as you want.
# 
# In this guide, we will be using a simulation to help illustrate our work. The simulation is part of a project of Professor Walmes Zeviani, from the Universidade Federal
# do Paraná (Federal University of Paraná), who oriented me through this project and suggested that I used his work here.
# 
# It involves two universes of children. In both universes, we have a given number of kids who are collecting cards for an album. What makes each universe so unique is
# how these children behave when trading. In one universe, we will have a unilateral environment, where when two of the kids meet to trade their cards with each other, at
# least one of them must benefit from the trade. In the other universe, a bilateral environment will exist, where both kids must benefit from the trade. A quick
# illustration: 
# 
# Kids k1 and k2 meet to trade. k1 has 3 cards that k2 needs, but k2 only has 2 cards that k1 wants.
# 
# Unilateral environment: They trade the cards they want from each other and k1 gives the third card to k2, who gives k1 a random card from their collection,
# if there is any extra.
# 
# Bilateral environment: Each one gives the other the two cards of interest. No other cards are traded.
# 
# There are a few more details about the simulation itself, which will be explained along the code. Whenever you see a comment hash followed by the '@' symbol, means
# the explanation regards the simulation, and not the code. -> #@
# 
# One last thing: any bugs that you find or any questions that you have may be addressed to my e-mail (hektor.brasil@gmail.com). I will answer you with maximum effort.
# 
# Without further ado, let's get started!

# IMPORTANT!
# 
# In S3, values within objects are called using the 'dollar' symbol (object$value). S4 uses the 'at' symbol (object@value).

# The first thing we need to set when working with OOP is the object classes we will be working with. In S4, the example below shows how to set a class.

kid <- setClass(
  Class = "kid", #the class

  slots = c(
    album = "logical", #@ A logical vector of size n. Each position (1:n) represents the ID of a card. 
    collection = "numeric" #@ A numeric vector of size n. Each position (1:n) represents the ID of a card.
  ), # Think of the slots as being the characteristics of the object. You must specify what those slots will hold (logical and numeric, in this case) and name them.

  prototype = list(
    album = logical(10), #@ If album[1] == TRUE means the card of ID 1 exists within the album. If album[1] == FALSE, the kid does not have such card.
    collection = numeric(10) #@ Each position counts the amount of extra cards of ID 1:n.
  ), # The prototype is the default value your slots will receive.

  validity = function(object){
    if(!is.logical(object@album)){
      return("kid@album is not logical.") # Validates the 'album' input
    }
    else if(!is.numeric(object@collection)){
      return("kid@collection is not numeric.") # Validates the 'collection' input
    }
    else if(!length(object@album) == length(object@collection)){
      return("Album and collection must be of same length.") # Makes sure 'album' and 'collection' have the same length
    }
  } # The validity function is not mandatory but it is recommended. It will save your objects from being defined with incorrect inputs.
)

kid() # This is an object of class 'kid'.

# In S4, Inheritance is present in a much more elegant way than in S3. It determines that a newly created class will *inherit* all of the characteristics from the
# previous class.

bi <- setClass(
  "bi",

  contains = "kid" # 'contains' is the argument used to define from which class the newly setted will inherit the characteristics.
) # 'bi' is the class that defines a *bilateral* behaviour.

uni <- setClass(
  "uni",

  contains = "kid"
) # 'uni' is the class that defines a *unilateral* behaviour.

# Although not explicitly necessary, create_universe will enhance our simulation by dinamically creating the universe in which we will be working.
# It will create n kids with an album size of p and define the behaviour and gather them all in a list.

create_universe <- function(kids_total = 10, album_size = 100, kid_class = "bi"){
  if(kid_class == "bi"){
    for(i in 1:kids_total){
      assign(paste0("kid", i), # The name of the object
             bi(album = logical(album_size), collection = numeric(album_size)), # Defining the class
             envir = .GlobalEnv) # Assigning to the Global Environment
    }
  }
  else if(kid_class == "uni"){
    for(i in 1:kids_total){
      assign(paste0("kid", i), bi(album = logical(album_size), collection = numeric(album_size)), envir = .GlobalEnv)
    }
  }
  assign("kids", eval(parse(text = paste0("list(", paste0("kid", 1:kids_total, collapse = ", "), ")"))), envir = .GlobalEnv) # Placing all generated kids in a list
  assign("kid_class", c("kids", kid_class), envir = .GlobalEnv) # Assigning the classes to a character vector
  assign("album_size", album_size, envir = .GlobalEnv) # Assigning the album size to a numeric vector
}

#@ Meet is an object that will define in which order the kids will meet, two at a time. Every kid at least tries to meet with another (they will not succeed if there
#@ are no cards to be traded).

# Try to understand 'meet' on your own! You can use the class we defined earlier as a guide.

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

# Now that we defined our classes, it is time to put our functions to use.
# When defining a function, the first thing to do is defining a generic function. It will understand the class of the objects we are using and will define the structure
# for the methods.

setGeneric(
  name = "putcards", # Every function deserves a name!
  def = function(kids_p, cards_p){ # Here we define the structure our next functions will have. They *must* match the parameters.
    standardGeneric("putcards")
  }
)

setMethod(
  f = "putcards", # The generic function 
  signature = "kid", # The class of the objects this function will affect
  definition = function(kids_p, cards_p){ # The function itself!
    for(card in cards_p){
      if(kids_p@album[card] == FALSE){ #@ If the card i is NOT in the album...
        kids_p@album[card] <- TRUE #@ ...put it in the album!
      }
      else{ #@ If the card i IS in the album...
        kids_p@collection[card] <- kids_p@collection[card] + 1 #@...put it with the others!
      }
    }
    return(kids_p)
  }
)

# The procedure is very standard. Try to understand how the 'removecards' function works on your own!

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
  def = function(kids_b, total_cards = album_size, pack_size = 5){ # When defining a default value to a parameter in your function, always do it in the Generic!
    standardGeneric("buycards")
  }
)

setMethod(
  f = "buycards",
  signature = "kid",
  definition = function(kids_b, total_cards, pack_size){
    pack <- sort(sample(total_cards, pack_size, TRUE)) #@ This function will make each kid "go to the store" to "buy" a new pack of cards.
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
  definition = function(kids_g, what){ #@ This will show a kid's collection or album, depends on what is asked...
    tryCatch(eval(parse(text = paste0("kids_g@", what))),
             error = function(e) print(paste0("'", what, "' is not a valid parameter."))) #@...and return an error function if the parameter does not exist.
  }
)

# This next function does not work with the class 'kid'. Try to understand what kind of object it affects and how!

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
