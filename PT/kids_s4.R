#----------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#- Ola! Seja bem-vindo ao meu exemplo de aplicacao S4 para usuarios R.                                                                                                -#                     -#
#-                                                                                                                                                                    -#
#- Antes de voce iniciar este tutorial, eu recomendo fortemente que voce ao menos entenda os basicos de Programacao Orientada a Objeto (POO) e S3. Ainda, se possivel,-#
#- os basicos de S4. Nao e obrigatorio entender esses conceitos mas certamente ajuda bastante, uma vez que este tutorial nao vai se aprofundar em todos os detalhes   -#
#- de S4.                                                                                                                                                             -#
#-                                                                                                                                                                    -#
#- Por enquanto, eu nao possuo materiais disponiveis sobre POO mas existe um exemplo de aplicacao S3 no meu repositorio do GitHub, utilizando o mesmo caso deste.     -#
#- Sinta-se a vontade para explorar o quanto desejar.                                                                                                                 -#
#-                                                                                                                                                                    -#
#- Neste guia, estaremos utilizando uma simulacao para ajudar a ilustrar nosso trabalho. A simulacao e parte de um projeto do Professor Walmes Zeviani, da            -#
#- Universidade Federal do Parana, que me orientou ao longo deste projeto e sugeriu que eu utilizasse seu trabalho aqui.                                              -#
#-                                                                                                                                                                    -#
#- A simulacao envolve dois universos de criancas. Em ambos os universos, temos um dado numero de criancas que estao colecionando cromos de um album. O que torna cada-#
#- universo unico e o comportamento destas criancas quando trocando. Em um universo, teremos um ambiente unilateral, onde quando duas criancas se encontram para      -#
#- trocar suas cartas, pelo menos uma delas se beneficia da troca. No outro universo, existira um ambiente bilateral, onde ambas as criancas devem se beneficiar com  -#
#- a troca. Uma rapida ilustracao:                                                                                                                                    -#
#-                                                                                                                                                                    -#
#- As criancas k1 e k2 se encontram para trocar. k1 tem 3 cartas que k2 precisa, mas k2 possui somente 2 cartas de interesse para k1                                  -#
#-                                                                                                                                                                    -#
#- Ambiente unilateral: Eles trocam as cartas de interesse e k1 da a terceira carta para k2, que da uma carta aleatoria de sua colecao, se tiver alguma sobrando      -#
#-                                                                                                                                                                    -#
#- Ambiente bilateral: Cada uma da duas cartas de interesse para a outra. Nenhuma outra carta e trocada.                                                              -#
#-                                                                                                                                                                    -#
#- Ha mais alguns detalhes sobre a simulacao, que serao explicados ao longo do codigo. Sempre que voce ver uma marcacao de comentario seguida de um arroba, significa -#
#- que a explicacao vai tratar da simulacao, e nao do codigo. -> #@                                                                                                   -#
#-                                                                                                                                                                    -#
#- Uma ultima coisa: quaisquer bugs que voce encontrar ou perguntas que voce tiver podem ser enviadas para meu email: hektor.brasil@gmail.com                         -#
#- Te responderei com maximo esforco!                                                                                                                                 -#
#-                                                                                                                                                                    -#
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------#



# Sem mais delongas, vamos comecar!


# Importante! --------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------------------------#
# IMPORTANTE!                                                                                                               #
#                                                                                                                           #
# Em S3, valores dentro de objetos sao chamados usando o cifrao (objeto$valor). S4 utiliza o arroba (objeto@valor)          #
#                                                                                                                           #
#---------------------------------------------------------------------------------------------------------------------------#


# Definindo classes e objetos ---------------------------------------------------------


# A primeira coisa a ser definida quando trabalhamos com POO sao as classes dos objetos com que trabalharemos. O exemplo abaixo mostra como definir uma classe S4.

kid <- setClass(
  Class = "kid", #A classe

  slots = c( # Pense que os slots sao as caracteristicas do objeto. Voce deve especificar o que esses slots suportarao (um logico e um numerico neste caso) e nomea-los
    album = "logical", #@ Um vetor logico de tamanho n. Cada posicao (1:n) representa o codigo de um card.
    collection = "numeric" #@ Um vetor numerico de tamanho n. Cada posicao (1:n) representa o codigo de um card.
  ), 

  prototype = list( # O prototype e o valor padrao que seu slot vai receber.
    album = logical(10), #@ Se album[1] == TRUE Significa que o card 1 existe dentro do album. Se album[1] == FALSE, a crianca nao tem dado card.
    collection = numeric(10) #@ Cada posicao conta a quantidade de cartas repetidas de codigo 1:n
  ), 

  validity = function(object){ # A funcao validity nao e obrigatoria mas e recomendada. Ela vai salvar os objetos de serem definidos com inputs incorretos.
    if(!is.logical(object@album)){
      return("kid@album is not logical.") # Valida o input 'album'
    }
    else if(!is.numeric(object@collection)){
      return("kid@collection is not numeric.") # Valida o input 'collection'
    }
    else if(!length(object@album) == length(object@collection)){
      return("Album and collection must be of same length.") # Garante que 'album' e 'collection' tem o mesmo comprimento
    }
  }
)


kid() # Este e um objeto de classe 'kid'.


#---

  
# Em S4, Heranca esta presente de uma forma muito mais elegante do que em S3. Ela determinara que uma classe definida recentemente 'herda' as caracteristicas da classe
# anterior.


bi <- setClass(
  "bi",

  contains = "kid" # 'contains' e o argumento utilizado para definid de qual classe o novo objeto herdara as caracteristicas.
) # 'bi' e a classe que define um comportamento *bilateral*.



uni <- setClass(
  "uni",

  contains = "kid"
) # 'uni' e a classe que define um comportamento *unilateral*.


#---


# Mesmo que nao explicitamente necessario, create_universe vai melhorar nossa simulacao ao criar dinamicanete o universo em que estaremos trabalhando.
# A funcao criara n criancas com um album de tamanho p, definira o comportamento de troca e agrupara todas em uma lista.

create_universe <- function(kids_total = 10, album_size = 100, kid_class = "bi"){
  if(kid_class == "bi"){
    for(i in 1:kids_total){
      assign(paste0("kid", i), # O nome do objeto
             bi(album = logical(album_size), collection = numeric(album_size)), # Definindo a classe
             envir = .GlobalEnv) # Atribuindo ao Ambiente Global
    }
  }
  else if(kid_class == "uni"){
    for(i in 1:kids_total){
      assign(paste0("kid", i), bi(album = logical(album_size), collection = numeric(album_size)), envir = .GlobalEnv)
    }
  }
  assign("kids", eval(parse(text = paste0("list(", paste0("kid", 1:kids_total, collapse = ", "), ")"))), envir = .GlobalEnv) # Alocando todas as criancas geradas em uma lista
  assign("kid_class", c("kids", kid_class), envir = .GlobalEnv) # Atribuindo as classes a um vetor string
  assign("album_size", album_size, envir = .GlobalEnv) # Atribuindo o tamanho do album a um vetor numerico
}

#---


#@ Meet e um objeto que definira qual a ordem de encontro das criancas, duas por vez. Todas as criancas ao menos tentam se encontrar com outra (elas nao terao sucesso
#@ se nao ha cartas para serem trocadas).

# Tente compreender 'meet' sozinho! Voce pode usar a classe definida previamente como um guia.

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


# Funcoes ---------------------------------------------------------------


# Agora que definimos nossas classes, e hora de colcar as funcoes para um bom uso.
# Quando definindo uma funcao, a primeira coisa a ser feita e definir uma funcao generica. Ela entendera qual a classe dos objetos utilizados e definira uma estrutura  
# para os metodos.

setGeneric(
  name = "putcards", # Todas as funcoes merecem um nome!
  def = function(kids_p, cards_p){ # Here we define the structure our next functions will have. They *must* match the parameters.
    standardGeneric("putcards")
  }
)

setMethod(
  f = "putcards", # The generic function's name 
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


#---


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


#---


setGeneric(
  name = "buycards",
  def = function(kids_b, total_cards = album_size, pack_size = 5){ # When defining a default value to a parameter in your function, always do it in the Generic!
    standardGeneric("buycards")
  }
)

setMethod( 
  f = "buycards", #@ The 'buycards' function will make each kid "go to the store" to "buy" a new pack of cards.
  signature = "kid",
  definition = function(kids_b, total_cards, pack_size){
    pack <- sort(sample(total_cards, pack_size, TRUE)) 
    kids_b <- putcards(kids_b, pack)
    return(kids_b)
  }
)


#---


setGeneric(
  name = "getcards",
  def = function(kids_g, what){
    standardGeneric("getcards")
  }
)

setMethod( 
  f = "getcards", #@ The 'getcards' function will show a kid's collection or album, depends on what is asked...
  signature = "kid",
  definition = function(kids_g, what){ 
    tryCatch(eval(parse(text = paste0("kids_g@", what))),
             error = function(e) print(paste0("'", what, "' is not a valid parameter."))) #@...and return an error function if the input parameter does not exist.
  }
)


#---


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


#---


setGeneric(
  name = "stock",
  def = function(k1, k2){
    standardGeneric("stock")
  }
)

setMethod(
  f = "stock", #@ The 'stock' function will look into the cards each kid has available to trade and see if there are any matches with their interests.
  signature = "kid",
  definition = function(k1, k2){
    k1.stock <- getcards(k1, "stock")
    k2.album <- which(getcards(k2, "album") == 0)
    k1.stock <- which(k1.stock > 0)[which(which(k1.stock > 0) %in% k2.album)]
    return(k1.stock)
  }
)


#---


setGeneric(
  name = "swap",
  def = function(kids_s, k1, k2, k1.stock, k2.stock){
    standardGeneric("swap")
  }
)

setMethod(
  f = "swap", #@ The 'swap' function is the biggest one because it must understand how the trade must happen, depending on the behaviour and card availability.
  signature = "kid",
  definition = function(kids_s, k1, k2, k1.stock, k2.stock){
    if(!length(k1.stock) == length(k2.stock)){ #@ This condition understands that there are different amounts of cards of interest from each kid...
      if(length(k1) < length(k2)){ #@... and which is the one with more cards of interest
        k.least <- k1 #@ kid with least cards to give
        k.most <- k2 #@ kid with most cards to give
        k.least.stock <- k1.stock
        k.most.stock <- k2.stock
      }
      else{
        k.least <- k2
        k.most <- k1
        k.least.stock <- k2.stock
        k.most.stock <- k1.stock
      }
      kids_s[[k.least]] <- removecards(kids_s[[k.least]], k.least.stock) #@ This removes the cards k.least gives to k.most
      kids_s[[k.most]] <- putcards(kids_s[[k.most]], k.least.stock) #@ This gives k.least's cards to k.most
      extra <- getcards(kids_s[[k.least]], "stock") #@ This will store k.least's stock
      k.extra <- min(length(k.most.stock) - lenght(k.least.stock), sum(extra)) #@ This will understand if k.least has enough cards in stock to return for what they want
      if(length(k.most.stock) > 1){
        k.most.stock <- sample(k.most.stock, length(k.least.stock) + k.extra, FALSE) #@ If k.least does not have enough cards, this step will remove excessive cards from k.most
      }
      ex <- k.extra
      k.least.remove <- numeric(1)
      while(ex > 0){ #This loop will remove cards from k.least's collection until enough has been given to k.most in exchange for their ones
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
      kids_s[[k.least]] <- putcards(kids_s[[k.least]], k.most.stock) #@ And finally, k.least will receive k.most's cards
      kids_s[[k.most]] <- removecards(kids_s[[k.most]], k.most.stock)
    }
    else{ #@ In this condition, k1 and k2 have the same amount of cards to trade
      kids_s[[k1]] <- putcards(kids_s[[k1]], k2.stock)
      kids_s[[k2]] <- putcards(kids_s[[k2]], k1.stock)
      kids_s[[k1]] <- removecards(kids_s[[k1]], k1.stock)
      kids_s[[k2]] <- removecards(kids_s[[k2]], k2.stock)
    }
    return(kids_s)
  }
)


#---


setGeneric(
  name = "trade",
  def = function(kids_bu){
    standardGeneric("trade")
  }
)

setMethod( 
  f = "trade", #@ The 'trade' function will be the simulation's mechanism to generate the kids' encounters and trade between them
  signature = "bi", #@ It works with the 'bi' class we created before
  definition = function(kids_bu){
    if(length(kids_bu) > 1){
      enc <- encounter(meet()) #@ Using the 'encounter' function with a 'meet' object
      for(i in 1:nrow(enc@order)){
        k1 <- enc@order[1, i]
        k2 <- enc@order[2, i]
        k1.stock <- stock(kids_bu, k1, k2) #@ Setting up
        k2.stock <- stock(kids_bu, k2, k1) #@ the stock!
        
        if(!length(k1.stock) == length(k2.stock)){ #@ Since this is the bilateral behaviour, there must always be equal trades
          k_min <- min(length(k1.stock), min(k2.stock))
          k1.stock <- sample(k1.stock, k_min, FALSE)
          k2.stock <- sample(k2.stock, k_min, FALSE)
        }

        if(!length(k1.stock) == 0 & !length(k2.stock) == 0){
          kids_bu <- swap(kids_bu, k1, k2, k1.stock, k2.stock)
        }
      }
    }
    return(kids_bu)
  }
)


#---


setMethod(
  f = "trade", #@ Wait a second! There is another 'trade' function. Is that a mistake from the developer?
  signature = "uni", #@ Nope! This one affects the 'uni' behaviour. Try to figure for yourself the differences :)
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
