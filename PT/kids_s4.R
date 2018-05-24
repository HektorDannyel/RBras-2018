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
  def = function(kids_p, cards_p){ # Aqui nos definimos a estrutura que nossa proxima funcao tera. Os metodos *devem* ter os mesmos parametros!
    standardGeneric("putcards")
  }
)

setMethod(
  f = "putcards", # O nome da funcao generica   
  signature = "kid", # A classe de objetos que a funcao afetara
  definition = function(kids_p, cards_p){ # A funcao, propriamente dito!
    for(card in cards_p){
      if(kids_p@album[card] == FALSE){ #@ Se o card i NAO estiver no album...
        kids_p@album[card] <- TRUE #@ ...coloque-o nele!
      }
      else{ #@ Se o card ESTIVER no album...
        kids_p@collection[card] <- kids_p@collection[card] + 1 #@...coloque-o com os outros!
      }
    }
    return(kids_p)
  }
)


#---


# O procedimento e bem padrao. Tente entender como a funcao 'removecards' funciona sozinho!

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
  def = function(kids_b, total_cards = album_size, pack_size = 5){ # Ao definir um valor padrao para um parametro em sua funcao, sempre o faca na definicao da Generica!
    standardGeneric("buycards")
  }
)

setMethod( 
  f = "buycards", #@ A funcao 'buycards' ira fazer com que cada crianca "va ate uma banca" para "comprar" um pacote novo de cards
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
  f = "getcards", #@ A funcao 'getcards' vai mostrar a colecao ou o album de alguma crianca, dependendo do que for pedido...
  signature = "kid",
  definition = function(kids_g, what){ 
    tryCatch(eval(parse(text = paste0("kids_g@", what))),
             error = function(e) print(paste0("'", what, "' is not a valid parameter."))) #@...e retornara uma mensagem de erro se o input do parametro nao existir.
  }
)


#---


# Esta proxima funcao nao funciona com a classe 'kid'. Tente entender que tipo de objeto ela afeta sozinho!

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
  f = "stock", #@ A funcao 'stock' vai examinar os cards disponiveis para troca de uma crianca e avaliar se a outra crianca precisa de alguma delas.
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
  f = "swap", #@ A funcao 'swap' e a maior de todas pois ela deve entender como a troca deve acontecer, dependendo do comportamento e disponibilidade de cards.
  signature = "kid",
  definition = function(kids_s, k1, k2, k1.stock, k2.stock){
    if(!length(k1.stock) == length(k2.stock)){ #@ Esta condicao compreende que ha diferentes quantidades de cards de interesse para cada crianca...
      if(length(k1) < length(k2)){ #@... e qual das duas possui mais cards de interesse.
        k.least <- k1 #@ crianca com menos cards para troca
        k.most <- k2 #@ crianca com mais cards para troca
        k.least.stock <- k1.stock
        k.most.stock <- k2.stock
      }
      else{
        k.least <- k2
        k.most <- k1
        k.least.stock <- k2.stock
        k.most.stock <- k1.stock
      }
      kids_s[[k.least]] <- removecards(kids_s[[k.least]], k.least.stock) #@ Isto remove os cards que k.least da para k.most
      kids_s[[k.most]] <- putcards(kids_s[[k.most]], k.least.stock) #@ Isto da os cards de k.least para k.most
      extra <- getcards(kids_s[[k.least]], "stock") #@ Isto armazenara o estoque de k.least
      k.extra <- min(length(k.most.stock) - lenght(k.least.stock), sum(extra)) #@ Isto compreendera se k.least possui cards o suficiente em estoque em troca pelo que quer
      if(length(k.most.stock) > 1){
        k.most.stock <- sample(k.most.stock, length(k.least.stock) + k.extra, FALSE) #@ Se k.least nao possuir cards o suficiente, este passo removera cards em excesso de k.most
      }
      ex <- k.extra
      k.least.remove <- numeric(1)
      while(ex > 0){ #Este loop removera cards da colecao de k.least ate que seja dado o suficiente para k.most em troca de seus cards.
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
      kids_s[[k.least]] <- putcards(kids_s[[k.least]], k.most.stock) #@ Finalmente, k.least recebera os cards de k.most.
      kids_s[[k.most]] <- removecards(kids_s[[k.most]], k.most.stock)
    }
    else{ #@ Nesta condicao, k1 e k2 tem a mesma quantidade de cards para trocar
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
  f = "trade", #@ A funcao 'trade' sera o mecanismo de simulacao para gerar os encontros e as trocas entre as criancas.
  signature = "bi", #@ Ela funciona com a classe 'bi' que nos criamos anteriormente
  definition = function(kids_bu){
    if(length(kids_bu) > 1){
      enc <- encounter(meet()) #@ Utilizando a funcao 'encounter' com um objeto 'meet'
      for(i in 1:nrow(enc@order)){
        k1 <- enc@order[1, i]
        k2 <- enc@order[2, i]
        k1.stock <- stock(kids_bu, k1, k2) #@ Definindo 
        k2.stock <- stock(kids_bu, k2, k1) #@ o estoque!
        
        if(!length(k1.stock) == length(k2.stock)){ #@ Ja que este e o comportamento bilateral, devemos sempre ter trocas igualitarias
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
  f = "trade", #@ Espere um segundo! Ha outra funcao 'trade'. Sera que isso foi um erro do desenvolvedor?
  signature = "uni", #@ Negativo! Esta afeta a classe 'uni'. Tente entender as diferencas por conta propria :)
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
