######################################################################################################
######################                  Potrzebna biblioteka                    ######################      
######################################################################################################


install.packages("flifo")
library(flifo)


######################################################################################################
######################                        Tabu Search                       ######################      
######################################################################################################






TabuSearch <- function(f, x, d, nb_count, tabu_list_len, max_iter){
  # f - function
  # x - punkt początkowy
  # d - parametr który określa zakres sąsiedztwa x
  # nb_count - ilość sąsiadów, których sprawdzamy
  # tabu_list_len - długość listy tabu
  # max_iter - ilość iteracji
  start_time <- Sys.time()
  tabu_list <- flifo::fifo(max_length = tabu_list_len) # tworzymy listę tabu
  candidate_list <- rep(list(NA),nb_count) # lista kandydatów
  candidate_list_outside_tabu <- rep(list(NA),nb_count) # lista kandydatów spoza listy tabu
  in_neigbourhood <- FALSE # określa czy kandydat jest w sąsiedztwie
  
  
  ### Tworzymy listę z wynikami
  out <- list(x_hist = matrix(NA, nrow = max_iter, ncol = 2), 
              f_hist = rep(NA, max_iter), 
              x_opt = x, 
              f_opt = f(x),
              t_eval = NA)
  
  out$x_hist[1, ] <- x
  out$f_hist[1] <- f(x)
  
  
  for(u in 2 : max_iter){
    
    
    ### zgodnie z zaleceniami z książki Metaheuristics for Hard Optimization tworzymy skończone sąsiedztwo
    for(j in 1:nb_count){ 
      candidate_list[[j]] <- x + runif(2, min = -d, max = d) # losowo dobieramy sąsiadów
    }
    
    
    place <- 1
    
    ### sprawdzamy czy znalezieni kandydaci znajdują się na liście tabu i w sąsiedztwach danych wartości
    for(i in candidate_list){
      
      
      if(!flifo::is.empty(tabu_list)){ # jeśli lista tabu nie jest pusta
        for(z in tabu_list){
          # czy kandydat jest w sąsiedztwie?
          if((i[1] <= z[1]+d) && (i[1]>= z[1]-d) && (i[2] <= z[2]+d) && (i[2]>= z[2]-d)){
            in_neigbourhood <- TRUE
          }
        }
      }
      if(in_neigbourhood == FALSE){
        candidate_list_outside_tabu[[place]] <- i # jeśli tak to dodajemy go listy gkandydatów spoza tabu
      }
      
      
      place <- place + 1 
      in_neigbourhood <- FALSE
    }
     
    ### dodajemy nowy punkt na listę tabu
    x_tabu <- x # do wsadzenia do listy tabu
    if(length(tabu_list)==tabu_list_len){ # sprawdzamy czy stos jest pełen
      pop(tabu_list) # usuwamy pierwszy element z kolejki
      push(tabu_list,x_tabu) # wrzucamy nowy
    }else{ # jeśli nie jest pełen to tylko wrzucamy
      push(tabu_list,x_tabu)
    }
    
    ### Wyznaczamy nowy punkt
    #########################
    calculated <- lapply(candidate_list_outside_tabu, f) # wyliczamy wartości funkcji w każdym punkcie
    x <- unlist((candidate_list[which.min(calculated)]),use.names=FALSE) # znajdujemy punkt z najniższą wartością
    
    
    ### Jeśli algorytm nie znalazł żadnych kandydatów spoza listy tabu, to kończymy działanie
    #########################################################################################
    if(is.null(x)){
      print(paste("Nie znaleziono żadnego kandydata spoza listy tabu w iteracji ", u))
      return(out)
    }
    
    
    ### resetujemy liste z kandydatami spoza listy tabu
    ###################################################
    candidate_list_outside_tabu <- rep(list(NA),nb_count)
    
  
    
    ### zapisujemy wyniki
    #####################
    out$x_hist[u,] <- x
    out$f_hist[u]  <- f(x)
    
    # sprawdzamy ekstremum
    ######################
    if(f(x) < out$f_opt){
      out$x_opt <- x
      out$f_opt <- f(x)
    }
  }
  
  out$t_eval <- Sys.time() - start_time
  return(out)
}


######################################################################################################
######################                         Wywołanie                        ######################      
######################################################################################################


# Rosenbrock function
ObjFun <- function(x){
  out <- (1-x[1]^2)^2 + 100 * (x[2] - x[1]^2)^2
  return(out)
}

# parametry
x0 <- c(3,4)
max_iter <- 1000
d <- 0.4
f <- ObjFun
nb_count <- 500
tabu_list_len <- 50




ts <- TabuSearch(ObjFun, x0, d,nb_count, tabu_list_len, max_iter)

###################################################################################################
###################################################################################################
###################################################################################################
