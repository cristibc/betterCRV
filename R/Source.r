# 1) Fiind data o functie f, introdusa de utilizator, determinarea unei
# constante de normalizare k. In cazul in care o asemenea constanta nu exista,
# afisarea unui mesaj corespunzator catre utilizator.

constantaNormalizare <- function(f){
  tryCatch(
    expr = {
      f_vect <- Vectorize(f)
      f_int <- integrate(f = f_vect, lower = -Inf, upper = Inf)
      k = 1/f_int$value
      return (k)
    },
    
    error = function(mesaj){
      message("Constanta de determinare nu exista\n")
      message(paste(mesaj, "\n"))
      return(NA)
    }
  )
}


# 2) Verificarea daca o functie introdusa de utilizator este
# densitate de probabilitate
densitateProbCheck <- function(f){
  tryCatch(
    expr={
      f_interval <- seq(-99999, 99999, 0.01)
      f_val <- sapply(f_interval, f)
      
      if (all(f_val < 0)){
        error
      }
      f_vect = Vectorize(f)
      f_integ <- integrate(f = f_vect, lower = -Inf, upper = Inf) $ value
      
      if (f_integ < 0.99 && f_integ > 1.001) {
        error
      }
      return (TRUE)
    },
    
    error = function(err) {
      return (FALSE)
    }
  )
  
}

# Test true
# f <- function(x) {
#   x*sin(x^2)
# }

# Test false
# f <- function(x) {
#   (-(x-2)^2)-1
# }

# 4) Reprezentarea grafică a densității și a funcției de repartiție pentru
# diferite valori ale parametrilor repartiției. Ȋn cazul ȋn care funcția de
# repartiție nu este dată ȋntr-o formă explicită(ex. repartiția normală) se
# acceptă reprezentarea grafică a unei aproximări a acesteia.


library(plotly)

ReprezentareGrafica <- function(f, x, y = c()) {
  tryCatch(
    expr ={
      if (length(y) == 0) {
        if (is.function(f))
          f <- sapply(x, f)
        plot(x, f, type="l", col="magenta")
      }
    },
    error = function(mesaj){
      message("Reprezentarea grafica nu a putut fi realizata\n")
      message(paste(mesaj, "\n"))
      return(NA)
    }
  )
}

# 5) Calculul mediei, dispersiei și a momentelor inițiale și centrate pȃnă
# la ordinul 4(dacă există). Atunci cȃnd unul dintre momente nu există, se va
# afișa un mesaj corespunzător către utilizator.

media <- function(f) {
  tryCatch(
    {
      prod <- function(x) {
        x * f(x)
      }
      return (integrate(f = Vectorize(prod), lower = -Inf, upper = Inf) $ value)
    },
    error = function(err) {
      message(paste("Nu se poate calcula media!\n"))
      message(paste(err, "\n"))
      return (NA)
    }
  )
}

dispersia <- function(f){
  medie <-  media(f)
  tryCatch(
    {
      prod <- function(x){
        ((x-medie)^2)*f(x);
      }
      return (integrate(f = Vectorize(prod), lower = -Inf, upper = Inf) $ value)
    },
    error = function(err){
      message(paste("Nu se poate calcula dispersia!\n"))
      message(paste(err))
      return(NA)
    }
  )
}

momentInitial <- function(f,ord){
  tryCatch(
    {
      prod <- function(x){
        ((x)^ord) * f(x)}
      return (integrate(f = Vectorize(prod), lower = -Inf, upper = Inf) $ value)
    },
    error = function(err){
      message(paste("Nu se poate calcula dispersia!\n"))
      message(paste(err))
      return(NA)
    }
  )
}

momentCentrat <- function(f,ord){
  medie <-media(f)
  tryCatch(
    {
      prod <- function(x){
        ((x-medie)^ord) * f(x)}
      return (integrate(f = Vectorize(prod), lower = -Inf, upper = Inf) $ value)
    },
    error = function(err){
      message(paste("Nu se poate calcula momentul centrat!\n"))
      message(paste(err))
      return(NA)
    }
  )
}

momente <- function(f){
  print("Momente intiale de la ordin 1 pana la ordin 4:\n")
  print(momentInitial(f,1))
  print(momentInitial(f,2))
  print(momentInitial(f,3))
  print(momentInitial(f,4))
  print("Momente centrate de la ordin 1 pana la ordin 4:\n")
  print(momentCentrat(f,1))
  print(momentCentrat(f,2))
  print(momentCentrat(f,3))
  print(momentCentrat(f,4))
}

f1 <- function (x){
  if(x > 0 && x < 20)
    return (1/ 20)
  else
    return (0)
}

# Test
# momente(f1)


# 6) Calculul mediei și dispersiei unei variabile aleatoare g(X), unde X are
# o repartiție continuă cunoscută iar g este o funcție continuă
# precizată de utilizator.

medie_calcul <- function(g,f){
  tryCatch(
    expr = {
      produs_medie <- function(x){
        return(g(x)*f(x))
      }
      medie <- integrate(Vectorize(produs_medie), lower = 0, upper = Inf) $ value
      return(medie)
    },
    error = function(mesaj){
      message("Calculul mediei nu a putut fi realizat!\n")
      message(paste(mesaj, "\n"))
      return(NA)
    }
  )
}

dispersie_calcul <- function(g,f){
  tryCatch(
    expr = {
      produs_dispersie <- function(x){
        return( (g(x)-medie_calcul(g,f))^2 * f(x) )
      }
      dispersie <- integrate(Vectorize(produs_dispersie), lower = 0, upper = Inf) $ value
      return(dispersie)
    },
    error = function(mesaj){
      message("Calculul dispersiei nu a putut fi realizat!\n")
      message(paste(mesaj, "\n"))
      return(NA)
    }
  )
}

# 7) Crearea unei funcții P care permite calculul diferitelor tipuri de
# probabilități asociate unei variabile aleatoare continue(similar funcției
# P din pachetul discreteRV)

meniuProbabilitateContinua <- function(){
  tryCatch(
    expr = {
      cat("Introduceti numarul corespunzator distributiei continue dorite.\n")
      cat("1 - Distributie normala\n")
      cat("2 - Distributie T/Student\n")
      cat("3 - Distributie Exponentiala\n")
      cat("4 - Distributie Gamma\n")
      cat("5 - Distributie X^2\n")
      choice <- as.numeric(readline(prompt ="Introduceti alegerea: "))
      
      switch(EXPR = choice,
             {
               x = as.numeric(readline(prompt = "X: "))
               mean = as.numeric(readline(prompt = "Mean: "))
               sd = as.numeric(readline(prompt = "SD: "))
               return(pnorm(x, mean, sd))
             },
             {
               x = as.numeric(readline(prompt = "X: "))
               df = as.numeric(readline(prompt = "DF: "))
               return(pt(x, df))
             },
             {
               x = as.numeric(readline(prompt = "X: "))
               rate = as.numeric(readline(prompt = "Rate: "))
               return(pexp(x, rate))
             },
             {
               x = as.numeric(readline(prompt = "X: "))
               shape = as.numeric(readline(prompt = "Shape: "))
               rate = as.numeric(readline(prompt = "Rate: "))
               return(pgamma(x, shape, rate))
             },
             {
               x = as.numeric(readline(prompt = "X: "))
               df = as.numeric(readline(prompt = "DF: "))
               return(pchisq(x, df))
             },
             
      )
    },
    error = function(mesaj){
      message("Probabilitatea nu a putut fi calculata!\n")
      message(paste(mesaj, "\n"))
      return(NA)
    }
  )
}

# Test

# meniuProbabilitateContinua()

# 8) Afișarea unei “fișe de sinteză” care să conțină informații de bază despre
# respectiva repartiție(cu precizarea sursei informației!). Relevant aici ar
# fi să precizați pentru ce e folosită ȋn mod uzual acea repartiție, 
# semnificația parametrilor, media, dispersia etc.


uniforma <- function(){
  cat("Aplicatii: - situatii de testare ale ipotezelor, cazuri de esantionare aleatorie, 
        finante")
  cat("Notatie: U(a,b)\n")
  cat("PDF: 1/(b-a) x∈[a,b] , 0 in rest\n")
  cat("CDF: 0 cand x<a, (x-a)/(b-a) cand x∈[a,b], 1 cand x>b")
  cat("Media: 1/2 * (a+b)\n")
  cat("Mediana: 1/2 * (a+b)\n")
  cat("Variatie: 1/12 * ((b-a)^2)\n")
}

normala <- function(){
  cat("Aplicatii: - fizica, biologie, statistica a rezultatelor ")
  cat("Notatie: N( μ,σ^2)\n")
  cat("PDF: 1 / ( σ * sqrt (π * 2)))*( exp (1)
       ^(( -(x - μ) ^2) /(2 * σ ^ 2))) \n")
  cat("CDF: (1 / 2) * (1 + erf ((x - μ) / ( σ * sqrt (2)))) \n")
  cat("Media: μ \n")
  cat("Mediana: μ \n")
  cat("Variatie: σ^2 \n")
}

exponentiala <- function(){
  cat("Aplicatii: -  estimarea evenimentelor care vor avea loc in viitor\n")
  cat("PDF: λ * exp (1) ^(- λ * x) \n")
  cat("CDF: 1 - exp (1) ^(- λ * x) \n")
  cat("Media: 1/λ \n")
  cat("Mediana: ln2/λ \n")
  cat("Variatie: 1/(λ^2) \n")
}

cauchy <- function(){
  cat("PDF: (1 / π) * arctan ((x - x0) / y) + (1/2)) \n")
  cat("CDF: 1 / (π * y * (1 + (x - x0) / y) ^2) \n")
  cat("Media: nedefinita \n")
  cat("Mediana: x0 \n")
  cat("Variatia: nedefinita \n")
}

chisquared <-  function(){
  cat("PDF: 1 / (2^(k/2)) * Γ * (k/2) \n")
  cat("CDF: ( 1 / (Γ * (k/2)) * γ( k/2, x/2)) \n")
  cat(" Media: k \n")
  cat("Mediana: aprox. k * ((1 - (2/9k))^3) \n")
  cat("Varianta: 2*k \n")
}


sinteza <- function(){
  tryCatch(
    expr = {
      cat("Introduceti numarul optiunii alese: ")
      cat("1. Repartitie uniforma\n")
      cat("2. Repartitie normala\n")
      cat("3. Repartitie exponentiala\n")
      cat("4. Repartitie Cauchy\n")
      cat("5. Repartitie ChiSquared\n")
      choice <- as.numeric(readline(prompt = "Repartitia aleasa este: "))
      
      switch( EXPR = choice,
              {
                uniforma()
              },
              
              {
                normala()
              },
              
              {
                exponentiala()
              },
              
              {
                cauchy()
              },
              
              {
                chisquared()
              }
      )
    },
    error = function(mesaj){
      message("Selectia nu exista!\n")
      message(paste(mesaj, "\n"))
      return(NA)
    }
  )
}

# Test
# sinteza()

# 9) Generarea a n valori(unde n este precizat de utilizator!) dintr-o
# repartiție de variabile aleatoare continue( solicitați material suport 
# pentru partea de simulare).

metoda_inversa_discret <- function(x) {
  tryCatch(
    expr = {
      U  <- runif(1)
      if(U <= x[1]){
        return(1)
      }
      
      for(j in 2:length(x)) {
        p1 = sum(x[1:(j-1)])
        p2 = sum(x[1:j])
        if(p1 < U && U <= p2 ) {
          return(j)
        }
      }
    },
    error = function(mesaj){
      message("Metoda inversa nu a putut fi aplicata!\n")
      message(paste(mesaj, "\n"))
      return(NA)
    }
  )
}
generate_ex <- function(n){
  for(i in seq_len(n)){
    n[i] <- metoda_inversa_discret(x)
  }
  
  return(n)
}

# Exemplu
# x <- c(1/3, 1/30, 2/15, 7/30, 4/15)
# generate_ex(10)


# 12) Construirea sumei și diferenței a două variabile aleatoare continue
# independente (folosiți formula de convoluție)
suma <- function(f,g){
  tryCatch(
    expr = {
      function(z){
        integrala <- function(x){
          return(f(x)*g(z-x))}
        suma <- integrate(Vectorize(integrala), lower = -Inf, upper = Inf) $ value
        return(suma)
      }
    },
    error = function(mesaj){
      message("Suma nu a putut fi calculata!\n")
      message(paste(mesaj, "\n"))
      return(NA)
    }
  )
}

diferenta <- function(f,g){
  tryCatch(
    expr = {
      function(z){
        integrala <- function(x){
          return(f(x)*g(x-z))
        }
        diferenta <- integrate(Vectorize(integrala), lower = -Inf, upper = Inf) $ value
        return(diferenta)
      }
    },
    error = function(mesaj){
      message("Diferenta nu a putut fi calculata!\n")
      message(paste(mesaj, "\n"))
      return(NA)
    }
  )
}

#TESTARE
# f <- function(x) dnorm(x, 1.7, 0.4)
# g <- function(x) dnorm(x, 1.5, 0.55)
# 
# s <- Vectorize(suma (f,g))
# d <- Vectorize(diferenta (f,g))
# 
# plot (s, from = -10 , to =10, col =" magenta ", type ="l", lwd = 3)
# plot (d, from = -10 , to =10, col =" magenta ", type ="l", lwd = 3)


