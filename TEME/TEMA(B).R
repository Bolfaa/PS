
#-------------------------------------------------------------------------------


# Definim o functie numita ex1 care primeste doi parametri: n (numarul de iteratii) si p (probabilitatea pentru distributia geometrica)
B1 <- function(n, p) {
  mean <- 0    # Variabila pentru media aproximata
  var <- 0     # Variabila pentru variantia aproximata
  
  # Parcurgem n iteratii
  for (i in 1:n) 
    {
    x <- rgeom(1, p)    # Generam o variabila geometrica cu probabilitatea p
    mean <- mean + x    # Adaugam x la suma mediei
    var <- var + x^2    # Adaugam x^2 la suma variantiei
  }
  
  mean <- mean / n    # Calculam media impartind suma mediei la n
  var <- var / n - mean^2    # Calculam variantia impartind suma variantei la n si scazand media la patrat
  
  exact_mean <- 1 / p    # Media exacta pentru distributia geometrica
  exact_var <- (1 - p) / p^2    # Variantia exacta pentru distributia geometrica
  cat("Media:",mean,"\n");
  cat("Media exacta:",exact_mean,"\n");
  
  cat("Varianta:",var,"\n");
  cat("Varianta exacta:",exact_var,"\n");
}

n <- c(5000, 10000, 100000, 500000)    # Vectorul n care contine numerele de iteratii
p <- c(0.2, 0.6, 0.6, 0.8)    # Vectorul p care contine probabilitatile pentru distributia geometrica
print("--EX1----------------------------------------------------------------");
cat("N:",n[1],"P:",p[1],"---------------\n");
B1(n[1], p[1])
cat("N:",n[2],"P:",p[1],"---------------\n");
B1(n[2], p[1])
cat("N:",n[3],"P:",p[1],"---------------\n");
B1(n[3], p[1])
cat("N:",n[4],"P:",p[1],"---------------\n");
B1(n[4], p[1])
cat("N:",n[1],"P:",p[2],"---------------\n");
B1(n[1], p[2])

# ------------------------------------------------------------------------------

# ex2: teorema limitei centrale --> Student(r)
B2 <- function(r, n, N, z) {
  exact_mean <- 0    # Media exacta pentru distributia Student
  exact_var <- sqrt(r / (r - 2))    # Varianta exacta pentru distributia Student
  upper_bound <- z * exact_var / sqrt(n) + exact_mean    # Limita superioara pentru medie
  
  sum = 0    # Variabila pentru numarul de medii care sunt mai mici sau egale cu limita superioara
  for (i in 1:N) 
    {
    x <- mean(rt(n, r))    # Generam o medie utilizand distributia Student
    if (x <= upper_bound)
    {
      sum <- sum + 1# Verificam daca media este mai mica sau egala cu limita superioara si crestem sum cu 1
    }
  }
  
  probabilitate_exacta <- pnorm(z)    # Probabilitatea exacta pentru distributia Student (este o funcție de distribuție cumulată (CDF) pentru distribuția normală standard.)
  probabilitate_aproximativa <- sum / N    # Probabilitatea aproximata calculata ca raportul sum si N
  eroare_abs <- abs(probabilitate_aproximativa - probabilitate_exacta)   # Eroarea absoluta
  cat("Eroarea absoluta:",eroare_abs,"\n");
}
z=c(-1.5,0,1.5);
N=c(5000,10000,20000);
print("--EX2-----------------------------------------------------------------");
cat("N=",N[1],"Z=",z[1],"\n");
B2(5, 50, N[1], z[1])
cat("N=",N[1],"Z=",z[2],"\n");                                                                        
B2(5, 50, N[1], z[2])
cat("N=",N[1],"Z=",z[3],"\n");
B2(5, 50, N[1], z[3])   
cat("N=",N[2],"Z=",z[1],"\n");    
B2(5, 50, N[2], z[1])
cat("N=",N[2],"Z=",z[2],"\n");
B2(5, 50, N[2], z[2])
cat("N=",N[2],"Z=",z[3],"\n");
B2(5, 50, N[2], z[3])
cat("N=",N[3],"Z=",z[1],"\n");
B2(5, 50, N[3], z[1])
cat("N=",N[3],"Z=",z[2],"\n");
B2(5, 50, N[3], z[2])
cat("N=",N[3],"Z=",z[3],"\n");
B2(5, 50, N[3], z[3])
# ------------------------------------------------------------------------------

B3 <- function(n, p, h, k) # h,k reprezintă limitele intervalului în care se dorește aproximarea probabilității.
  {
  x <- n * p    # Media aproximata pentru distributia binomiala
  sigma <- sqrt(n * p * (1 - p))    # Deviatia standard aproximativa pentru distributia binomiala
  z_h <- (h - x) / sigma    # Calculam z(h) reprezintă valoarea standardizată a limitei inferioare
  z_k <- (k - x) / sigma    # Calculam z(k) reprezintă valoarea standardizată a limitei superioare 
  aprox_prob <- pnorm(z_k) - pnorm(z_h)    # Probabilitatea aproximativa
  print("--EX3----------------------------------------------------------------");
  cat("Probabilitatea aproximativa(Moivre-Laplace):",aprox_prob,"\n");
}

B3(100, 0.3, 20, 40)# h,k reprezintă limitele intervalului în care se dorește aproximarea probabilității.
