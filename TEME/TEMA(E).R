#Functie din saptamana 13
# Functie pentru calculul intervalului de incredere pentru o distributie normala
zconfidence_interval = function(mean, sigma, n, alpha) {
  z_critic = -qnorm(alpha/2) #pt a calcula valorile critice
  L=mean - (z_critic * sigma/sqrt(n));
  R=mean + (z_critic * sigma/sqrt(n));
  interval <- c(L,R);
  return(interval)
}

#-------------------------------------------------------------------------------
# E1
E1 <- function() {
  # Datele initiale
  n <- 10  # numarul de masuratori
  mean <- 138  # media masuratorilor
  sigma <- 11  # deviatia standard a masuratorilor
  
  # Nivel de incredere de 90%
  alpha_90 <- 0.1
  interval_90 <- zconfidence_interval(mean, sigma, n, alpha_90)
  
  # Nivel de incredere de 95%
  alpha_95 <- 0.05
  interval_95 <- zconfidence_interval(mean, sigma, n, alpha_95)
  
  # Nivel de incredere de 99%
  alpha_99 <- 0.01
  interval_99 <- zconfidence_interval(mean, sigma, n, alpha_99)
  
  # Afisarea intervalelor de incredere
  cat("Intervalul de incredere de 90%:", interval_90, "\n")
  cat("Intervalul de incredere de 95%:", interval_95, "\n")
  cat("Intervalul de incredere de 99%:", interval_99, "\n")
}


#-------------------------------------------------------------------------------
# E2
E2 <- function() {
  # Datele initiale
  n <- 256  # dimensiunea eșantionului
  mean <- 18  # media eșantionului
  variatie <- 1.44  # dispersia eșantionului
  
  # Nivel de încredere de 95%
  alpha_95 <- 0.05
  interval_95 <- zconfidence_interval(mean, sqrt(variatie), n, alpha_95)
  
  # Afisarea intervalului de încredere
  cat("Intervalul de incredere de 95%:", interval_95, "\n")
}


#-------------------------------------------------------------------------------


# E3
E3 = function(prim, p0, n, alfa, tip) {
  scor_z = (prim - p0) / sqrt(p0 * (1 - p0) / n)
  
  if (tip == "left") {
    z_critic <- qnorm(alfa)
    if (scor_z < z_critic)
      print("Ho se respinge și Ha se acceptă")
    else
      print("Ho nu se poate respinge")
  }
  
  if (tip == "symmetric") {
    z_critic <- qnorm(alfa / 2)
    if (abs(scor_z) < abs(z_critic))
      print("Ho se respinge și Ha se acceptă")
    else
      print("Ho nu se poate respinge")
  }
  
  if (tip == "right") {
    z_critic <- qnorm(1 - alfa)
    if (scor_z > z_critic)
      print("Ho se respinge și Ha se acceptă")
    else
      print("Ho nu se poate respinge")
  }
  
  print(scor_z)
  print(z_critic)
}

# Apelarea functiilor pentru rezolvarea problemelor
E1()
E2()
E3(100/153, 0.12, 153, 0.01, "right")
E3(17/153, 0.12, 153, 0.01, "right")


#-------------------------------------------------------------------------------