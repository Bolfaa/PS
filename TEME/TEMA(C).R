
#-------------------------------------------------------------------------------

cat("\n C1------------------------------------------------------------------  \n")

#estimeaza volumul folosind o simulare monte carlo
# Definim funcția de verificare a punctului
check_point <- function(x1, x2, x3, a) {  #verifica daca punctul respecta o anumita conditie,daca conditia s a indeplinit,atunci punctul e in solid 
  x3 >= x1^2+x2^2 && x3 <= a
}

# Funcția pentru estimarea volumului
volum_estimat <- function(a, num_points) {
  count_inside <- 0
  # Generăm puncte aleatoare în interiorul cubului
  for (i in 1:num_points) {
    x1 <- runif(1, -sqrt(a), sqrt(a))  # Coordonata x1 între -sqrt(a) și sqrt(a) ,1 reprezinta numaru de valori
    x2 <- runif(1, -sqrt(a), sqrt(a))  # Coordonata x2 între -sqrt(a) și sqrt(a)
    x3 <- runif(1, 0, a)               # Coordonata x3 între 0 și a
    
    if (check_point(x1, x2, x3, a)) {  # Verificăm dacă punctul este în interiorul conului
      count_inside <- count_inside + 1
    }
  }
  
  volume <- count_inside / num_points * (2 * sqrt(a))^2 * a # Calculăm volumul estimat
  return(volume)
}

# Funcția de executare a cerinței C1
C1 = function(a, sample) {
  
  volume <- volum_estimat(a, sample)  # Estimăm volumul
  exact_volume <- (pi * a^2) / 2             # Calculăm volumul exact
  
  # Calculăm eroarea relativă
  relative_error <- abs(volume - exact_volume) / exact_volume
  
  # Afișăm rezultatele
  cat("Pentru a =", a, ", dimensiunea eșantionului =", sample,"\n")
  cat("Volum estimat:", volume,"\n")
  cat("Volum exact:", exact_volume,"\n")
  cat("Eroare relativă:", relative_error,"\n")
  cat("\n")
}

#testam pt fiecare exemplu dat in exercitiu

sample=c(10000,20000,50000);
a=c(2,4,10);
for(i in 1:3){
  C1(a[1],sample[i]);
  C1(a[2],sample[i]);
  C1(a[3],sample[i]);
}

#-------------------------------------------------------------------------------

cat("\n C2------------------------------------------------------------------ \n")
cat("\n")

# Definim funcția de verificare a punctului în interiorul patrulaterului
check_point <- function(x, y) {
  if (x >= 0 && y >= 0 && 3*y <= x + 6 && y <= 12 - 3*x) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Funcția pentru estimarea ariei patrulaterului folosind metoda Monte Carlo
estimate_area <- function(num_points) {
  count_inside <- 0
  
  # Determinăm intervalul rectangular care include toate punctele interioare ale patrulaterului T
  a <- 0  # Coordonata x minimă
  b <- 6  # Coordonata x maximă
  c <- 0  # Coordonata y minimă
  d <- 4  # Coordonata y maximă
  
  for (i in 1:num_points) {
    # Generăm puncte aleatoare în interiorul zonei rectangulare
    x <- runif(1, a, b)  # Coordonata x între a și b
    y <- runif(1, c, d)  # Coordonata y între c și d
    
    if (check_point(x, y)) {  # Verificăm dacă punctul este în interiorul patrulaterului
      count_inside <- count_inside + 1
    }
  }
  
  area_rectangle <- (b - a) * (d - c)  # Calculăm arie dreptunghiului
  area_estimated <- count_inside / num_points * area_rectangle  # Calculăm arie estimată
  
  return(area_estimated)
}

C2 <- function(sample_size) {
  estimated_area <- estimate_area(sample_size)
  cat("Aria estimată a patrulaterului T este:", estimated_area, "\n")
}

C2(20000);

#-------------------------------------------------------------------------------


cat("\n C3------------------------------------------------------------------ \n")
cat("\n")
cat("PUNCTUL A------------------------------------------------------- \n")
cat("\n")
integrala_a <- function(x) { #calculeaza valoarea din interiorul integralei
  (x + 1) / sqrt(4 - x^2)
}

# Calculăm valoarea exactă a integralei
exact_integral <- integrate(integrala_a, -1, 1)$value #returneaza valoarea numerica a integralei
cat("Valoarea exactă a integralei este:", exact_integral, "\n")

# Estimăm valoarea aproximată folosind metoda Monte Carlo
C3_A <- function(N) {
  sum <- 0
  for (i in 1:N) {
    u <- runif(1, -1, 1)  # Generăm un număr aleator între -1 și 1
    sum <- sum + integrala_a(u)
  }
  result <- 2 * sum / N  # Aproximăm integrala folosind formula ariei dreptunghiului
  cat("Valoarea aproximată a integralei este:", result, "\n")
}

# Apelăm funcția MC_integration_a cu N = 2000
C3_A(200000)


cat("\n")
cat("PUNCTUL B------------------------------------------------------- \n")
cat("\n")
#LA TOATE FACEM LA FEL
integrala_b <- function(x) {
  1 / (x^2 + 4)
}

# Calculăm valoarea exactă a integralei
exact_integral <- integrate(integrala_b, lower = -Inf, upper = 0)$value
cat("Valoarea exactă a integralei este:", exact_integral, "\n")

# Estimăm valoarea aproximată folosind metoda Monte Carlo
C3_b <- function(N) {
  sum <- 0
  for (i in 1:N) {
    u <- -runif(1, 0, 1)  # Generăm un număr aleator negativ între 0 și 1
    sum <- sum + integrala_b(u)
  }
  result <- 2 * sum / N  # Aproximăm integrala folosind formula ariei dreptunghiului
  cat("Valoarea aproximată a integralei este:", result, "\n")
}

# Apelăm funcția MC_integration_b cu N = 2000
C3_b(200000)

cat("\n")
cat("PUNCTUL C------------------------------------------------------- \n")
cat("\n")

integrala_C <- function(x) {
  x * exp(x)
}

# Calculăm valoarea exactă a integralei
exact_integral <- integrate(integrala_C, lower = -Inf, upper = 0)$value
cat("Valoarea exactă a integralei este:", exact_integral, "\n")

# Estimăm valoarea aproximată folosind metoda Monte Carlo
C3_c <- function(N) {
  sum <- 0
  for (i in 1:N) {
    u <- -runif(1, 0, 1)  # Generăm un număr aleator negativ între 0 și 1
    sum <- sum + integrala_C(u)
  }
  result <- 2 * sum / N  # Aproximăm integrala folosind formula ariei dreptunghiului
  cat("Valoarea aproximată a integralei este:", result, "\n")
}

# Apelăm funcția MC_integration_c cu N = 2000
C3_c(200000)
#-------------------------------------------------------------------------------






#-------------------------------------------------------------------------------
cat("\n C4------------------------------------------------------------------ \n")
m = 100000  # Numărul inițial de conturi false
n = 500  # Numărul de conturi false adăugate zilnic
p = 0.5  # Probabilitatea de adăugare a unui cont fals (distribuție binomială)
q = 0.1  # Probabilitatea de dezactivare a unui cont fals

cat("\n")
print("A: ")
cat("\n")

# Funcția pentru estimarea numărului mediu de zile
C4_A = function() {
  fake_accounts = m
  
  nr_days = 0
  
  while (fake_accounts > 0) {
    new_fake_accounts = rbinom(1, n, p)  # Numărul de conturi false adăugate zilnic
    deactived_accounts = rbinom(1, fake_accounts + new_fake_accounts, q)  # Numărul de conturi dezactivate
    fake_accounts = fake_accounts - deactived_accounts
    nr_days = nr_days + 1
  }
  
  cat("Numărul de zile:", nr_days)
  return(nr_days)
}

C4_A()


cat("\n")
cat("\n")
print("B: ")
cat("\n")

# Funcția pentru estimarea probabilității că după 40 de zile să existe cel mult 50000 de conturi false
C4_B = function(N) {
  
  s = 0
  for (i in 1:N) {
    fake_accounts = m
    
    nr_days = 0
    
    while (fake_accounts > 0) {
      new_fake_acc = rbinom(1, n, p)  # Numărul de conturi false adăugate zilnic
      deactived_accounts = rbinom(1, fake_accounts + new_fake_acc, q)  # Numărul de conturi dezactivate
      fake_accounts = fake_accounts - deactived_accounts
      nr_days = nr_days + 1
      if (nr_days == 40 && fake_accounts <= 50000) {
        s = s + 1
        break
      }
    }
  }
  
  cat("Probabilitate:", s / N)
}

C4_B(50000)


cat("\n")
print("C")
cat("\n")
target_prob = 0.99
error_tolerance = 0.01

MC_estimation = function() {
  fake_acc = m
  nr_days = 0
  
  while (fake_acc > 0) {
    new_fake_acc = rbinom(1, n, p)  # Numărul de conturi false adăugate zilnic
    deact = rbinom(1, fake_acc + new_fake_acc, q)  # Numărul de conturi dezactivate
    fake_acc = fake_acc - deact
    nr_days = nr_days + 1
  }
  
  return(nr_days)
}

N = 10000  # Numărul de simulări pentru estimare
results = replicate(N, MC_estimation())  # Efectuăm simulările

estimated_prob = mean(results >= 40)  # Probabilitatea estimată
lower_bound = quantile(results >= 40, (1 - target_prob) / 2)  # Limita inferioară a intervalului de încredere
upper_bound = quantile(results >= 40, 1 - (1 - target_prob) / 2)  # Limita superioară a intervalului de încredere

error = upper_bound - estimated_prob  # Eroarea estimată

cat("Estimare probabilitate:", estimated_prob, "\n")
cat("Limită inferioară a intervalului de încredere:", lower_bound, "\n")
cat("Limită superioară a intervalului de încredere:", upper_bound, "\n")
cat("Eroare:", error, "\n")


