#Partea I
#Exercitiu rezolvat
disc_area = function(N) {
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, -1, 1);
    y = runif(1, -1, 1);
    if(x*x + y*y <= 1)
      N_C = N_C + 1;
  }
  print(pi); #valoarea exacta
  print(abs(pi-4*N_C/N)); #eroare absoluta
  print(abs(pi-4*N_C/N)/abs(pi)); #eroare relativa
  return(4*N_C/N); 
}
#disc_area(100000)

#I1 
volum_sfera=function(N)
{
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, -1, 1);
    y = runif(1, -1, 1);
    z=runif(1,-1,1)
    if(x*x + y*y + z*z <= 1)
      N_C = N_C + 1;
  }
  print(4*pi/3); #valoarea exacta
  print(abs(4*pi/3-8*N_C/N)); #eroare absoluta
  print(abs(4*pi/3-8*N_C/N)/abs(4*pi/3)); #eroare relativa
  return(8*N_C/N); 
}
#volum_sfera(1000)


#Partea II

#Ex rezolvat
MC_integration = function(N) {
  sum = 0;
  for(i in 1:N) {
    u = runif(1, 0, 10);
    sum = sum + exp(-u*u/2);
  }
  return(10*sum/N);
}
#MC_integration(10000)

MC_integr_average= function(k, N) {
  estimates = vector();
  for(i in 1:k)
    estimates[i] = MC_integration(N);
  print(mean(estimates));
  print(sd(estimates));
}
#MC_integr_average(40,10000)

MC_improved_integration = function(N) {
  sum = 0;
  for(i in 1:N) {
    u = rexp(1, 1);
    sum = sum + exp(-u*u)/exp(-u);
  }
  #print(sqrt(pi)/2);
  return(sum/N);
}
#MC_improved_integration(10000)

MC_improved_integr_average= function(k, N) {
  estimates = vector();
  for(i in 1:k)
    estimates[i] = MC_improved_integration(N);
  print(mean(estimates));
  print(sd(estimates));
}

#MC_improved_integr_average(40,10000)

#Ex propuse II
#II1
exII1a=function(N)
{
  sum = 0;
  for(i in 1:N) {
    u = runif(1, 0, pi);
    sum = sum + sin(u)*sin(u);
  }
  print(pi/2)
  return(pi*sum/N);
}

#exII1a(10000)

exII1b=function(N)
{
  sum = 0;
  for(i in 1:N) {
    u = runif(1, 1, 4);
    sum = sum + exp(u);
  }
  return(pi*sum/N);
}

#exII1b(100000)

exII1c=function(a,N)
{
  sum = 0;
  for(i in 1:N) {
    u = runif(1, 0, a);
    sum = sum + 1/sqrt(1-u*u);
  }
  print(pi/2)
  return(a*sum/N);
}

#exII1c(0.999,10000)


#Partea III
#Ex rezolvate
Nr_days = function() {
  nr_days = 1;
  last_errors = c(27, 31);
  nr_errors = 27;
  while(nr_errors > 0) {
    lambda = min(last_errors);
    nr_errors = rpois(1, lambda);
    last_errors = c(nr_errors, last_errors[1]) ;
    nr_days = nr_days + 1;
  }
  return(nr_days);
}
#Nr_days()

MC_nr_days = function(N) {
  s = 0;
  for(i in 1:N)
    s = s + Nr_days();
  return(s/N);
}
#MC_nr_days(10000)

#Ex propuse III
exIII1=function() {
  nr_days = 1;
  last_errors = c(13, 15,9);
  nr_errors = 13;
  while(nr_errors > 0) {
    lambda = mean(last_errors);
    nr_errors = rpois(1, lambda);
    last_errors = c(nr_errors, last_errors[1],last_errors[2]) ;
    nr_days = nr_days + 1;
  }
  return(nr_days);
}
#exIII1()

exIII1_nr_days = function(N) {
  s = 0;
  for(i in 1:N)
    s = s + exIII1();
  return(s/N);
}
#exIII1_nr_days(1000)


#Partea IV
Nr_days_IV = function() {
  nr_days = 1;
  last_errors = c(18, 22,28);
  nr_errors = 18;
  while(nr_errors > 0) {
    lambda = mean(last_errors);
    nr_errors = rpois(1, lambda);
    last_errors = c(nr_errors, last_errors[1],last_errors[2]) ;
    nr_days = nr_days + 1;
  }
  return(nr_days);
}
#Nr_days_IV()

MC_nr_days_IV=function(N)
{
  s=0;
  for(i in 1:N)
    if(Nr_days_IV()>21)
      s=s+1
  return(s/N)
}
#MC_nr_days_IV(100)

#Am pierdut sirul??
MC_prob=function(p_prim,alfa,epsilon)
{
  z=qnorm(alfa/2)
  return(p_prim*(1-p_prim)*(z/epsilon)^2)
}
#MC_prob(0.29,0.01,0.002)


#Exercitii pt activitate
ex_I2 = function(N) {
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, 0, 2);
    y = runif(1, 0, 1.125);
    if(y<=-2*x*x+5*x-2)
      N_C = N_C + 1;
  }
  exact=1.125;
  estimare=2.25*N_C/N;
  print(exact); #valoarea exacta
  print(abs(exact-estimare)); #eroare absoluta
  print(abs(exact-estimare)/abs(exact)); #eroare relativa
  return(estimare); 
}
#ex_I2(10000)


#*care este media pt exIII2?
exIII2_medie = function(N) {
  s = 0;
  for(i in 1:N)
    s = s + exIII2();
  return(s/N);
}
#exIII2_medie(10000)


#------------------------------------------------------------------------------------------------------------------
#Problem de rezolvat

#I2
ex_I2 = function(N) {
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, 0, 2);
    y = runif(1, 0, 1.125);
    if(y<=-2*x*x+5*x-2)
      N_C = N_C + 1;
  }
  exact=1.125;
  estimare=2.25*N_C/N;
  print(exact); #valoarea exacta
  print(abs(exact-estimare)); #eroare absoluta
  print(abs(exact-estimare)/abs(exact)); #eroare relativa
  return(estimare); 
}
#ex_I2(10000)

exII1b <- function(tol) {
  N <- 1000
  estimare_veche <- 0
  estimare_noua <- 0
  
  while(abs(estimare_noua - exp(4) + exp(1)) > tol) {
    sum <- 0
    for(i in 1:N) {
      u <- runif(1, 1, 4)
      sum <- sum + exp(u)
    }
    estimare_veche <- estimare_noua
    estimare_noua <- (4 - 1)*sum/N
    N <- N + 1000
  }
  print(estimare_noua)
}

exII1b(0.0001)

exII1d <- function(N) {
  sum <- 0
  for (i in 1:N) {
    u <- runif(1, 0, 1)
    x <- 1/sqrt(4*u/(1-u))
    sum <- sum + 1/(4*x^2-1)
  }
  print(sum/N)
}

exII1d(1000000)
#----------------------------------------------------------------------------------------------------------
#II2
exII2 = function(lambda,N) {
  sum = 0;
  for(i in 1:N) {
    u = rexp(1, lambda);
    sum = sum + exp(-2*u*u)/(lambda*exp(-lambda*u));
  }
  print(sqrt(pi/8))
  return(sum/N);
}

#exII2(3,50000)
#-------------------------------------------------------------------------------------------------------------
exIII2=function(){
  p=runif(1,0,1)
  if(p<0.25) x=rexp(1,4)
  else x=rexp(1,12)
  return(x)
}
#exIII2();