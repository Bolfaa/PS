#############################################################################
#I

density_exponential = function(lambda, n, a) {
  x = seq(0, a, length=n);
  y = dexp(x, lambda);
  plot(x, y, type = 'l');
}
#density_exponential(2,10000,8)

#I1
density_gamma = function(alfa,lambda, n, a) {
  x = seq(0, a, length=n);
  y = dgamma(x,alfa,lambda)
  plot(x, y, type = 'l');
}
#density_gamma(3,4,10000,8)

#I2
density_student=function(r,n,a)
{
  x=seq(-a,a,length=n) #luam interval [-a,a]
  y=dt(x,r)
  plot(x,y,type='l')
}

#density_student(40,5000,6)


###########################################################################
#II

#ex 1
LLN_Poisson1 = function(lambda, n) {
  sum = 0;
  for(i in 1:n) {
    u = rpois(1, lambda);
    sum = sum + u;
  }
  return(sum/n);
}

#LLN_Poisson1(2,10000) 
#rezultatele vor fi diferite de fiecare data cand rulez

#ex 2
LLN_Poisson2 = function(lambda, n) {
  return(mean(rpois(n, lambda)));
}

#LLN_Poisson2(2,1000000)

#ex 3
LLN_Gamma = function(alfa, lambda, n) {
  #print(alfa/lambda) #pentru a verifica daca aproximarea din return este buna
  return(mean(rgamma(n, alfa, lambda)));
}
#LLN_Gamma(2,5,10000)

#Ex propuse II

#II1a
LLN_Exp = function(lambda, n) {
  #print(1/lambda) #pentru a verifica daca aproximarea din return este buna
  return(mean(rexp(n,lambda)));
}
#LLN_Exp(2.5,10000)

#II1b
LLN_Binomial = function(m,p, n) {
  #print(m*p) #pentru a verifica daca aproximarea din return este buna
  return(mean(rbinom(n,m,p)));
}
#LLN_Binomial(35,0.45,10000)


############################################################################
#III

#Ex
CLT_Poisson = function(lambda, n, N, z) {
  expectation = lambda;
  st_dev = sqrt(lambda);
  upper_bound = z * st_dev/sqrt(n) + expectation;
  sum = 0;
  for(i in 1:N) {
    x_n = mean(rpois(n, lambda));
    if(x_n <= upper_bound) {
      sum = sum + 1;
    }
  }
  #print(pnorm(z));
  return(sum/N);
}
#CLT_Poisson(2.3,40,1000,2)

#Exercitii propuse

#III1

CLT_Exp = function(lambda, n, N, z) {
  expectation = 1/lambda;
  st_dev = sqrt(1/lambda^2);
  upper_bound = z * st_dev/sqrt(n) + expectation;
  sum = 0;
  for(i in 1:N) {
    x_n = mean(rexp(n, lambda));
    if(x_n <= upper_bound) {
      sum = sum + 1;
    }
  }
  return(sum/N);
}
#CLT_Exp(3,50,1000,1)

############################################################################
#IV

#ex rezolvat modificat
binomial_probability = function(n, p, k) {
  expectation = n*p;
  variance = n*p*(1 - p);
  standard_deviation = sqrt(variance);
  q1 = (k - 0.5 -expectation)/standard_deviation;
  q2 = (k + 0.5 -expectation)/standard_deviation;
  print(dbinom(k,n,p))
  return(pnorm(q2)-pnorm(q1));
}
#binomial_probability(33,0.2,7)

#IV1
binomial_probability1 = function(n, p, k) {
  expectation = n*p;
  variance = n*p*(1 - p);
  standard_deviation = sqrt(variance);
  q1 = (k - 0.5 -expectation)/standard_deviation;
  #q2 = (k + 0.5 -expectation)/standard_deviation;
  print(sum(dbinom(0:(k-1),n,p)))
  return(pnorm(q1));
}
#binomial_probability1(40,0.35,20)


#IV2
binomial_probability2 = function(n, p, k) {
  expectation = n*p;
  variance = n*p*(1 - p);
  standard_deviation = sqrt(variance);
  q1 = (k + 0.5 -expectation)/standard_deviation;
  #q2 = (k + 0.5 -expectation)/standard_deviation;
  print(sum(dbinom((k+1):n,n,p)))
  return(1-pnorm(q1));
}
#binomial_probability2(40,0.35,20)

#I3
density_Normal=function(expectation,variance,n,a)
{
  x=seq(expectation-a,expectation+a,length=n) 
  y=dnorm(x,expectation,variance)
  plot(x,y,type='l')
}
#density_Normal(3,1,10000,6)

##############################################################################
#Alea cu albastru de rezolvat



#II2
II=function(n,r)
{
  print (mean(rt(n,r)));
}
II(10000,3)
II(100000,5)
#II(1000000,4)

#III2
III = function(alfa,lambda, n, N, z) {
  expectation = alfa/lambda;
  st_dev = sqrt(alfa/lambda^2);
  upper_bound = z * st_dev/sqrt(n) + expectation;
  sum = 0;
  for(i in 1:N) {
    x_n = mean(rgamma(n, alfa,lambda));
    if(x_n <= upper_bound) {
      sum = sum + 1;
    }
  }
  print(pnorm(z));
  print(sum/N);
}
III(3,3,50,10000,0)
III(3,3,50,5000,-1.5)
III(3,3,50,20000,1.5)



