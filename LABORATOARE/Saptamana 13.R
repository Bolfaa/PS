file_zconfidence_interval = function(filename, alpha) {
  data = as.numeric(scan(filename))
  n = length(data)
  x_bar = mean(data)
  
  z_alpha_half = abs(qnorm(alpha/2))
  sigma = 10
  margin_of_error = z_alpha_half * sigma / sqrt(n)
  lower = x_bar - margin_of_error
  upper = x_bar + margin_of_error
  return (c(lower, upper))
}


alpha = 0.05

exIII.4=function(alfa, filename)
{
  x = scan(filename);
  n = length(x)
  sample_mean = mean(x)
  s=sd(x)
  t_critic=qt(1-alfa/2,n-1)
  L=sample_mean-t_critic*s/sqrt(n)
  R=sample_mean+t_critic*s/sqrt(n)
  interval=c(L,R)
  print(interval)
}
exIII.4(0.05,"history.txt")
exIII.4(0.01,"history.txt")


p0 = 0.1
n = 150
x = 20
p = x/n
alpha = 0.05
prob = pbinom(q = x-1, size = n, prob = p0)
if (prob <= alpha) {
  print("Procentul componentelor defecte este semnificativ mai mare decat 10%.")  
} else {
  print("Nu exista suficiente evidente pentru a afirma ca procentul componentelor defecte este semnificativ mai mare decat 10%.")
}