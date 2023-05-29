ex1=function(x,p)
{
  k=length(x)
  a=runif(1,0,1)
  suma=0
  for(i in 1:k)
  {
    suma=suma+p[i]
    if(suma-p[i]<=a && a<suma)
      print(x[i])
  }
}

ex1(c(3,7,10,14),c(0.2,0.3,0.4,0.1))
