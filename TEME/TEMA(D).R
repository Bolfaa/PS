#-------------------------------------------------------------------------------

D1=function(x,k)
{
  n=length(x);
  limita= n/2 + 1;#numar de aparitii
  print("Limita:")
  print(limita);
  print("----")
  for (i in 1:k)
  {
    index=sample(1:n, 1);#luam random un index
    print("Index:")
    print(index); #afisam index
    print("---")
    element= x[index]; #luam elementul de la acea pozitie
    count=0;#nr de aparitii
    for (j in 1:n) 
      if (x[j] == element) 
        count = count + 1; # verificam de cate ori apare
    if (count >= limita) #returneaza cand gaseste unu
      print(element);#sau return
  }
  if(count<limita)
  print("x nu are M-element");
}
x = c(50, 50, 19, 19, 50, 50, 19, 20, 90, 50, 50, 50, 50)
k = 4;# PUNCTU B
print( "D1:") 
D1(x,k)
print( "---")
#-------------------------------------------------------------------------------

#compileaza, nu da rezultatul bun
D2 <- function(i, A) {
  n <- length(A)
  if (n == 1) {
    return(A)
  } else {
    z <- sample(A, 1)  # Alegem aleator un element din A
    A_less <- A[A < z]  # Elementele mai mici dec??t z
    A_greater <- A[A > z]  # Elementele mai mari dec??t z
    
    if (length(A_less) > i) {
      return(D2(i, A_less))
    } else if (n > i + length(A_greater)) {
      return(z)
    } else {
      return(D2(i - n + length(A_greater), A_greater))
    }
  }
}

A <- c(1, 2, 3, 4, 5)
i <- 0
result <- D2(i, A)
print( "D2:") 
print(result)  # Output: 3
print( "---") 

#-------------------------------------------------------------------------------
D3 <- function(S, a) {
  n <- length(S)    # Dimensiunea vectorului S
  m <- floor(a * log(n))    # Calcul??m dimensiunea submul??imii
  print(m)
  if (m < 1)
    return("Nu e buna dimensiunea, e prea mica")    # Verific??m dac?? dimensiunea este prea mic??
  if (m > n)
    return("Dimensiunea este peste size-ul lui S")    # Verific??m dac?? dimensiunea este mai mare dec??t dimensiunea lui S
  Sir_nou <- sample(S, m)    # Extragem o submul??ime aleatoare din S
  print(Sir_nou)
  Sir_sortat <- sort(Sir_nou)    # Sort??m submul??imea
  index <- ceiling(m/2)    # Calcul??m indexul pentru mediana
  mediana <- Sir_sortat[index]    # Ob??inem mediana
  return(mediana)
}

S <- c(10, 80, 70, 50, 12, 64, 7, 5)    # Vectorul S
a <- 3    # Factorul de scalare

print("D3:")
D3(S, a)
print("---")
#-------------------------------------------------------------------------------