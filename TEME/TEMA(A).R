A1_a=function(n,k,p,lamda)#pt valorile de la n la  k
{
  n1=n+k
  x=k:n1
  poison=dpois(x,lamda)  # Calculează distribuția Poisson
  geometrica=dgeom(x,p)     # Calculează distribuția geometrică
  binomiala=dbinom(x,n,p)  # Calculează distribuția binomială
  
  # Creează o fereastră grafică cu dimensiuni mai mari
  par(mar=c(5, 5, 4, 2) + 1)
  
  # Plotarea distribuției geometrică cu aspect mai atractiv
  plot(x, geometrica, type = 'h', lwd = 2, col = "red", xlab = "x", ylab = "Probability", main = "Distributions")
  
  # Adăugarea distribuției binomiale pe grafic
  lines(x, binomiala, type = 'h', lwd = 2, col = "green")
  
  # Adăugarea distribuției Poisson pe grafic
  lines(x, poison, type = 'h', lwd = 2, col = "yellow")
  
  # Adăugarea legendei
  legend("topright", legend = c("Geometric", "Binomial", "Poisson"), col = c("red", "green", "yellow"), lwd = 2)
}

A1_a(20,1,0.7,4)  # Apelează funcția cu parametri specifici

A1_b=function(n,p)
{
  a=1-(1-p)*(1-p)   # Calculează o valoare de probabilitate
  cat("\nP(X = impar): ")
  cat(p/a);         # Afișează valoarea de probabilitate calculată
  cat("\nP(X ⩾ 4): ")
  cat(1 - sum(dgeom(seq(1, 3), p)))   # Calculează și afișează valoarea de probabilitate
  cat("\nP(X ⩽ 20): ")
  cat(pgeom(20, p))  # Calculează și afișează valoarea de probabilitate
}

A1_b(10,0.5)    # Apelează funcția cu parametri specifici

A1_c=function(lambda)
{
  k0=0;
  cat("\n cea mai mica valoare este: ")
  while(1){
    PY=1-ppois(k0-1,lambda)   # Calculează o valoare de probabilitate
    if(PY<10^(-7)){           # Verifică dacă valoarea de probabilitate este mai mică decât un prag
      print(k0);              # Afișează valoarea lui k0
      break;
    }
    k0=k0+1;                  # Incrementarea lui k0
  }
}

A1_c(3)    # Apelează funcția cu parametri specifici

A2_a=function(filename)
{
  data=read.csv(filename,header=T,sep=',');   # Citirea datelor dintr-un fișier CSV
  probabilitati=data[['P']];                   # Extrage coloana 'P' din date
  statistica=data[['S']];                      # Extrage coloana 'S' din date
  
  cat("\nMedia pentru probabilitati: ")
  cat(mean(probabilitati));                    # Calcularea și afișarea mediei pentru 'P'
  cat("\nMediana pentru probabilitati: ")
  cat(median(probabilitati));                  # Calcularea și afișarea medianei pentru 'P'
  cat("\nDeviatia standard pentru probabilitati: \n")
  cat(sd(probabilitati));                      # Calcularea și afișarea deviației standard pentru 'P'
  #cat("\nQuantile pentru probabilitati: \n")
  cat("\n")
  print(quantile(probabilitati));               # Afișarea cuartilelor pentru 'P'
  
  cat("\nMedia pentru statistica: ")
  cat(mean(statistica));                        # Calcularea și afișarea mediei pentru 'S'
  cat("\nMediana pentru statistica: ")
  cat(median(statistica));                      # Calcularea și afișarea medianei pentru 'S'
  cat("\nDeviatia standard pentru statistica: ")
  cat(sd(statistica));                          # Calcularea și afișarea deviației standard pentru 'S'
  cat("\n")
  # cat("\nQuantile pentru statistica: \n")
  print(quantile(statistica));                   # Afișarea cuartilelor pentru 'S'
}

A2_a("note.csv");                              # Apelarea funcției cu fișierul "note.csv"

A2_b=function(filename, tip)
{
  data=read.csv(filename,header=T,sep=',');      # Citirea datelor dintr-un fișier CSV
  sample=data[[tip]];                            # Extrage coloana specificată ('P' sau 'S')
  m=mean(sample);                                # Calcularea mediei
  s=sd(sample);                                  # Calcularea deviației standard
  outliers=vector();                             # Vector pentru a stoca valorile outliere
  j=0;
  for(i in 1:length(sample)){
    if(sample[i]>=m-2*s && sample[i]<=m+2*s)     # Verificarea dacă valorile sunt în intervalul specificat
    {
      j=j+1;
      outliers[j]=sample[i]                      # Adăugarea valorilor outliere în vector
    }
  }
  sample=outliers;                               # Actualizarea mostrei cu valorile outliere
  return(sample);                                # Returnarea mostrei actualizate
}

A2_c=function(filename)
{
  tab=read.csv(filename, header = T, sep = ',')   # Citirea datelor dintr-un fișier CSV
  probabilitati=tab[['P']]                         # Extrage coloana 'P' din date
  prob=A2_b(filename,'P')                        # Apelarea funcției exA2_b pentru 'P'
  nr_p=seq(1,10,1)
  hist(probabilitati,breaks = nr_p,col="green",freq = T)   # Desenarea unui histogramă pentru 'P'
  statistica=tab[['S']]                                    # Extrage coloana 'S' din date
  statistica=A2_b(filename,'S')                          # Apelarea funcției exA2_b pentru 'S'
  nr_s=seq(1,10,1)
  hist(statistica,breaks = nr_s,col="red",freq = T)        # Desenarea unui histogramă pentru 'S'
}

A2_c("note.csv")                                        # Apelarea funcției cu fișierul "note.csv"
