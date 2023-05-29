ex1_3=function(file){
  table=read.csv(file,header=T,sep=',');
  frate=table[['female']];
  mrate=table[['male']];
  
  interval=seq(min(frate),max(frate),(max(frate)-min(frate))/7)
  hist(frate,breaks=interval,freq=T,col='red');
  
  interval=seq(min(mrate),max(mrate),(max(mrate)-min(mrate))/7)
  hist(mrate,breaks=interval,freq=T,col='blue');
  
}
ex1_3("life_expectancy.csv")


#-------------------------------------------------------------------------------------
ex2_1=function(filename)
{
  saple=scan(filename);
  print(mean(sample));
  print(median(sample));
}

#ex2_1("sample1.txt")

ex2_2=function(filename)
{
  data=read.csv(filename,header=T,sep=',');
  female=data[['female']];
  male=data[['male']];
  print(mean(female));
  print(median(female));
  print(mean(male));
  print(median(male));
}

#ex2_2("life_expectancy.csv")

ex3=function(filename)
{
  data=read.csv(filename,header=T,sep=',');
  female = data[['female']];
  male = data[['male']];
  print(mean(female));
  print(sd(female));
  print(mean(male));
  print(sd(male));
}

#ex3("life_expectancy.csv")

ex3_prim=function(filename)
{
  data=read.csv(filename,header=T,sep=',');
  female = data[['female']];
  male = data[['male']];
  print(median(female));
  print(summary(female[2]));
  print(summary(female[5]));
  print(median(male));
  print(summary(male[2]));
  print(summary(male[5]));
}

#ex3_prim("life_expectancy.csv")

#-------------------------------------------------------------------------------
outliers_mean = function(sample)
{
  m = mean(sample)
  s = sd(sample)
  outliers = vector()
  j = 0
  for(i in 1:length(sample))
    if(sample[i] < m - 2*s || sample[i] > m + 2*s) {
      j = j + 1
      outliers[j] = sample[i]
    }
  print(outliers)
}

outliers_iqr = function(sample)
{
  q1 = summary(sample)[2]
  q3 = summary(sample)[5]
  outliers = vector()
  iqr = q3 - q1
  j = 0;
  for(i in 1:length(sample))
    if(sample[i] < q1-1.5*iqr || sample[i] > q3+1.5*iqr)
    {
      j = j + 1
      outliers[j]=sample[i]
    }
  print(outliers)
}

ex3_3=function(filename){
  sample=scan(filename)
  
  print(summary(sample))
  
  print(summary(sample)[0])
  print(summary(sample)[1])
  print(summary(sample)[2])
  print(summary(sample)[3])
  print(summary(sample)[4])
  print(summary(sample)[5])
  
  outliers_mean(sample)
  outliers_iqr(sample)
  
}

ex3_3("sample2.txt")

#-------------------------------------------------------------------------------


ex9=function(n,p)
{
  prob=dgeom(0:n, p)
  barplot(prob, names.arg=0:n,main='Densitatea', xlab="Valori", ylab="Probabilitati");
  print(prob);
}
ex9(10, 0.3)

ex10=function(n,p)
{
  prob=dpois(0:n, p)
  barplot(prob, names.arg=0:n,main='Densitatea', xlab="Valori", ylab="Probabilitati");
  print(prob);
}
ex10(10, 0.5)


