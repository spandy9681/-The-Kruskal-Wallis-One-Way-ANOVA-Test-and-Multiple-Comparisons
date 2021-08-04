library(combinat)
x <- permn(1:9)

R <- function(x,i)
{
  t1 = sum(x[1:i]) ; t2 = sum(x[(i+1):(2*i)]) ;t3 = sum(x[(2*i+1):length(x)])
  return(c(t1,t2,t3))
}

R_val <- sapply(x, function(x){R(x,i = 2)})
dim(R_val)
H <- function(x,N,n)
{
  t = (12/(n*N*(N+1)))*sum(x^2) - 3*(N+1)
}
H_vec <- apply(R_val, 2, function(x){H(x,N = 6,n = 2)})

H <- function(n,k = length(n))
{
  if(length(n) == 1)
  {
    N = n*k
    x = sample(1:N)
    R = NULL
    for(i in 1:k)
    {
      R[i] = sum(x[(1+n*(i-1)):(n*i)])
    }
    h = (12/(n*N*(N+1)))*sum(R^2) - 3*(N+1)
    return(h)
  }
  else
  {
    N = sum(n)
    x = sample(1:N)
    R = NULL
    L = c(1,cumsum(n) + 1)
    U = cumsum(n)
    for(i in 1:k)
    {
      R[i] = sum(x[L[i]:U[i]])
    }
    h = (12/(N*(N+1)))*sum((R^2)/n) - 3*(N+1)
    return(h)
  }
}

H_obs = NULL
sim = 90000
n = 10 ; k = 7
for(i in 1:sim)
{
  H_obs[i] = H(n,k)
}
hist(H_obs,prob = T,breaks = 200,ylim = c(0,0.5))
curve(dchisq(x,df = k-1),add = T,n = 10000)
N = n*k ; F = (N - k)*(H_obs)/((k-1)*(N-1-H_obs))
hist(F,prob = T,breaks = 200,ylim = c(0,1))
curve(df(x,df1 = k-1,df2 = N-k),add = T,n = 10000)
J = ((k-1)*F + H_obs)/2
hist(J,prob = T,breaks = 200,ylim = c(0,1))
