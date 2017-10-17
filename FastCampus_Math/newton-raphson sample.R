
#newton function define
newton<-function(f,tol=1e-10,x0=1,N=100)
{
 h=1e-10
 i=1
 x1=x0
 p=numeric(N)
 while( i <= N )
 {
  df.dx = (f(x0+h) - f(x0))/h
  cat("df.dx:",df.dx,"\n")
  x1 = (x0-(f(x0) / df.dx))
  cat("x1:",x1,"\n")
  p[i] = x1
  i = i+1
  if(abs(x1-x0) < tol) break
  x0=x1
 }
 return( p[1:i-1] )
} 
f1<-function(x) { x^2-2 }
newton(f1)

f2<-function(x) { 10*x^3+x^2-2 }
newton(f2)

f3<-function(x) { x^3+3*x^2-6*x-8 }
newton(f3)
