## -----------------------------------------------------------------------------
fun<-function(x){
  sin(x)}
LeftRect<-function(down, up, n){
  h = (up-down)/n
  s = fun(down)*h
  for (i in 1:n)
  {
    s = s + fun(down+i*h)*h
  } 
  
  s
}

## -----------------------------------------------------------------------------
LeftRect(1,2,100)

