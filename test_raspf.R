raspf<-function(r,e)
{
  layout(matrix(c(2,1,1,2,1,1,0,3,3),3,3, byrow = T))
  plot (r,e)
  boxplot(r)
  boxplot(e,horizontal =T)
}