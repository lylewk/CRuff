#' @export
run_it=function(parms,dat,i.which,observer,area){
labs=c('Fem','Tib','Hum','Rad')
labs2=c('Femur','Tibia','Humerus','Radius')
bone=which(i.which==labs)

est.stat=function(y){
  stat=parms[1]*y+parms[2]
  z=qnorm((1-area)/2)
  lo=z*parms[3]
  hi=-lo
  return(round(c(lo+stat,stat,stat+hi)))
}

dat=dat[complete.cases(dat),]
N=NROW(dat)
out=matrix(NA,nc=3,nr=N)
for(i in 1:N){
  out[i,]=est.stat(dat[i,2])
}
out=data.frame(dat[,1],out)
colnames(out)=c('actual','lo.25','est','hi.25')
#windows()
plot(out$actual,out$est,xlab='Actual',ylab='Estimated',
     main=paste(observer,labs2[bone]))
abline(0,1)

lo.quarter=round(0.25*N)
hi.quarter=round(0.75*N)
mid.half=c(lo.quarter+1,hi.quarter-1)
lo.quarter=c(1,lo.quarter)
hi.quarter=c(hi.quarter,N)

grouped=vector()
grouped[lo.quarter[1]:lo.quarter[2]]=1
grouped[mid.half[1]:mid.half[2]]=2
grouped[hi.quarter[1]:hi.quarter[2]]=3

below=rep(FALSE,N)
with.in=rep(FALSE,N)
above=rep(FALSE,N)
for(i in 1:N){
  if(out$actual[i]<out$lo.25[i]) below[i]=TRUE
  if(out$actual[i]>=out$lo.25[i] & out$actual[i]<=out$hi.25[i]) with.in[i]=TRUE
  if(out$actual[i]>out$hi.25[i]) above[i]=TRUE
}

bias=out$est-out$actual
cat('mean bias =  ',mean(bias),'\n\n')

in.groups=as.numeric(table(grouped))
bias=mean(out$est[grouped==1]-out$actual[grouped==1])
cat('mean bias bottom quartile =  ',bias,'\n')
cat('Number actual below 50% PI, Number actual in 50% PI, Number actual above 50% PI\n')
cat('Bottom quartile ',sum(below[grouped==1]),sum(with.in[grouped==1]),
    sum(above[grouped==1]),'\n')
cat('\n')
bias=mean(out$est[grouped==2]-out$actual[grouped==2])
cat('mean bias middle half =  ',bias,'\n')
cat('Middle half ',sum(below[grouped==2]),sum(with.in[grouped==2]),
    sum(above[grouped==2]),'\n')
cat('\n')
bias=mean(out$est[grouped==3]-out$actual[grouped==3])
cat('mean bias top quartile =  ',bias,'\n')
cat('Top quartile ',sum(below[grouped==3]),sum(with.in[grouped==3]),
    sum(above[grouped==3]))
cat('\n\n','All data\n')
cat(sum(below),sum(with.in),sum(above))
cat('\n')
}
