# Sign Test

n<-8
med<-6
st<-3
limit<-3
data<-rnorm(n,mean =med,sd=st)
median<-median(data)
below<-sum(data > med)
above<-sum(data < med)
sus<-min(above,below)
pvalue<-2*min(pbinom(sus,n,0.5),0.5)
pvalueround<-round(pvalue,digits=3)
stripchart(data,main=paste("Sign Test for median = ",med,". P-value = ",pvalueround), method="jitter",pch=21,col=c("black"),bg=c("red"),xaxt="n",xlim=c(med-limit*st,med+limit*st))
axis(1, at = seq(round(med-limit*st,digits=0),round(med+limit*st,digits=0), by = 1), las=2)
abline(v=med)


# Raw data

n<-10
mn<-0
mx<-12
md<-0.5*(mx-mn)
data<-sample(mn:mx,n)
stripchart(data,main=paste("Raw Data"), method="stack",pch=21,col=c("black","black"),bg=c("red","red"),xaxt="n",xlim=c(mn-1,mx+1))
axis(1, at = seq(mn,mx, by = 1), las=2)
abline(v=md)

# Discard zeros

n<-10
mn<-0
mx<-12
md<-0.5*(mx-mn)
data<-sample(mn:mx,n)
condition=(data==md)
discard<-data[condition==TRUE]
use<-data[condition==FALSE]
list<-list("use"=use,"discard"=discard)
stripchart(list,main=paste("Discard zeros"), method="stack",pch=21,col=c("black","black"),bg=c("red","red"),xaxt="n",xlim=c(mn-1,mx+1))
axis(1, at = seq(mn,mx, by = 1), las=2)
abline(v=md)

# Include ranks

n<-5
mn<-0
mx<-12
md<-0.5*(mx-mn)
data<-sample(mn:mx,n)
condition=(data==md)
discard<-data[condition==TRUE]
use<-data[condition==FALSE]
list<-list("use"=use,"discard"=discard)
stripchart(list,main=paste("Signed Ranks"), method="stack",pch=21,col=c("black","black"),bg=c("red","red"),xaxt="n",xlim=c(mn-1,mx+1))
axis(1, at = seq(mn,mx, by = 1), las=2)
abline(v=md)
ranks=rank(abs(use-md))
ranksign<-ifelse(use<md,-1,1)
signedranks<-ranks
text(use,1,signedranks,adj=c(0.5,-3),cex=0.7,col="black")

# Include signed ranks

n<-10
mn<-0
mx<-12
md<-0.5*(mx-mn)
data<-sample(mn:mx,n)
condition=(data==md)
discard<-data[condition==TRUE]
use<-data[condition==FALSE]
list<-list("use"=use,"discard"=discard)
stripchart(list,main=paste("Signed Ranks"), method="stack",pch=21,col=c("black","black"),bg=c("red","red"),xaxt="n",xlim=c(mn-1,mx+1))
axis(1, at = seq(mn,mx, by = 1), las=2)
abline(v=md)
ranks=rank(abs(use-md))
ranksign<-ifelse(use<md,-1,1)
signedranks<-ranksign*ranks
text(use,1,signedranks,adj=c(0.5,-3),cex=0.7,col="black")

# Signed ranks with W test statistic

n<-8
mn<-2
mx<-20
md<-0.5*(mx-mn)
data<-sample(mn:mx,n)
condition=(data==md)
discard<-data[condition==TRUE]
use<-data[condition==FALSE]
ranks=rank(abs(use-md))
ranksign<-ifelse(use<md,-1,1)
signedranks<-ranksign*ranks
W<-min(sum(signedranks[signedranks>0]),(-1)*sum(signedranks[signedranks<0]))
list<-list("use"=use,"discard"=discard)
stripchart(list,main=paste("Signed Ranks W =",W), method="stack",pch=21,col=c("black","black"),bg=c("red","red"),xaxt="n",xlim=c(mn-1,mx+1))
axis(1, at = seq(mn,mx, by = 1), las=2)
abline(v=md)
text(use,1,signedranks,adj=c(0.5,-3),cex=0.7,col="black")
print(data)

# create wilcoxon signed rank function

wilcoxon_rank <- function(data, eta = 0)
{
  condition = (data == eta)
  data_used <- data[condition == FALSE]
  ranks <- rank(abs(data_used - eta))
  rank_signs <- ifelse(data_used < eta, -1, 1)
  signed_ranks <- ranks * rank_signs
  W <- min(sum(signed_ranks[signed_ranks > 0]), (-1)*sum(signed_ranks[signed_ranks < 0]))
  print(W)
}

# use function

data <- c(6,8,8,2,10,11,15)
wilcoxon_rank(data, eta = 10)

# create wilcoxon signed rank function with simulation

wilcoxon_rank_simulation <- function(data, eta = 0, n = 12)
{
  data <- rnorm(n, mean = eta, sd = 4)
  condition = (data == eta)
  data_used <- data[condition == FALSE]
  ranks <- rank(abs(data_used - eta))
  rank_signs <- ifelse(data_used < eta, -1, 1)
  signed_ranks <- ranks * rank_signs
  W <- min(sum(signed_ranks[signed_ranks > 0]), (-1)*sum(signed_ranks[signed_ranks < 0]))
  print(W)
}

# use function

wilcoxon_rank_simulation(data)

# simulate repeatedly and count proportions of each signed rank sum

W_data <- replicate(10000,wilcoxon_rank_simulation(data, n = 10))
barplot(table(W_data))

# simulate repeatedly and explore critical values

sample_size = 10
check <- 8
W_data <- replicate(10000,wilcoxon_rank_simulation(data, n = sample_size))
table(W_data)
props <- proportions(table(W_data))
reds <- check + 1
greys <- sample_size * (sample_size + 1) * 0.5
vec_reds <- rep("red",times = reds)
vec_greys <- rep("blue", times = greys)
vec_col <- c(vec_reds, vec_greys)
upto <- check + 1
p <- sum(props[c(1:upto)])
barplot(props,
        col = vec_col,
        main = paste("W of",check,"or less appeared for",p,"of samples"))



