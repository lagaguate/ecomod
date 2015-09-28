Levene<-function(dat1,dat2,rows=T) {
#//levenes test for inequality of variances using either rows with a grouping variable or columns
#if rows then dat2=groupings

if(rows)
{
    group <- as.factor(dat2)  # precautionary
    meds <- tapply(dat1, group, median)
    resp <- abs(dat1 - meds[group])
    aa<-anova(lm(resp ~ group))[1, 4:5]
}
else
{
    med1<-median(dat1)
    med2<-median(dat2)
    resp1<-abs(dat1 - med1)
    resp2<- abs(dat2 - med2)
    sss <- rbind(data.frame(group='one', dats=resp1), data.frame(group='two',dats=resp2))
    aa<-with(sss, anova(lm(dats ~ group))[1, 4:5])
}

return(aa)
}

