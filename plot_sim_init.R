library("ggplot2")                     # Load ggplot2 package

dati1<-read.delim("sdtab1",sep="",skip=1)
dati1<-dati1[which(dati1$CMT==5),]

ggplot(data=dati1)+geom_line(data=dati1, aes(x=DAY, y=Ctot))
ggplot(data=dati1)+geom_line(data=dati1, aes(x=DAY, y=Cfree),col='red')


par(mar=c(4, 4, 4, 4) + 0.1)
plot(dati1$DAY, dati1$E, type = "l", axes = FALSE, xlab = "", ylab = "")
axis(2)
mtext("Conc. fosfato",side=2,line=2.5)
box()
## Draw the time axis
axis(1)
mtext("TIME (day)",side=1,line=2.5)
par(new = TRUE)
plot(dati1$DAY, dati1$Cfree,type = "l", lty=2,axes=FALSE, xlab="", ylab="",col="blue")
axis(4,col="blue",col.axis="blue")
mtext("Conc. libera erdafitinib",side=4,line=2.5, col="blue")
lines(dati1$DAY, dati1$compeffPD,type = "l", lty=1, xlab="", ylab="",col="blue") 
legend("bottomright",legend=c("Conc. fosfato","Plasma", "Comp. effetto"),
       text.col=c("black","blue","blue"),lty=c(1,2,1),col=c("black","blue","blue"))


plot(dati1$DAY, dati1$Cfree,type = "l", xlab="TIME(day)", ylab="Conc. libera di erdafitinib [ng/mL]")
lines(dati1$DAY, dati1$compeffPD, type = "l",xlab = "", ylab = "", col="red")
legend("bottomright",legend=c("Plasma", "Comp. effetto"),
       text.col=c("black","red","blue"),lty=c(1,1),col=c("black","red"))

