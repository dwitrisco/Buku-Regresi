# Data :  mtcars
# mpg : peubah respon, y 
# wt : peubah bebas atau penjelas, x.

#================
#Deteksi Leverage
#================

#buka data mtcars
data('mtcars')

#keterangan mtcars
str(mtcars)

#pisahkan variabel per kolom agar analisis bisa langsung per variabel
#jika hendak disatukan kembali gunakan dettach
attach(mtcars)

#hitung regresinya
reg<-lm(mpg~wt) 
summary(reg)

#melihat nilai hat values tiap amatan
hatvalues(reg)
par(mfrow=c(1,1))
#plot nilai peubah Y dengan hatvalues
plot(mpg,hatvalues(reg),xlim=c(10,35),ylim=c(0,0.25),xlab="mpg",ylab="Hat Value",cex=0.5)

#batas leverages (2p/n, p=2 untuk regresi sederhana)
abline(c(4/length(mpg),0),lty=2)
text(22,0.22,"High Leverage",cex=0.8, col='red')
text(22,0.015,"Low Leverage",cex=0.8, col='red')



#================
#Deteksi Pencilan
#================


#=======
#SISAAN
#=======

#meneruskan regresi pada deteksi leverages
#menghitung sisaan dengan tingkat kepercayaan 95%
resi<-residuals(reg)
res <- signif(residuals(reg), 5)

#prediksi Yi
pre <- predict(reg)

#seting tampilan 2 kolom 1 baris
par(mfrow=c(1,2))

#plot regresi
plot(wt,mpg,xlim=c(1,6.5),ylim=c(9,36),xlab="Car Weight",ylab="Miles Per Gallon",cex=0.5)
abline(reg, col='red')
#membentuk garis selisih Y dengan prediksi Y
segments(wt, mpg, wt, pre, col="red")
#menambahkan nilai residuals
library(calibrate)
textxy(wt, mpg, res, cex=0.7, pos=3)
#menandai pencilan
library(shape)
plotcircle(mid=c(2.1,33.1),r=0.6,lty=1,lwd=2,lcol="red")
plotcircle(mid=c(5.3,15.3),r=0.3,lty=1,lwd=2,lcol="red")


#plot sisaan
plot(pre,resi,xlab="predicted",ylab="residual")
abline(h=0)
textxy(pre,resi, res, cex=0.7, pos=3)
plotcircle(mid=c(27.2,6.5),r=3,lty=1,lwd=2,lcol="red")
plotcircle(mid=c(8,6),r=2,lty=1,lwd=2,lcol="red")


#=========
#STUDENT
#=========

#meneruskan regresi pada deteksi leverages
#hitung sisaan terstandarkan (STUDENT)
res.stand<-rstandard(reg)

#pengaturan layout grafik
par(mfrow=c(1,2))

#grafik qqnorm
qqnorm(res.stand,datax=TRUE,main=" ",axes=FALSE,ylim=c(-4,4),xlab=" ",ylab=" ")
title(ylab="Standard Normal Quantiles",xlab="Standardized Residual")
axis(1,-4:4)
axis(2,-3:3)
box("plot")
#tambahkan qqline
qqline(res.stand,datax=TRUE)

#plot sisaan terstandarkan
plot(mpg,res.stand,xlab="mpg",ylab="residual terstandarkan")
abline(h=0)
library(shape)
plotcircle(mid=c(33,2.3),r=3,lty=1,lwd=2,lcol="red")
plotcircle(mid=c(15.1,2.13),r=2,lty=1,lwd=2,lcol="red")


#==========
#RSTUDENT
#==========
#meneruskan regresi pada deteksi leverages
#hitung Studentized Residuals (RSTUDENT)
rstud<-rstudent(reg)

#urutkan nilai rstudent
sortud<-sort(rstud)
ord<-qt(ppoints(sortud),df=100)

#plot qqnorm rstudent
plot(sortud,ord,ylim=c(-4,4),xlim=c(-2.5,3),ylab=" ",xlab=" ")
qqline(sortud,datax=TRUE)
title(ylab="Standard Student's t Quantiles",xlab="Studentized Residual")

#plot rstudent
plot(mpg,rstud,xlab="mpg",ylab="Studentized Residuals")
abline(h=0)
abline(h=2, col='red')
library(shape)
plotcircle(mid=c(33,2.3),r=3,lty=1,lwd=2,lcol="red")
plotcircle(mid=c(15.1,2.3),r=2,lty=1,lwd=2,lcol="red")


#=========
#DFFITTS
#=========

#meneruskan regresi pada deteksi leverages
#hitung DFFITTS
dffits(reg)

par(mfrow=c(1,1))

#plot DFFITS
plot(mpg,dffits(reg),xlim=c(5,35),ylim=c(-1.2,1.2),cex=0.6,xlab='mpg',ylab='DFFITS')
abline(c(2*sqrt(2/length(mpg)),0),lty=2, col='red')
abline(-c(2*sqrt(2/length(mpg)),0),lty=2, col='red')
text(20.00,0.8,'High Influence',cex=0.9, col='red')
text(20.00,-0.8,'High Influence',cex=0.9, col='red')
library(shape)   
plotcircle(mid=c(33.3,0.65),r=2,lty=1,lwd=2,lcol="red")
plotcircle(mid=c(14.8,1.1),r=1.5,lty=1,lwd=2,lcol="red")


#================
#Cook's distance
#================
# F-transform Cook's distance
cooks<-cooks.distance(reg)
cooksf<-pf(cooks,2,30)
plot(mpg,cooksf,xlim=c(10,35),ylim=c(0.0,0.45),cex=0.6,
     xlab="mpg",ylab="F-Dist'n Percentile of Cook's Distance")
abline(h=0.125, col='red')


#============================
#deteksi dengan library OLSRR
#============================
library(olsrr)
ols_plot_dffits(reg)
ols_plot_cooksd_bar(reg)
ols_plot_resid_lev(reg)


