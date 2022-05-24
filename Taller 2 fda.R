##TALLER 2 ESTADISTICA DESCRIPTIVA
##-----------------------------------------------------------------------

##-----------------------------------------------------------------------


library(R.matlab)
library(fda.usc)
library(fdaoutlier)
library(fda)
library(funData)
library(MFPCA)
numMuestras <- 268
numEmision <- 571

##setwd("D:/Especialización en Estadística/Primer Semestre 2022/Análisis de Datos Funcionales/Talleres/Taller 1.2 Estadística Descriptiva/")

datos <- readMat("C:/Users/KMGARCAS/Downloads/sugar_Process//data.mat")

intensidad <- data.frame(datos$X)
emision <- datos$EmAx
longExc <- datos$ExAx

listaMatricesDatos <- list()
listaMatricesFunciones <- list()



sec_der = int2Lfd(2)
base = create.bspline.basis(c(1,numEmision),nbasis=60)
dat_fdpar= fdPar(base,sec_der,lambda=6)
class(datfd1$fd)
datfd1 = smooth.basis (argvals=1:numEmision,y=t(listaMatricesDatos[[1]]),fdParobj=dat_fdpar)
datfd2 = smooth.basis (argvals=1:numEmision,y=t(listaMatricesDatos[[2]]),fdParobj=dat_fdpar)
datfd3 = smooth.basis (argvals=1:numEmision,y=t(listaMatricesDatos[[3]]),fdParobj=dat_fdpar)
datfd4 = smooth.basis (argvals=1:numEmision,y=t(listaMatricesDatos[[4]]),fdParobj=dat_fdpar)
datfd5 = smooth.basis (argvals=1:numEmision,y=t(listaMatricesDatos[[5]]),fdParobj=dat_fdpar)
datfd6 = smooth.basis (argvals=1:numEmision,y=t(listaMatricesDatos[[6]]),fdParobj=dat_fdpar)
datfd7 = smooth.basis (argvals=1:numEmision,y=t(listaMatricesDatos[[7]]),fdParobj=dat_fdpar)

plot(datfd$fd[1],type="1")

funSmoothData = list()
funSmoothData [[1]]= fd2funData(fdobj=datfd1$fd,argvals=1:numEmision)
funSmoothData [[2]]= fd2funData(fdobj=datfd2$fd,argvals=1:numEmision)
funSmoothData [[3]]= fd2funData(fdobj=datfd3$fd,argvals=1:numEmision)
funSmoothData [[4]]= fd2funData(fdobj=datfd4$fd,argvals=1:numEmision)
funSmoothData [[5]]= fd2funData(fdobj=datfd5$fd,argvals=1:numEmision)
funSmoothData [[6]]= fd2funData(fdobj=datfd6$fd,argvals=1:numEmision)
funSmoothData [[7]]= fd2funData(fdobj=datfd7$fd,argvals=1:numEmision)


multiFunData(funSmoothData) -> mfd.fundat

nuevafuncion = MFPCA(mfd.fundat,2,fit = T,uniExpansions = list(list(type= "uFPCA"),
                                                               list(type= "uFPCA"),
                                                               list(type= "uFPCA"),
                                                               list(type= "uFPCA"),
                                                               list(type= "uFPCA"),
                                                               list(type= "uFPCA"),
                                                               list(type= "uFPCA")))


##Punto Habb 1
str(nuevafuncion$functions)
x11()
plot(nuevafuncion$functions)

##punto 2

summary(nuevafuncion)


##punto 3
screeplot(nuevafuncion)

##PUNTO 4

par(mfrow=c(1,2))
scoreplot(nuevafuncion)
matplot(nuevafuncion$scores[,1:2],type="1",lty=1)
plot(nuevafuncion$fit)