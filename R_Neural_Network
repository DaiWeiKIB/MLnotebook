library(limma)  
library(neuralnet)
library(NeuralNetTools)

expFile="LASSO.geneExp.txt"     
diffFile="diff.txt"        


rt=read.table(expFile, header=T, sep="\t", check.names=F)
rt=as.matrix(rt)
rownames(rt)=rt[,1]
exp=rt[,2:ncol(rt)]  #提取剩下的列(从第二列开始)并将它们赋值给exp 
dimnames=list(rownames(exp),colnames(exp))
data=matrix(as.numeric(as.matrix(exp)),nrow=nrow(exp),dimnames=dimnames)
data=avereps(data)


diffRT=read.table(diffFile, header=T, sep="\t", check.names=F, row.names=1)
diffRT=diffRT[row.names(data),]  #只提取与实验数据行名匹配的行


dataUp=data[diffRT[,"logFC"]>0,]
dataDown=data[diffRT[,"logFC"]<0,]
dataUp2=t(apply(dataUp,1,function(x)ifelse(x>median(x),1,0)))  #选择log-fold正变化的基因
dataDown2=t(apply(dataDown,1,function(x)ifelse(x>median(x),0,1)))  #选择负对数倍变化的基因


outTab=rbind(dataUp2, dataDown2)
outTab=rbind(id=colnames(outTab), outTab)
write.table(outTab, file="LASSO.txt", sep="\t", quote=F, col.names=F)

inputFile="LASSO.txt"      

data=read.table(inputFile, header=T, sep="\t", check.names=F, row.names=1)
data=as.data.frame(t(data))


group=gsub("(.*)\\-(.*)\\-(.*)\\-(.*)\\-(.*)", "\\5", row.names(data))
data$con=ifelse(group=="con", 1, 0)
data$treat=ifelse(group=="treat", 1, 0)


fit=neuralnet(con+treat~., data, hidden=5)
fit$result.matrix
fit$weight
#plot(fit)

pdf(file="LASSO.pdf", width=10, height=10)
plotnet(fit)
dev.off()


net.predict=compute(fit, data)$net.result
net.prediction=c("con", "treat")[apply(net.predict, 1, which.max)]
predict.table=table(group, net.prediction)
predict.table
conAccuracy=predict.table[1,1]/(predict.table[1,1]+predict.table[1,2])
treatAccuracy=predict.table[2,2]/(predict.table[2,1]+predict.table[2,2])
paste0("Con accuracy: ", sprintf("%.3f", conAccuracy))
paste0("Treat accuracy: ", sprintf("%.3f", treatAccuracy))


colnames(net.predict)=c("con", "treat")
outTab=rbind(id=colnames(net.predict), net.predict)
write.table(outTab, file="neural.predict.txt", sep="\t", quote=F, col.names=F)
