#######################
######################
######################
data_rjc<-list(N=68,Nf=30,Cobs=Cobs*landSp[1,],Index=I[1,])
data_rjc<-list("N"=55,"Nf"=30,"Cobs"=Cobs[13:68]*landSp[1,],"Index"=I[1,1:30])
parameters<-c("Bmed", "Imed","C", "Y","I","K","q","sigma2","tau2","r")

start <- Sys.time() # d?but du temps

result<-bugs(data=data_rjc,inits=NULL,
             model.file="model_bayesien_RJC.txt",
             n.chains=3,n.burnin=5,n.thin=100,debug=TRUE,
             parameters.to.save=parameters, # les inits sont g?n?r?s automatiquement
             n.iter=500,codaPkg=TRUE,DIC=TRUE)


# coda=T afin que la sortie de BRugsFit soitun objet du type mcmc

# Fin du temps et indique la dur?e de l'inf?rence
print("Time:") 
total.run.time <- Sys.time() - start
print(total.run.time)
#beep() # fonction alarme pour avertir de l'obtention des r?sultats du mod?le

mcmc_result<-result # permet de garder une copie des r?sultats sous forme mcmc
res<-as.matrix(mcmc_result) # transforme les r?sultats en matrice
res<-t(res) # puis la transpose
write.table(res,"res.csv") # enregistre la matrice des r?sultats sous forme csv dans le r?pertoire de travail (choisi au d?but du script)
save(mcmc_result, file="result.Rdata") # enregistre dans le r?pertoire de travail une sauvegarde .R des objets voulus (permet de conserver une copie des donn?es au format mcmc par exemple)

#save plots
pdf(paste("mcmcplots_G",Nbq,".pdf",sep=""))
plot(result)
#gelman.plot(result)
dev.off()
#summary(result)

#v?rifier convergence
gelman.diag(result) 
heidel.diag(result)