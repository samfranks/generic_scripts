############################################################
#
#		AIC table, Coefficient, and SE Matrix code
#		Generates weighted parameter estimates and unconditional standard errors (with customization)
#		For general linear models, but can be customized to accommodate generalized linear models or mixed effects models
#
#		by Samantha Franks
#		2011/11/22
#
############################################################

# REQUIRES:
# a dataset called "dat" with the variables of interest
# a Model list with models written as formulas
	# e.g. modellist[[1]] <- formula(Response ~ x), modellist[[2]] <- formula(Response ~ x + y)
# a model vector called "globalmodel" which is the global model in your analysis (ie. contains the most complex model in your candidate set) - this is a short way to generate all possible coefficient combinations, including interactions, for the Coefficient matrix that will be used to calculate weighted parameter estimates; may not be appropriate for all model sets depending on the models included
# a character string "wd" that is the name of the desired  directory for output
# a character string "out" that is a description of the output

# dat <- read.csv("DatasetName.csv")
# modellist <- list()  # set up blank list to hold model formulas
# modellist[[1]] <- formula(Response ~ x)
# modellist[[2]] <- formula(Response ~ y)
# modellist[[3]] <- formula(Response ~ x + y)
# etc
# globalmodel <- lm(Response ~ x * y)

ModelList<-modellist
coefnames <- names(globalmodel$coef)
modeloutput<-list()	# blank model output list

# AIC function
# a function to produce relevant calculations for an AIC table that is used in conjunction with a list containing the model names
calculate.AIC<-function(aictable,modellist) {
modelnames<-modellist
delta.aic<-aictable$AIC.c-min(aictable$AIC.c)
lik.aic<-exp(-delta.aic/2)
aic.w<-lik.aic/(sum(lik.aic))
aic.table<-data.frame(modelnames,AIC.table,delta.aic,lik.aic,aic.w)
}

# blank Coefficient Matrix
CoefMatrix <- matrix(NA, nrow=length(ModelList), ncol=length(coefnames), dimnames = list(c(1:length(ModelList)), coefnames))

# blank SE Matrix
SEMatrix <- matrix(NA, nrow=length(ModelList), ncol=length(coefnames), dimnames = list(c(1:length(ModelList)), coefnames))

# blank AIC matrix
AIC.table<-matrix(NA, nrow=length(ModelList), ncol=6, dimnames = list(c(1:length(ModelList)), c("r.squared","n.obs","df","-2loglik","AIC","AIC.c")))

# loop for calculating model output, filling CoefMatrix and SEMatrix

for(i in 1:length(ModelList)){

modeloutput[[i]]<-lm(ModelList[[i]], data=dat)
m<-modeloutput[[i]]

# fill in row "i" of the AIC table
aic<-AIC(m)
df<-length(m$coef) + 1 # +1 for the variance
n.obs<-nrow(dat)
AIC.table[i,"r.squared"]<-summary(m)$adj.r.squared
AIC.table[i,"n.obs"]<-n.obs
AIC.table[i,"df"]<-df
AIC.table[i,"-2loglik"]<- -2*logLik(m)
AIC.table[i,"AIC"]<-aic
aic.c<-aic + (2*df*(df+1))/(n.obs-df-1)
AIC.table[i,"AIC.c"]<-aic.c

# fill in row "i", column "j" of the Coefficient Matrix
for (j in 1:length(coefnames)) {
	
if (coefnames[j] == "(Intercept)" & length(names(m$coef)) == 1) {CoefMatrix[i,j] <- summary(m)$coef[coefnames[j],1]} else {
CoefMatrix[i,j] <- summary(m)$coef[,1][coefnames[j]]
# CoefMatrix[i,j] <- summary(m)$coef[coefnames[j],1] # gives subscript out of bounds error if coefnames[j] is not one of the parameters in the model
# CoefMatrix[i,j] <- summary(m)$coef[,1][coefnames[j]] gives NA if model is the NULL model (only 1 parameter, the intercept) because in that case, summary(m)$coef is a vector (1-dimension only) rather than a dataframe (2-dimensions)
# code a special case for the NULL (intercept only) model, all other models with other parameters get the "else" statement
} # close if else statement

# fill in row "i" of the Standard Error Matrix
if (coefnames[j] == "(Intercept)" & length(names(m$coef)) == 1) {SEMatrix[i,j] <- summary(m)$coef[coefnames[j],2]} else {
SEMatrix[i,j] <- summary(m)$coef[,2][coefnames[j]]
} # close if else statement

} # close inner "j" loop

} # close outer "i" loop

# calculate deltaAICs, likelihoods, and AIC weights
AIC.output<-data.frame(AIC.table)
aic<-calculate.AIC(AIC.output,as.character(ModelList))
#for (i in 4:9) {
# aic[,i]<-round(aic[,i], digits=3)
# }
aic.ordered<-aic[rev(order(aic$aic.w)),]

# see AIC table
aic.ordered[,c(2,5,6,7,8,9,10)] <- round(aic.ordered[,c(2,5,6,7,8,9,10)], digits = 3)
print(aic.ordered)

# write AIC table output to .csv files
setwd(wd) # working directory for output
write.csv(aic.ordered,paste("AIC output_",out,".csv",sep=""),row.names=FALSE)

# write model output to .csv files

sink(paste("model summaries_",out,".doc",sep=""))
for (i in 1:length(modellist)) {
print(paste("MODEL ", i, sep = ""))
print(ModelList[[i]])
print(summary(modeloutput[[i]]))
print(paste("###################################################################"))
}
sink()

#--------------------------------------------------
# calculate parameter likelihoods, weighted estimates, and unconditional SEs and CIs
#--------------------------------------------------

CoefMatrix[is.na(CoefMatrix)] <- 0
SEMatrix[is.na(SEMatrix)] <- 0

### parameter likelihoods

parlik<-rep(0,length(coefnames))

# if parameter j is included in the model i (ie. there is a non-zero value in the CoefMatrix for model i), add model i's AIC weight from the dataframe "aic" to the value of parlik[j], otherwise, add "0"

for (j in 1:length(coefnames)) {

for (i in 1:length(modeloutput)) {

if (CoefMatrix[i,j] != 0) {parlik[j] <- parlik[j] + aic[i,"aic.w"]} else {parlik[j] <- parlik[j]}

}
}

### weighted parameter estimates

weightedpar<-rep(NA,length(coefnames))

for (j in 1:length(coefnames)) {

weightedpar[j] <- sum(CoefMatrix[,j] * aic[,"aic.w"])

}

### unconditional SEs and CIs
# AICw * sqrt((SE^2) + (coef - weightedpar)^2 ))

uncondSE<-rep(NA,length=ncol(SEMatrix))


for (j in 1:length(coefnames)) {

uncondSE[j] <- sum(aic[,"aic.w"] * sqrt((SEMatrix[,j]^2) + (CoefMatrix[,j] - weightedpar[j])^2))
}

CI<-1.96 * uncondSE

lowerCI<-weightedpar - CI
upperCI<-weightedpar + CI

### parameter table

par.table <- data.frame(coefnames, parlik, weightedpar, uncondSE, lowerCI, upperCI)
par.table.new <- data.frame(coefnames,round(par.table[,2:6], digits=3)) # values rounded to 3 decimal places
print(par.table.new)

write.csv(par.table.new,paste("parameter estimates_",out,".csv",sep=""),row.names=FALSE)

setwd("/Users/samantha/Documents/Sam's Stuff")	# return working directory to your usual default