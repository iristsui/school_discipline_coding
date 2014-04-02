# PBIS Multivariate Analyses
# By: Casey Tsui
# Format: R
# Last updated: 4/20/11

library(gee)
library(corrplot)

CreatePValues <- function(x) {
  # Args:
  #   x: Regression Model Object
  p <- 2 * pnorm(abs(coef(summary(x))[,5]), lower.tail = FALSE)
  return(p)
}

CreateGradRate95CI <- function(x) {
  # Args:
  #   x: GEE Regression model
  model <- x
  estimate <- coef(summary(model))[, 1]
  robust.se <- coef(summary(model))[, 4]
  area.under.normal.curve <- qnorm(0.975)
  min95 <- estimate - (area.under.normal.curve * robust.se)
  max95 <- estimate + (area.under.normal.curve * robust.se)
  p <- CreatePValues(model)
  return(cbind(estimate, p, min95, max95))
}

CreateGradRateGEEGraph <- function() {
  pbis.measure <- factor(c("EBS continuous", "SET continuous", "EBS criterion", 
                    "SET criterion"))
  outcome.var <- c(7)
  dat <- rbind(CreateGradRate95CI(model.1.7)[3, ],
               CreateGradRate95CI(model.2.7)[3, ],
               CreateGradRate95CI(model.3.7)[3, ],
               CreateGradRate95CI(model.4.7)[3, ])
  dat <- as.data.frame(dat)
  return(cbind(dat, pbis.measure, outcome.var))
}

grad.rate.gees <- CreateGradRateGEEGraph()






disc.var.names <- c("iss", "oss", "major", "odr", "recid", "num.stud",
                    "grad.rate", "attend.rate", "perc.unexcused",
                    "reading.score", "math.score", "afram.major.disp.index",
                    "asian.major.disp.index", "latino.major.disp.index",
                    "afram.oss.disp.index", "asian.oss.disp.index",
                    "latino.oss.disp.index")
model.1.covars <- c("high.school", "male.perc", "afram.perc", "asian.perc",
                    "latino.perc", "frpm.perc")
model.2.covars <- c("nces_member", "stud.per.fte", "urbanicity")
all.model.vars <- c(model.1.covars, model.2.covars, disc.var.names, 
                    pbis.fidelity.measures)
interaction.1.var <- "high.school:nces_member"
interaction.2.var <- "frpm.prop.cntr:urbanicity"
interaction.3.var <- "fte.100.student.cntr:urbanicity"




#correlation matrix
corrs <- subset(lm.data, select=c(all.model.vars))
corrs$urbanicity <- as.numeric(corrs$urbanicity)
corrs$high.school <- NULL
x <- cor(corrs, use="pairwise.complete.obs")
corrplot(x, method="circle", order="PCA", title="Correlation Matrix")
corrplot(x, method="circle", order="hclust", title="Correlation Matrix",
         addrect=6)
corrplot(x, method="color", order="hclust", title="Correlation Matrix",
         addrect=6)
#dendrogram
plot(as.dendrogram(hclust(dist(x), "ward")),
     horiz = TRUE,
     center = TRUE,
     nodePar = list(lab.cex=0.7, pch=NA))




#scatterplot matrix of outcome variables
x.names <- names(corrs)
x <- lm.data[, x.names]
plotmatrix(x)


#Pvalue for VGAM
pt(coef(x)[, 3], df=227)


RunModel <- function(formula, z) {
  #  Args:
  gee(formula,
      id      = school_educationalorgid,
      data    = lm.data,
      family  = poisson,
      corstr  = z,
      maxiter = 100)
}



Model1 <- function(y, x, z="exchangeable") {
  #  Args:
  covars <- paste(model.1.covars, collapse=" + ")
  depvar <- paste(y, "~ year")
  text <- paste(depvar, x, covars, sep=" + ")
  formula <- as.formula(text)
  model <- RunModel(formula, z)
  print(summary(model))
  print(CreatePValues(model))
}


Model2 <- function(y, x, z="exchangeable") {
  #  Args: y
  covars1 <- paste(model.1.covars, collapse=" + ")
  covars2 <- paste(model.2.covars, collapse=" + ")
  covars  <- paste(covars1, covars2, sep=" + ")
  depvar <- paste(y, "~year")
  text <- paste(depvar, x, covars, sep=" + ")
  formula <- as.formula(text)
  model <- RunModel(formula, z)
  print(summary(model))
  print(CreatePValues(model))
}





Model2Int1 <- function(y, x, z="exchangeable") {
  # Args:
  #   x: Data frame outcome variable
  return(gee(y ~ year +          
                 x +
                 high.school +
                 male.prop +
                 afram.prop +
                 aian.prop +
                 asian.prop +
                 latino.prop +
                 frpm.prop +
                 nces_member.cntr +
                 fte.100.student +
                 urbanicity +
                 high.school:nces_member +
                 offset(exposure),
         id = school_educationalorgid,
         data = lm.data,
         family = poisson,
         corstr = z,
         maxiter = 100))
}

Model2Int2 <- function(y, x, z="exchangeable") {
  # Args:
  #   x: Data frame outcome variable
  return(gee(y ~ year +          
                 x +
                 high.school +
                 male.prop +
                 afram.prop +
                 asian.prop +
                 latino.prop +
                 frpm.prop.cntr +
                 nces_member +
                 fte.100.student +
                 urbanicity +
                 frpm.prop.cntr:urbanicity +
                 offset(exposure),
         id = school_educationalorgid,
         data = lm.data,
         family = poisson,
         corstr = z,
         maxiter = 100))
}

Model2Int3 <- function(y, x, z="exchangeable") {
  # Args:
  #   x: Data frame outcome variable
  return(gee(y ~ year +          
                 x +
                 high.school +
                 male.prop +
                 afram.prop +
                 asian.prop +
                 latino.prop +
                 frpm.prop +
                 nces_member +
                 fte.100.student.cntr +
                 urbanicity +
                 fte.100.student.cntr:urbanicity +
                 offset(exposure),
         id = school_educationalorgid,
         data = lm.data,
         family = poisson,
         corstr = z,
         maxiter = 100))
}

Model2Full <- function(y, x, z="exchangeable") {
  # Args:
  #   x: Data frame outcome variable
  return(gee(y ~ year +          
                 x +
                 high.school +
                 male.prop +
                 afram.prop +
                 asian.prop +
                 latino.prop +
                 frpm.prop.cntr +
                 nces_member.cntr +
                 fte.100.student.cntr +
                 urbanicity +
                 high.school:nces_member.cntr +
                 frpm.prop.cntr:urbanicity +
                 fte.100.student.cntr:urbanicity +
                 offset(exposure),
         id = school_educationalorgid,
         data = lm.data,
         family = poisson,
         corstr = z,
         maxiter = 100))
}

################################################################################

# List school IDs that are outliers for outcome variables
subset(lm.data, iss > 1000, select=school_ncesschoolid)
subset(lm.data, oss > 1000, select=school_ncesschoolid)
subset(lm.data, expul > 0.05, select=school_ncesschoolid)
subset(lm.data, major > 4, select=school_ncesschoolid)
subset(lm.data, odr > 4, select=school_ncesschoolid)
subset(lm.data, recid > 4, select=school_ncesschoolid)
subset(lm.data, num.stud > 4, select=school_ncesschoolid)
subset(lm.data, grad.rate < 90, select=school_ncesschoolid)
subset(lm.data, attend.rate < 80, select=school_ncesschoolid)



# Naive linear regression of outcome variables and fidelity measure
with(lm.data, lm(iss.100.day ~ fidelity))
with(lm.data, lm(oss.100.day ~ fidelity))
with(lm.data, lm(expul.100.day ~ fidelity))
with(lm.data, lm(major.100.day ~ fidelity))
with(lm.data, lm(odr.100.day ~ fidelity))



response.names <- c("iss.100.day", "oss.100.day", "major.100.day",
                    "odr.100.day", "grad.rate", "attend.rate")
pbis.fidelity.measures <- c("ebs.implementationaverage",
                            "set.implementationaverage",
                            "ebs.criterion8080",
                            "set.criterion8080")



# List all standard deviations
cbind(sd(subset(lm.data, select=response.names), na.rm=TRUE))



# Model 1
for (i in seq(along=pbis.fidelity.measures)) {
  for (j in seq(along=response.names)) {
		print("# NEW MODEL #######################################################") 
    print(paste("Response variable =", response.names[j], sep=" "))
    print(paste("PBIS fidelity measure =", pbis.fidelity.measures[i], sep=" "))
    pbis <- pbis.fidelity.measures[i]
    response <- response.names[j]
    Model1(response, pbis)
  }
}
########## EDIT BELOW







# Model 2
for (i in seq(along=pbis.fidelity.measures)) {
  for (j in seq(along=response.names)) {
    pbis <- lm.data[, pbis.fidelity.measures[i]]
    response <- lm.data[, response.names[j]]
    model <- with(lm.data, Model2(response, pbis))
    p.model <- CreatePValues(model)
    print(paste("Response variable =", response.names[j], sep=" "))
    print(paste("PBIS fidelity measure =", pbis.fidelity.measures[i], sep=" "))
    print(summary(model))
    print(p.model)
		print("# END OF MODEL ####################################################") 
  }
}


# Model 2, high school / enrollment interaction
for (i in seq(along=pbis.fidelity.measures)) {
  for (j in seq(along=response.names)) {
    pbis <- lm.data[, pbis.fidelity.measures[i]]
    response <- lm.data[, response.names[j]]
    model <- with(lm.data, Model2Int1(response, pbis))
    p.model <- CreatePValues(model)
    print(paste("Response variable =", response.names[j], sep=" "))
    print(paste("PBIS fidelity measure =", pbis.fidelity.measures[i], sep=" "))
    print(summary(model))
    print(p.model)
		print("# END OF MODEL ####################################################") 
  }
}

# Model 2, urbanicity / frpm interaction
for (i in seq(along=pbis.fidelity.measures)) {
  for (j in seq(along=response.names)) {
    pbis <- lm.data[, pbis.fidelity.measures[i]]
    response <- lm.data[, response.names[j]]
    model <- with(lm.data, Model2Int2(response, pbis))
    p.model <- CreatePValues(model)
    print(paste("Response variable =", response.names[j], sep=" "))
    print(paste("PBIS fidelity measure =", pbis.fidelity.measures[i], sep=" "))
    print(summary(model))
    print(p.model)
		print("# END OF MODEL ####################################################") 
  }
}


# Model 2, urbanicity / fte interaction
for (i in seq(along=pbis.fidelity.measures)) {
  for (j in seq(along=response.names)) {
    pbis <- lm.data[, pbis.fidelity.measures[i]]
    response <- lm.data[, response.names[j]]
    model <- with(lm.data, Model2Int3(response, pbis))
    p.model <- CreatePValues(model)
    print(paste("Response variable =", response.names[j], sep=" "))
    print(paste("PBIS fidelity measure =", pbis.fidelity.measures[i], sep=" "))
    print(summary(model))
    print(p.model)
		print("# END OF MODEL ####################################################") 
  }
}

# Model 2, urbanicity / fte interaction
for (i in seq(along=pbis.fidelity.measures)) {
  for (j in seq(along=response.names)) {
    pbis <- lm.data[, pbis.fidelity.measures[i]]
    response <- lm.data[, response.names[j]]
    model <- with(lm.data, Model2Int3(response, pbis))
    p.model <- CreatePValues(model)
    print(paste("Response variable =", response.names[j], sep=" "))
    print(paste("PBIS fidelity measure =", pbis.fidelity.measures[i], sep=" "))
    print(summary(model))
    print(p.model)
		print("# END OF MODEL ####################################################") 
  }
}



# Model 2, all interactions
for (i in seq(along=pbis.fidelity.measures)) {
  for (j in seq(along=response.names)) {
    pbis <- lm.data[, pbis.fidelity.measures[i]]
    response <- lm.data[, response.names[j]]
    model <- with(lm.data, Model2Full(response, pbis))
    p.model <- CreatePValues(model)
    print(paste("Response variable =", response.names[j], sep=" "))
    print(paste("PBIS fidelity measure =", pbis.fidelity.measures[i], sep=" "))
    print(summary(model))
    print(p.model)
		print("# END OF MODEL ####################################################") 
  }
}









# gamlss models
gamlss(iss ~ year + ebs.implementationaverage, data=lm.data, family=NBI)
