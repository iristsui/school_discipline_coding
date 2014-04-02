model.1.7 <- gee(grad.rate ~ year +
                             ebs.implementationaverage +
                             male.prop +
                             afram.prop +
                             asian.prop +
                             latino.prop +
                             frpm.prop +
                             nces_member +
                             fte.100.student +
                             urbanicity,
                             data=lm.data,
                             id=school_educationalorgid,
                             family=binomial,
                             corstr="AR-M",
                             Mv=1)
CreatePValues(model.1.7)



model.2.7 <- gee(grad.rate ~ year +
                             ebs.criterion8080 +
                             male.prop +
                             afram.prop +
                             asian.prop +
                             latino.prop +
                             frpm.prop +
                             nces_member +
                             fte.100.student +
                             urbanicity,
                             data=lm.data,
                             id=school_educationalorgid,
                             family=binomial,
                             corstr="AR-M",
                             Mv=1)
CreatePValues(model.2.7)


model.3.7 <- gee(grad.rate ~ year +
                             set.implementationaverage +
                             male.prop +
                             afram.prop +
                             asian.prop +
                             latino.prop +
                             frpm.prop +
                             nces_member +
                             fte.100.student +
                             urbanicity,
                             data=lm.data,
                             id=school_educationalorgid,
                             family=binomial,
                             corstr="AR-M",
                             Mv=1)
CreatePValues(model.3.7)


model.4.7 <- gee(grad.rate ~ year +
                             set.criterion8080 +
                             male.prop +
                             afram.prop +
                             asian.prop +
                             latino.prop +
                             frpm.prop +
                             nces_member +
                             fte.100.student +
                             urbanicity,
                             data=lm.data,
                             id=school_educationalorgid,
                             family=binomial,
                             corstr="AR-M",
                             Mv=1)
CreatePValues(model.4.7)
