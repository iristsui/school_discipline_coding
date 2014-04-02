# PBIS Grade Range Plot
# By Casey Tsui
# Format: R
# Last updated: 10/26/10


# Author's Comments:



a$grade_lo <- a$nces_gslo
a$grade_lo <- as.character(a$grade_lo)
a$grade_lo[a$grade_lo == "KG"] <- "00"
a$grade_lo[a$grade_lo == "PK"] <- "-1"
a$grade_lo <- as.numeric(a$grade_lo)

a$grade_hi <- a$nces_gshi
a$grade_hi <- as.numeric(as.character(a$grade_hi))





a.old <- unique(a[, c(2, 72:73)])
a.old <- a.old[order(a.old$grade_lo), ]
a.old$index <- seq(1:nrow(a.old))


plot(1, type="n", axes=TRUE, xlab="Grades offered", ylab="School Index",
     xlim=c(-1,12), ylim=c(0,137), main="Grade ranges of PBIS schools")
abline(v=6)



for(i in 1:nrow(a.old)) {
  lines(c(a.old$grade_lo[i], a.old$grade_hi[i]),
        c(a.old$index[i], a.old$index[i]),
        type='l')
}
