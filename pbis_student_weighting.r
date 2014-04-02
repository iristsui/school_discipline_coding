

students.unexcused <- ddply(days.unexcused.per.student, c("school_educationalorgid", "school_schoolyear"), function(df) c(students_unexcused = length(df$student_studentid)))
> students.unexcused

 days.unexcused.per.student$no.odr.students <- days.unexcused.per.student$nces_member - days.unexcused.per.student$students_unexcused

> head(days.unexcused.per.student)
  school_educationalorgid school_schoolyear student_studentid days.unexcused.per.student
1                    1094            200405            277511                          2
2                    1094            200405            277528                          1
3                    1094            200405            277533                          2
4                    1094            200405            277534                          2
5                    1094            200405            277536                          1
6                    1094            200405            277540                          3
  students_unexcused school.statelabel nces_member school.instruc.days no.odr.students
1                228                MD        1772                 180            1544
2                228                MD        1772                 180            1544
3                228                MD        1772                 180            1544
4                228                MD        1772                 180            1544
5                228                MD        1772                 180            1544
6                228                MD        1772                 180            1544
> days.unexcused.per.student.x <- days.unexcused.per.student[, c(1:4, 8)]
> no.odr.students <- days.unexcused.per.student[, c(1:3, 9)]
> names(days.unexcused.per.student.x)
[1] "school_educationalorgid"    "school_schoolyear"          "student_studentid"         
[4] "days.unexcused.per.student" "school.instruc.days"       
> days.unexcused.per.student.x$weight <- 1
> head(days.unexcused.per.student.x)
  school_educationalorgid school_schoolyear student_studentid days.unexcused.per.student
1                    1094            200405            277511                          2
2                    1094            200405            277528                          1
3                    1094            200405            277533                          2
4                    1094            200405            277534                          2
5                    1094            200405            277536                          1
6                    1094            200405            277540                          3
  school.instruc.days weight
1                 180      1
2                 180      1
3                 180      1
4                 180      1
5                 180      1
6                 180      1
> no.odr.students$student_studentid <- -99
> no.odr.students$days.unexcused.per.student <- 0
> no.odr.students <- days.unexcused.per.student[, c(1:3, 8:9)]
> days.unexcused.per.student.x$weight <- 1
> no.odr.students$student_studentid <- -99
> no.odr.students$days.unexcused.per.student <- 0
> length(days.unexcused.per.student.x)
[1] 6
> length(no.odr.students)
[1] 6
> jen.format <- rbind(days.unexcused.per.student.x, no.odr.students)
Error in match.names(clabs, names(xi)) : 
  names do not match previous names
> names(days.unexcused.per.student.x)
[1] "school_educationalorgid"    "school_schoolyear"          "student_studentid"         
[4] "days.unexcused.per.student" "school.instruc.days"        "weight"                    
> names(no.odr.students)
[1] "school_educationalorgid"    "school_schoolyear"          "student_studentid"         
[4] "school.instruc.days"        "no.odr.students"            "days.unexcused.per.student"
> no.odr.students$weight<- no.odr.students$no.odr.students
> no.odr.students$no.odr.students <- NULL
> jen.format <- rbind(days.unexcused.per.student.x, no.odr.students)
> head(jen.format)
  school_educationalorgid school_schoolyear student_studentid days.unexcused.per.student
1                    1094            200405            277511                          2
2                    1094            200405            277528                          1
3                    1094            200405            277533                          2
4                    1094            200405            277534                          2
5                    1094            200405            277536                          1
6                    1094            200405            277540                          3
  school.instruc.days weight
1                 180      1
2                 180      1
3                 180      1
4                 180      1
5                 180      1
6                 180      1
> jen.format$perc <- jen.format$days.unexcused.per.student / jen.format$school.instruc.days * 100




> jen.format$school.instruc.days <- as.numeric(as.character(jen.format$school.instruc.days))
> jen.format$perc <- jen.format$days.unexcused.per.student / jen.format$school.instruc.days * 100
> table(jen.format$perc)

                0 0.555555555555556 0.568181818181818 0.666666666666667  1.11111111111111 
            15428              5833              3615               412              1563 
 1.13636363636364  1.33333333333333  1.66666666666667  1.70454545454545                 2 
             1186                78               566               524                20 
 2.22222222222222  2.27272727272727  2.66666666666667  2.77777777777778  2.84090909090909 
              260               334                13               157               225 
 3.33333333333333  3.40909090909091  3.88888888888889  3.97727272727273                 4 
               66               142                38                95                 3 
 4.44444444444444  4.54545454545455  4.66666666666667                 5  5.11363636363636 
               29                57                 1                22                39 
 5.55555555555556  5.68181818181818  6.11111111111111              6.25  6.66666666666667 
                9                31                 5                25                 5 
 6.81818181818182  7.22222222222222  7.38636363636364  7.95454545454545  8.33333333333333 
               22                 2                15                 6                 1 
 8.52272727272727  8.66666666666667  9.44444444444444   9.6590909090909  10.2272727272727 
                9                 1                 2                 5                 4 
 10.7954545454545  11.3636363636364  13.0681818181818  15.3409090909091  17.0454545454545 
                4                 1                 1                 1                 1 
> table(jen.format$days.unexcused.per.student)

    0     1     2     3     4     5     6     7     8     9    10    11    12    13    14 
15428  9860  2827  1110   607   385   208   134    86    61    41    30    26    18     6 
   15    17    18    19    20    23    27    30 
   10     7     4     4     1     1     1     1 




> 30/176
[1] 0.1704545
> head(jen.format)
  school_educationalorgid school_schoolyear student_studentid days.unexcused.per.student
1                    1094            200405            277511                          2
2                    1094            200405            277528                          1
3                    1094            200405            277533                          2
4                    1094            200405            277534                          2
5                    1094            200405            277536                          1
6                    1094            200405            277540                          3
  school.instruc.days weight      perc
1                 180      1 1.1111111
2                 180      1 0.5555556
3                 180      1 1.1111111
4                 180      1 1.1111111
5                 180      1 0.5555556
6                 180      1 1.6666667
> weighted.averages <- ddply(jen.format, c("school_educationalorgid, "school_schoolyear"), function(df) c(weighted.average = weighted.mean(perc, weight)))
Error: unexpected symbol in "weighted.averages <- ddply(jen.format, c("school_educationalorgid, "school_schoolyear"
> weighted.averages <- ddply(jen.format, c("school_educationalorgid", "school_schoolyear"), function(df) c(weighted.average = weighted.mean(perc, weight)))
Error in weighted.mean(perc, weight) : object 'perc' not found
> weighted.averages <- ddply(jen.format, c("school_educationalorgid", "school_schoolyear"), function(df) c(weighted.average = weighted.mean(df$perc, df$weight)))
> head(weighted.averages)
  school_educationalorgid school_schoolyear weighted.average
1                     217            200506      0.001566416
2                     217            200607      0.001531561
3                     218            200506      0.001469238
4                     218            200607      0.001887562
5                     218            200708      0.002166298
6                     220            200607      0.001521028
> fix(jen.format)
> hist(weighted.averages$weighted.averages)
Error in hist.default(weighted.averages$weighted.averages) : 
  'x' must be numeric
> hist(weighted.averages$weighted.average)
> ddply(weighted.averages, c("school_schoolyear"), function(df) c(mean = mean(df$weighted.average, na.rm=TRUE)))
  school_schoolyear        mean
1            200405 0.001629208
2            200506 0.001645117
3            200607 0.001747691
4            200708 0.001460411

