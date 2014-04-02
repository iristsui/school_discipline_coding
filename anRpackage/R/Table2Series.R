Table2Series <-
function(x) {
  print("##TABLE 2 ALL SCHOOLS##")
  print(CreateTable2(x))
  print("##TABLE 2 EBS SCHOOLS##")
  print(CreateTable2(x[which(x$i.ebs==1), ]))
  print("##TABLE 2 SET SCHOOLS##")
  print(CreateTable2(x[which(x$i.set==1), ]))
  print("##TABLE 2 EBS+SET SCHOOLS##")
  print(CreateTable2(x[which(x$i.both==1), ]))
}

