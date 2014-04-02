Table1Series <-
function(x) {
  print("##TABLE 1 ALL SCHOOLS##")
  print(CreateTable1(x))
  print("##TABLE 1 EBS SCHOOLS##")
  print(CreateTable1(x[which(x$i.ebs==1), ]))
  print("##TABLE 1 SET SCHOOLS##")
  print(CreateTable1(x[which(x$i.set==1), ]))
  print("##TABLE 1 EBS+SET SCHOOLS##")
  print(CreateTable1(x[which(x$i.both==1), ]))
}

