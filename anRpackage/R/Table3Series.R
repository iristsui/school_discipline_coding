Table3Series <-
function(x) {
  print("##TABLE 3 ALL SCHOOLS##")
  print(CreateTable3(x))
  print("##TABLE 3 EBS SCHOOLS##")
  print(CreateTable3(x[which(x$i.ebs==1), ]))
  print("##TABLE 3 SET SCHOOLS##")
  print(CreateTable3(x[which(x$i.set==1), ]))
  print("##TABLE 3 EBS+SET SCHOOLS##")
  print(CreateTable3(x[which(x$i.both==1), ]))
}

