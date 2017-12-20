mydata <- read.delim("ExampleData.txt", stringsAsFactors=FALSE)
if(!dir.exists('data'))
dir.create('data')
write.csv(file='data/mydata.csv',mydata)
