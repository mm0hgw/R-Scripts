mydata <- read.delim("ExampleData.txt", stringsAsFactors=FALSE)
if(!dir.exists('data'))
dir.create('data')
save(file='data/mydata.rda',mydata)
