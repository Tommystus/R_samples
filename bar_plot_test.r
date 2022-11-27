# This is a simple example to look at survival rate of Titanic passenger by ticket class
#
# Load data - note that read.csv can only read http NOT https
# so change any url with https to http and hope that the web site support it
tn = read.csv('http://vincentarelbundock.github.io/Rdatasets/csv/datasets/Titanic.csv')
# list table column names
names(tn)
# list first few rows of data
cat("first few rows:","\n")
head(tn)
# This table has data already summarized by Class, Sex, Age, Survived
# Let's summarize it by passenger class and whether they survived
fs = aggregate(tn$Freq, by=list(tn$Class, tn$Survived), FUN=sum)
cat("Display variable freq summarized by passenger class and live/die:","\n")
# the variable name by itself is a command to show what's in it
fs
# rename the summarized table columns so that they make more sense
names(fs) = c("Class","Survived","Sum")
fs
# Transpose the data into a matrix to make it easier to bargraph
# This is done in 2 steps
# 1. order the data by the variable that will become the column and row names of the matrix
# 2. use the reshape function to do the transpose
# 3. set empty space in matrix with 0 so we don't get error when using them (this happen when row*col != total data row)
rowOrder = order(fs$Class,fs$Survived,fs$Sum)
fs = fs[rowOrder,]
tfs = reshape( fs, v.name='Sum', idvar='Survived', timevar='Class', direction='wide')
tfs[is.na(tfs)] = 0
# set the matrix row name to make it looks nice :)
row.names(tfs) = tfs$Survived
tfs
# create the matrix of the number part (don't need first column of yes/no)
# the notation [,-1] means all row, all column minus 1st one
m = as.matrix(tfs[,-1])
# set the column names of the matrix to the passenger class
colnames(m) = levels(fs$Class)
m
# Setup color to plot (see http://www.r-bloggers.com/color-palettes-in-r/ for example color palettes)
colorSet = rainbow(length(row.names(m)))
# Finally, we get to plot the data!
barplot(m, col=colorSet)
# And set the legend text
legend('topleft', legend=row.names(m), col=colorSet, title='Survived', fill=colorSet)
