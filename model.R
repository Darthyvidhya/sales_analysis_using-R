#Importing data
library(readr)
SD <- read_csv("E:/SD.csv")
View(SD)

#Creating a Dataframe
flami<-data.frame(SD$Invoice.Number,
 SD$Total.Quantity,
 SD$Total.Value,
 SD$CGST,
 SD$SGST,
 SD$Taxable.Value,
 SD$hsn.number,
 SD$hsn.value)
flami

#Accessing
taxable<-SD$Taxable.Value
total<-SD$Total.Value
cgst<-SD$CGST
sgst<-SD$SGST
quantity<-SD$Total.Quantity
invoiceno.<-SD$Invoice.Number
hsnno.<-SD$hsn.number
hsnamount<-SD$hsn.value

#1 Correlation
#Corplot
library(corrplot)
m<-cor(flami)
corrplot(m,method="number",tl.cex=0.5)
#Graphical plot
plot(total,taxable,col=7,main="realtion between total & taxable")
plot(total,cgst,col=5,main="realtion between total & CGST ")
plot(total,sgst,col=9,main="relation between total & SGST")
plot(cgst,sgst,col=2,main = "relation between CGST & SGST"


#2 Reggression
#Multiple regression
model12<-lm(total~taxable+cgst+sgst,flami=SD[,c("total","taxable","cgst","sgst")])
model12

summary(model12)

#Prediction
newflami<-data.frame(taxable=560,cgst=28,sgst=28)
newtotal<-predict(model12,newflami)
newtotal
#Graphical Representation
plot(total,taxable+cgst+sgst,col="red",main="Totalvalue and other factors",
    abline(lm(taxable+cgst+sgst~total)),
    cex=1.3,pch=1,type="b",xlab="total",ylab="other factors")
plot

#3Piechart
    #Pie chart between quantity of sales and statetax(SGST):
quantity1ofsale<-SD$SGST
place<-SD$Place.Of.Supply
pie(quantityofsale,place)
pie
piepercent<-paste(round(100*quantity1ofsale/sum(quantity1ofsale),1),"%")
pie(quantity1ofsale,labels=piepercent)

    #Pie chart between quantity of sales and central tax(CGST):
quantity2ofsale<-SD$CGST
place<-SD$Place.Of.Supply
pie(quantity2ofsale,place)
piepercent<-paste(round(100*quantity2ofsale/sum(quantity2ofsale),1),"%")
pie(quantity2ofsale,labels=piepercent)

#4.Bar chart
x<-SD$UQC
y<-SD$Taxable.Value
barplot(y,names.arg=x)

d<-SD$UQC
g<-SD$Total.Value
barplot(g,names.arg = d)

t<-SD$UQC
h<-SD$SGST
barplot(h,names.arg=t,main="PLOT BETWEEN QUANTITY AND SGST")

j<-SD$UQC
k<-SD$SGST
barplot(k,names.arg=j,main="PLOT BETWEEN QUANTITY AND CGST")

#5.Plotting
a<-c(SD$Taxable.Value)
b<-c(SD$Total.Value)
c<-c(SD$CGST)
d<-C(SD$SGST)
dataa<-data.frame(a,b,c,d)
dataa

plot(dataa$a,type="o",col="red",main="sales data",xlab="value before tax" ,
 ylab = "value after tax",xlim=c(0,80),ylim = c(0,100))
lines(dataa$b,col="blue")
lines(dataa$c,col="brown")
lines(dataa$d,col="green")
legend("topright",c("taxable","cgst","sgst"),cex = 0.9,fill = c("blue","brown","green"))


#6.Anova
#One-way Anova
production=lm(SD$Total.Quantity~SD$Total.Value)
anova(production)

#Two-way Anova
fit=aov(SD$Total.Quantity~cgst+sgst)
anova(fit)

#7.Statistical analysis between CGST & SGST
#CGST
mean(cgst)
median(cgst)
var(cgst)
range(cgst)
max(cgst)
min(cgst)
sd(cgst)
IQR(cgst)

#SGST
mean(cgst)
median(cgst)
var(cgst)
range(cgst)
max(cgst)
min(cgst)
sd(cgst)
IQR(cgst)

#Frequency of the data
table(SD$Total.Value)
table(SD$Taxable.Value)

#To find the range of the sales
range(SD$Total.Quantity)
range(SD$Total.Value)
range(SD$Taxable.Value)
range(SD$CGST)
range(SD$SGST)
range(SD$hsn.value)












