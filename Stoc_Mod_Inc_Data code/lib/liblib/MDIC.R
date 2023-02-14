MDIC<-function(model)
{
#READ ALL VARIABLES----
y= serie$y
x= serie$x
min_temp=serie$min_temp
difs1=serie$difs1
difs2=serie$difs2
ma_1=serie$ma_1
max_temp=serie$max_temp
mean_temp=serie$mean_temp
median_temp=serie$median_temp
min_winddirection=serie$min_winddirection
max_winddirection=serie$max_winddirection
mean_winddirection=serie$mean_winddirection
min_winddirection=serie$min_winddirection
median_winddirection=serie$median_winddirection
min_windforce=serie$min_windforce
max_windforce=serie$max_windforce
mean_windforce=serie$mean_windforce
median_windforce=serie$median_windforce
x2= serie$x**2
x3= serie$x**3
x4= serie$x**4
c1= cos(2*pi*serie$x/nb_temps)
s1= sin(2*pi*serie$x/nb_temps)
c2= cos(4*pi*serie$x/nb_temps)
s2= sin(4*pi*serie$x/nb_temps)
c3= cos(8*pi*serie$x/nb_temps)
s3= sin(8*pi*serie$x/nb_temps)

#pr<-read_excel("pr.xlsx")
pr<-predict(model)#pr$pr

#pr<- fitted(model)

#SET INDEX a----
a<- 0.25

n<- length(pr)
ord<- length(model$coefficients)

#MEAN (mean_y) & VARIANCE (sigma_2_y)----
mean_y<- pr

anv<-anova(model)
mse_res<-anv$`Mean Sq`
mse<-mse_res[length(mse_res)]
sd_y<- sqrt(mse)

#DEFINE VECTOR (p) OF NORMAL PROBABILITIES----
p<-c()
p_a<-c()

#BUILD NORMAL PROBABILITIES (p)----
for (i in 1:n){
p[i]<- (1/sqrt(2*pi*mse))*exp(-((y[i]-pr[i])^2)/2*sd_y)
p_a[i]<-p[i]^a
}

#MQ CALCULATION----
sum_f<- sum(p_a)
MQ<- -((1+(1/a))*(1/n)*sum_f)

#MDIC CALCULATION----
MDIC<- (n*MQ)+((2*pi)^(-a/2))*((1+a)^(2+(ord/2)))*ord
MDIC
}
