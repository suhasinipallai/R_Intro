iris_data <- iris
str(iris_data)
#3.a  data-type is dataframe 
#3.b  columns(variables)= 5 , rows(observations) = 150
sepal_area <- iris_data$Sepal.Width * iris_data$Sepal.Length
class(sepal_area)
#calculate the avg sepal area
avg_sepal_area <- mean(sepal_area)
avg_sepal_area
#round the avg_sepal_area to 4 digits 17.8229
round(avg_sepal_area,digits=4)
#for loop
for (a in sepal_area) {
  print(paste("Sepal area is :" ,a))
}
#create a new column 
iris_data$Avg.Sepal.Area <- iris_data$Sepal.Width * iris_data$Sepal.Length

iris_data$Species
species_counts <- table(iris_data$Species)
species_counts
#all species have equal observations = 50(setosa,versicolor,virginica)
par(mfrow=c(1,2))
#plotting - use help ?plot()
x <- iris_data$Petal.Length
y <- iris_data$Petal.Width
#q 8.a plot petal width vs petal length
plot(x ,y,main="Petal width Vs Petal length",xlab ="petal length",ylab="petal width")
#plot(x = iris_data$Petal.Length , y= iris_data$Petal.Width,xlab ="petal length",ylab="petal width")

#q 8.b plot petal length vs sepal length
plot(x=iris_data$Petal.Length , y=iris_data$Sepal.Length , main = "Petal Length vs Sepal Length",xlab="Petal Length",ylab="Sepal Length")


#view dataset mtcars
View(mtcars)
#to look at the mtcars doc
?mtcars
#10.a => [, 5]	 drat	-  Rear axle ratio
#10.b min & max 1/4 mile time
minqsec <- min(mtcars$qsec)
minqsec
# min qsec is 14.5
maxqsec <- max(mtcars$qsec)
maxqsec
#max qsec is 22.9
View(mtcars)
#Ford Pantera L has slowest qsec with 14.50
#Merc 230 has the fastest time  with 22.90

#11 create a table of count of cylinders
counts <- table(mtcars$cyl)
counts
#bar plot 
barplot(counts,main="Count of Cylinders",xlab="cylinders",col=c("lightblue","pink","yellow"))
# engine with 8 cyl is most prevalent

#12 printing isa gas guzzler if cyl =8

#for (m in mtcars){
#  print(m)
#}

for(i in 1:nrow(mtcars)) {
  if(mtcars[i,2] == 8) {
    print(paste(rownames(mtcars)[i], "is a gas guzzler!"))
  }
}

#13
starwars
?starwars
head(starwars)

#how many diff home world do characters come from 
distinct(starwars,homeworld)

#how many characters call each of these home
homeworlds <- 
  starwars %>%
  filter(!is.na(homeworld)) %>%
  group_by(homeworld) %>%
  summarise(Count = n())
homeworlds

# homeworld for diff characters

homeworlds %>%
  filter(Count >1) %>%
  ggplot(aes(reorder(homeworld,Count),Count)) +
  geom_col(fill="purple") +
  scale_y_continuous(breaks = seq(0,12,2)) +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  xlab("Home World") +
  ggtitle("Homeworlds of Star Wars characters")

# check the largest,smallest character
starwars
starwars %>%
  ggplot(aes(mass,height,label=name)) +
  geom_text(size=3 ,
            check_overlap = TRUE,aes(color=gender))

#to see metrics in inches and pounds
starwars %>%
  mutate(mass2 = mass * 2.2) %>%
  mutate(height2 = height * 0.393071) %>%
  ggplot(aes(mass2,height2,label=name)) +
  geom_text(size=3 ,
            check_overlap = TRUE,aes(color=gender)) +
  xlab("Weight(pounds)") +
  ylab("height(pounds)")


# plotting using starwars

ggplot(starwars,aes(x=mass,y=height,color="red")) +
  geom_boxplot() 
  
?starwars



#14
library(swirl)



#----tidyverse practice
?tibble
 1:10 %>%
  sum() %>%
  sqrt()
library(tidyverse)
a<-1:5
t<-tibble(a,b=a*2)  
View(t)
t %>% View()

#count across columns - pipelines
table(mtcars$cyl)

mtcars %>%
  count(cyl)

# gather(),seperate(),unite(),spread() - rely on key value pairs

?gather
?data.frame

messy1 = data.frame(
  name=c("Wilbur","Petunia","Gregory"),
  a = c(67,80,64),
  b = c(56,90,50),
  c = c(99,30,40)
)
messy1
plot(messy1)

# that was a messy data

messy1 = gather(messy1,drug,heartrate,a,b,c)
messy1
plot(messy1$name,messy1$heartrate)

#separate function

?separate

messy2=data.frame(
  name=c("Wilbur_m","Petunia_f","Gregory_m"),
  a = c(67,80,64),
  b = c(56,90,50),
  c = c(99,30,40)
)
messy2

messy2 = separate(messy2,name,c("name","gender"),sep="_")
messy2
messy2 = gather(messy2,drug,heartrate,a,b,c)
messy2

# unite function - unite multiple columns into one

?unite

messy3 = data.frame(
  name=c("Wilbur_m","Petunia_f","Gregory_m"),
  last_name= c("Smith","Gordon","Jones"),
  a = c(67,80,64),
  b = c(56,90,50),
  c = c(99,30,40)
)
messy3

#first gather,then separate then unite 

messy3 = gather(messy3, drug , heartrate ,a,b,c)
messy3

messy3 = separate(messy3,name,c("first_name","gender"),sep="_")
messy3

messy3 = unite(messy3,name, 1,3, sep = " ")
messy3

#apply funtions
?cbind
?apply
?vector
x<- c(a=1,b=2)
is.vector(x)
as.vector(x)
