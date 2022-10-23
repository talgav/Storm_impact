#Hello, this is a script which will help you implement the material from the last lesson
#'QUANTIFYING DIVERSITY I' on your data.
#In this script you will find all the function you are expected to use with explanations 
#and using example on Bees data set
#First go over the script and see what each function does. Then try to use your own data.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#First we will install and upload two libraries: "vegan" and "rareNMtests".
# You can do it by first installing the package and then running the following line:

library("vegan", lib.loc="~/R/win-library/3.1")
library("rareNMtests", lib.loc="~/R/win-library/3.1")

#or you can just mark the box next to the package name in the lower right window 
#under packages

#For using the functions we will use a data set of bees from the Arava:
#import the data set from its location on your computer (you can do it either 
#by direct code line or via the 'import Dataset' button in RStodio):

bees_data <- read.csv("C:/Users/nimrodelk/OneDrive/community ecology/R practice/bees_data.csv")
View(bees_data)

#you can see that each row is a sample with id in the column called 'site'. 
#For each site we have information on distans from agriculre ('distance_agri')
#and the name of the location ('site_loc')

#In this data set you can see that were there is no data the value is 'NA'. 
#For our later functions we will need to change NA's to zeros(0), we'll do it like this:

bees_data[is.na(bees_data)]=0

#look at the data again to make sure we succeeded:

View(bees_data)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                         individual rarefaction curves
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#For the first functions we will use we need only the species observation data without the name and information on each site
#So we will create another subset with only this data:

bees_sp=bees_data[,4:ncol(bees_data)]

#look at the subseting formula- we ask R to take all the rows and the columns from 
#number 4 (the first species) and to the end of the dataframe

#Next we will use the function 'rarefy' from the vegan package.
#look at the help for this function:

?rarefy

#Some of the important arguments are:
#x-is the dataframe we will use
#sample= how many individuals to use in the rarefraction

#We want to define the sample size for the rarefaction. 
#We will take the minimal number of records in a single site this is because want 
#to avoid extrapolating to sample size larger then were observed

raremax <- min(rowSums(bees_sp))

#'min' is the function for finding the minimal value. inside the 'min' function we 
#use 'rowSums' function which calculates the sum of each row- in this case the number of 
#individuals in a sitelet's see what is this minimal number of specimens in a site

raremax

#We can see that the site with the least number of individuals sampled has 77 specimens

#Now we proceed with the rarefaction. We will see what is the sp richness in 
#each site when we sample randomly 77 individuals:

rare_index_raremax=rarefy(x=bees_sp,sample=raremax)

#now type the name of the element 'rare_index_raremax' to see what are the rarefaction
# results

rare_index_raremax

#what you will see is the species richness in each site if 77 specimens are taken
#And plot:

rare_plot_raremax=rarecurve(bees_sp,step=20,sample =raremax)

#function 'rarecurve' plots the rarefaction.here you see the richness in each site
#to make some sense into this messy plot lets give each site color.

site_color<-rep(c("black","blue","red","pink","green"),each = 4) # rep will replicate each of the colors 4 time

rare_plot_raremax=rarecurve(bees_sp,step=20,sample =raremax,col = site_color) # I added the col argument that give each line color accurding to th 'site_color'


#this is still too messy...
#even if we color each site in different color and add legend we
#wont be able to understand this results...
#let's group all the samples by a common feature (in this case site)

bees_site<-bees_data[,c(1,4:ncol(bees_data))] #create data that includes the species and the site

bees_per_site<-list() #create an empty list. A list is a group of elemnts which can be from differnt types.
#In our case each element of the list will be an output of the colSums function for specific site

#next is the for loop function:

for (site in unique(bees_site$site_loc)){ #this is the first line of a for loop. we set an 
  #index name- in this case 'site' that will take a different value in each iteration
  #the values will be taken from the vector 'unique(bees_site$site_loc)' which is the names of the site found in the 'site_loc' column of the data
  #so now that we have location set to "Amatzya" and then to "Gidron" and so on... the next line are subseting the data according to the corrent value of 'site'
  
  one_site<-subset(bees_site,site_loc==site) 
  
  #this line apply the function colSums to each subsetted site data.
  
  
  site_colsum<-colSums(one_site[,2:ncol(one_site)])
  #now we will store the output in the list ('rarefy_result') oder the name of the current 'site'. 
  #In lists we use a double brackets[[]] to specify an element
  
  bees_per_site[[site]]<-site_colsum
  
}

#look at the list now:
bees_per_site
#You can see it has 5 elements each is the output of the function 'specaccum' on a diffrent location
#to get elemnts from a list we use [[]] or the name of the element. If I want to see the data for "Amatzya" which is the first elemnt I can use:
bees_per_site[[1]]
#or
bees_per_site$Amatzya

#convert the list into data frame structure. 
bees_per_site_data<-as.data.frame(bees_per_site)

#flip the rows and columns location with the 't' function.

bees_per_site_data<-t(bees_per_site_data)

#lets find the new minmal value (just like we did before.

raremax_group<-min(rowSums(bees_per_site_data))

#let rarefy the grouped per site data.

rarefy(bees_per_site_data,raremax_group)

#the next two lines are vectors that contain the sites name and a list of colors.
#we will use them inside the 'plot' function in order to make the plot color coded and add a legend.

site_name<-rownames(bees_per_site_data)
colors<-c("black","blue","red","pink","green")

# let's plot the rarefy curve per site.

rareplot_per_site<-rarecurve(bees_per_site_data,step=20,sample =raremax_group,col=colors,label=NULL) 
legend("bottomright",legend = site_name, lty=c(1,1),col = colors)


# if you want to see the "steps" in creating rarefaction curve,
#You can use more than one sample size- here we will use sample sizes 
#between 10 to 100 in 10 individuals interavls 

rare_index_steps=rarefy(bees_sp,sample=seq(10,raremax,10))
rare_index_steps

#If you set the sample size to be higher then raremax you can still run the function 
#but you should note that for the sites with less than 100 individuals observed you are 
#looking for richness outside of the range of individuals which were actually recorded

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                accumulation curves
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Now we will move to speacies accumulation curve in which we'll look at more then one sample and we group them by a common feature.
#For this example we can use the information about the location of the sample from the bees data. We have 5 location: 'Amatzya','Gidron','Shahak','Sheizaf' and 'Znav_Hatzeva'
#We want to create separte curve for each of the locations.  We can subset the data according to location
#This, for example, is for location 'Amatzya':

bees_amatzya=subset(bees_data,site_loc=="Amatzya") #we use the full data set with the additional site information. subset is by column name 'site_loc' and for rows with value "Amatzya" 

#now we will make data frame with only the species data of Amatzya:
sp_amatzya=bees_amatzya[,4:ncol(bees_amatzya)]

#now for the accumulation, we will use function 'specaccum'.  We can use it in two ways which depands on how we define the methos arrgument-
#method="random"- will accumulate by transect. Which mean that we will see get the mean richness for ascending number of transects  

acc_amatzya_random=specaccum(sp_amatzya,method="random")

#watch the results:
acc_amatzya_random
#method="rarefaction"- will accumulate by individuals. Which mean that we will see get the mean richness for ascending number of transects but will calculate the mean number of individuals in each step

acc_amatzya_rarefaction=specaccum(sp_amatzya,method="rarefaction")
acc_amatzya_rarefaction

#if we want to do the same for all the sites we can do it one by one. We can also build a loop for making the calculation for all the sites.
acc_result=list()#first create an empty list. A list is a group of elemnts which can be from differnt types. 
#In our case each element of the list will be an output of the rarefaction function for specific location
#next is the for loop:
for (location in unique(bees_data$site_loc)){#this is the first line of a for loop. we set an index name- in this case 'location' that will take a different value ineach iteration
  #the values will be taken from the vector 'unique(bees_data$site_loc)' which is the names of the location found in the 'site_loc' column of the data
  #so now that we have location set to "Amatzya" and then to "Gidron" and so on... the next two lines are subseting the data according to the corrent value of 'location':
  current_location=subset(bees_data,site_loc==location)
  current_location_sp=current_location[,4:ncol(current_location)]
  acc_location_rarefaction=specaccum(current_location_sp,method="rarefaction")#we run the function 'specaccum' on the data of the current 'location'
  acc_result[[location]]=acc_location_rarefaction#now we will store the output in the list ('rarefy_result') oder the name of the current 'location'. In lists we use a double brackets[[]] to specify an element
}
#look at the list now:
acc_result
#You can see it has 5 elements each is the output of the function 'specaccum' on a diffrent location
#to get elemnts from a list we use [[]] or the name of the element. If I want to see the data for "Amatzya" which is the first elemnt I can use:
acc_result[[1]]
#or
acc_result$Amatzya

## the next block of code will plot all the rarefy_result (for all the sites) on one plot.
plot(acc_result$Amatzya,col="black",ylim = c(0,80),ylab = "Sp richness") #creat plot
lines(acc_result$Gidron,col = "blue") # add line to the plot
lines(acc_result$Shahak,col="red")
lines(acc_result$Sheizaf,col="pink")
lines(acc_result$Znav_Hatzeva,col="green")
legend("bottomright",legend = site_name,lty=c(1,1),col =colors) # add legend to the plot

### for advanced! you can also use the 'sapply'  function to create this plot automatically
ylm <- range(sapply(acc_result, '[[', 'richness') + 
               sapply(acc_result, '[[', 'sd') * c(-2, 2)) # set the rangr for the y axis

# sapply - a plotting function over the indices of the list
sapply(seq_along(acc_result), function(i) { # on every list component aplly the i function that will do this:
  if (i==1) { # If it's the first list element use the plot function
    with(acc_result[[i]], {
      plot(sites, richness, type='l', ylim=ylm, 
           xlab='Sites', ylab='rarefaction', las=1)
      segments(seq_len(max(sites)), y0=richness - 2*sd, #compute and add sd
               y1=richness + 2*sd)
    })    
  } else {
    with(acc_result[[i]], { # for subsequent elements, use the lines function that add lines to plot
      lines(sites, richness, col=i)
      segments(seq_len(max(sites)), y0=richness - 2*sd, ## compute and add sd
               y1=richness + 2*sd, col=i)
    })     
  }
})
legend("bottomright",legend = site_name,lty=c(1,1),col =1:5) # add legend to the plot

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  sample-based rarefaction curve 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# now let's create sample based rarefaction curve for "Amatzya" site

# we will use the sp_amatzya data frame we created earlier.

#now we will rarefy by sample. we will use the function 'rarefaction.sample' from the 
#packge rareNMtest. in this function the first argument is the data name,  the method argument 
#'sample-size" means that the function will use incidence data and 'q = 0' is used for species
#richness rarefaction.

amatzya_sample_rarefy<-rarefaction.sample(sp_amatzya, method = "sample-size", q = 0)

#take alook at the results

amatzya_sample_rarefy


# lets plot the results

plot(amatzya_sample_rarefy,type="l")

#now let's do it for all the sites. the steps are similar to the for-loop we created earlier but 
#instand  of 'specacum' function we'll use 'rarefaction.sample' function.

sample_rarefy_result<-list()

for (location in unique(bees_data$site_loc)){
  current_location=subset(bees_data,site_loc==location)
  current_location_sp=current_location[,4:ncol(current_location)]
  sample_rarefy_location_rarefaction= rarefaction.sample(current_location_sp,method ="sample-size",q = 0)
  sample_rarefy_result[[location]]=sample_rarefy_location_rarefaction 
}

sample_rarefy_result

plot(sample_rarefy_result$Amatzya,col="black",ylim = c(0,80),type="l") #creat plot
lines(sample_rarefy_result$Gidron,col = "blue") # add line to the plot
lines(sample_rarefy_result$Shahak,col="red")
lines(sample_rarefy_result$Sheizaf,col="pink")
lines(sample_rarefy_result$Znav_Hatzeva,col="green")
legend("bottomright",legend = site_name,lty=c(1,1),col =colors) # add legend to the plot

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         compering the rarefaction curevs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#now we are going to  compare to rarefaction  curves using ' EcoTest.individual' function from 'rareNMtests' package
#lets look at the help on this function:
?EcoTest.individual
#Arrguments:
#x- is the data frame
#MARGIN- we will need to set to '2' because our site are in the columns

t1=t(bees_sp[1:4,])
t2=EcoTest.individual(t1,niter=100)
plot(t2)

# there is also EcoTest.sample function that compare between samples base curves.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                    calculating Hill numbers for the site
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#in the renyi function that find R?nyi diversities with any scale or the corresponding Hill number 
# we insert few arumant:
#x=data
#scales = desierd q values
#hill=T : Calculate Hill numbers

#Type '?renyi' to see the help

renyi_profile=renyi(bees_data[,4:ncol(bees_data)],  scales = c(0, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, Inf), hill = T)
renyi_df=data.frame(renyi_profile) #create a data frame from the function output

#Now let's look what we got:
renyi_df#the rows are the sites and the column are the diffrent Hill numbers

#next line is for writing the data to an Excel sheet. look at the ?write.csv help
write.csv(renyi_df,"renyi.csv")


#And now let's plot the results:
plot(renyi_profile)

#what you will see is for each site the diversity index for the range of values you have
#defined in the argument 'scales' in the 'renyi' function.
#When the value is 0 you will see the species richness
#when it is 1 you will get the Shannon diversity index


#lets group the data by site and plot all the sites together for more understandable results

renyi_profile_site<-renyi(bees_per_site_data,scales = c(0, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, Inf), hill = T)
renyi_data<-data.frame(t(renyi_profile_site))
rownames(renyi_data)=NULL
renyi_data$q<-c(0, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, Inf)
q<-renyi_data$q

plot(q,renyi_data$Amatzya,type = 'l',col = "black",xlim =c(0,4),ylab = "Diversity")  # I limited the x-axis because I wanted to focus on the low q numbers
lines(q,renyi_data$Gidron,col="blue")
lines(q,renyi_data$Shahak,col="red")
lines(q,renyi_data$Sheizaf,col="pink")
lines(q,renyi_data$Znav_Hatzeva,col="green")
legend("topright",legend = site_name,lty=c(1,1),col =colors)


#Now try to use the above functions on your own data!