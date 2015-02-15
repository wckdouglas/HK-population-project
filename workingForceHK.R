#!/bin/env Rscript

#load packages
require(ggmap)
require(data.table)
require(plyr)
require(dplyr)
require(parallel)
require(reshape2)

#set up function
getName <- function(x){
	# clean up location string
	place <- unlist(strsplit(x,'-'))[1]
	region <- unlist(strsplit(x,'-'))[2]
	return(paste(place,region,sep=' , '))
}

getLattitude <- function(x){
	# return lontitude and latitude for a given place
	geocode(x)
}

clean_label <- function(x){
	paste(unlist(strsplit(x,'_')),collapse=' of total ')
}
#read data
dat <- fread('table.csv')
setnames(dat,make.names(colnames(dat)))

# clean up table
dat <- dat[grep('Total',District.Council...Constituency.Area,invert=T)] %>% 
	filter(!is.na(Working.Population)) %>%
	mutate(area = sapply(District.Council...Constituency.Area,getName), 
		percentage_work = Working.Population/sum(Working.Population),
		percentage_population = Population/sum(Population))  %>%
	select(-V4,-District.Council...Constituency.Area)

# add lontitude and latitude
result <- mclapply(dat$area ,getLattitude, mc.cores=4)
l <- do.call(rbind,result)
dat <- cbind(dat,l) %>% 
	mutate (percentage_working_pop = Working.Population/Population) %>%
	select(area,lon,lat,
		percentage_population,percentage_work,percentage_working_pop) 

# reshape data for plotting
dat.m <- melt(dat,
		id.var=c('area','lon','lat'),
		measures.var=c('percentage_work',
				'percentage_population',
				'percentage_working_pop')) %>%
	filter(variable != 'percentage_working_pop') %>%
	mutate(label = sapply(as.character(variable),clean_label)) %>%
	select(-variable)

hk <- get_map('hong kong',zoom=11,maptype='terrain',
	source='google',crop=T)

# plotting graph
p <- ggmap(hk,legend = 'topleft') +
        geom_point(data=dat.m,aes(x=lon,
                                y=lat,
                                color=value),
                size=3)+
        facet_grid(.~label)+
        scale_color_gradient(low='yellow',high='red')+
        labs(title='Fraction of Working Force and Population in HK',
                color='Fraction',x='lonitude',
                y='latitude')
ggsave(p,file='percentage_workforce.png')
