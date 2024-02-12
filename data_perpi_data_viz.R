######Dataviz project
library(tidyverse)
library(parallel)
library(ggplot2)
library(ggbeeswarm)
library(dplyr)
library(tibble)
library(ggrepel)
library(readr)
library(ggridges)

setwd(dir='E:/SIGMA/M2/UE_r_902')
#setwd(dir='D:/SIGMA/M2/UE_r_902')

H31 <- read_delim('H_31_latest-2023-2024.csv', delim = ';')

h31 <- H31[c(1,2,5,6,47)]

h31luchon <- h31[h31$NOM_USUEL == 'LUCHON', ]

get_date  <- function(dataframe, df_date_col)
{
  dataframe$annee <-as.integer(substr(df_date_col, 1, 4))
  dataframe$month <-as.integer(substr(df_date_col, 5, 6))
  dataframe$day <-as.integer(substr(df_date_col, 7, 8))
  dataframe$hour <-as.integer(substr(df_date_col, 9, 10))
  
  return (dataframe)
}

h31luchon <- get_date(h31luchon , h31luchon$AAAAMMJJHH)

h31luchon <- h31luchon[h31luchon$annee == '2023', ]
h31luchon$month <- month.name[h31luchon$month]


#drop missing TN value
h31_alti <- drop_na(h31,TN)
#select data
unique(h31_alti$ALTI)
h31_alti <- h31[h31$ALTI == 109|
                 h31$ALTI == 405|
                 h31$ALTI == 330|
                 h31$ALTI == 618, ]

#Create dd/mm/yyyy Columns
h31_alti <- get_date(h31_alti , h31_alti$AAAAMMJJHH)
h31_alti <- h31_alti[h31_alti$annee == '2023', ]
#h31_alti$month <- month.name[h31_alti$month]


#In form rada chart data
vect_m <- c()
df <- data.frame(matrix(ncol = 12, nrow = 0))
dataf_alti <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(dataf_alti) <-  c('city','month','month_mean_temp')
city <-  unique(h31_alti$NOM_USUEL)

for (i in 1:length(city))
{
  h_city <- h31_alti[h31_alti$NOM_USUEL == city[i], ]
  for (m in 1:12)
  {
    h_city_month <- h_city[h_city$month == m,]
    Mmean <- mean(h_city_month$TN, na.rm = TRUE)
    vect_m[m] <- Mmean
  }
  df[nrow(df) + 1,] = vect_m
  temp_df <- data.frame(rep(city[i],12),month.name,vect_m)
  colnames(temp_df) <-  c('city','month','month_mean_temp')
  dataf_alti <- rbind(dataf_alti, temp_df)

}
temp_df <- NULL
colnames(df) <- month.abb
row.names(df) <- city


### Radar Chart ###

################### Radar Chart fmsb lib ###
library(fmsb)
#ADD Min and Max for Radar Fmsb package
fmsb_df <- rbind(rep(25, 12),rep(0, 12),df)
op <- par(mar = c(1, 2, 2, 2))
radarchart(fmsb_df, col = c("#00AFBB", "#E7B800", "#FC4E07",'#fdc87f'))
legend(
  x = "top", legend = rownames(fmsb_df[-c(1,2),]), horiz = TRUE,#bty = "n",
   pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07",'#fdc87f'),
  cex = 1, pt.cex = 1.5)
par(op)

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    #pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

# Reduce plot margin using par()
op <- par(mar = c(1, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(
  data = fmsb_df, caxislabels = c(0, 5, 10, 15, 20),
  color = c("#00AFBB", "#E7B800", "#FC4E07",'#fdc87f')
)
# Add an horizontal legend
legend(
  x = "bottom", legend = rownames(fmsb_df[-c(1,2),]), horiz = TRUE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07",'#fdc87f'),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)

################### Radar Chart ggradar lib ###
install.packages("devtools")
devtools::install_github("ricardo-bion/ggradar")

library(ggradar)
ggradar_df <- cbind(city,df)
#set colors
library(viridis)
# Get colors from the 'viridis' palette
viridis_col <- viridis_pal(direction = -1)(4)


#order data 
#distinct alti & order
mpg <- h31_alti %>% distinct(NOM_USUEL, .keep_all=TRUE)
mpg <-mpg[order(mpg$ALTI),]
#apply
ggradar_df$city <- factor(ggradar_df$city,      # Reordering group factor levels
                      levels = mpg$NOM_USUEL)

rad <-ggradar(ggradar_df, grid.max = 25,grid.mid = 10, grid.min = 0 ,
        grid.label.size = 3,
        base.size = 40,
        values.radar = c('0°c', '10°c','25°c'),
        group.colours = viridis_col, #ordre alti 3412
        group.line.width = .5,
        group.point.size = 2,
        background.circle.transparency = 0.1,
        axis.label.size = 4,
        legend.text.size = 10,
        plot.legend = FALSE) 
rad


p <- ggplot(mpg, aes(x = c(1:4), y = ALTI,label = NOM_USUEL)) + 
  geom_line(colour = "grey") + 
  geom_point(colour = viridis_col, size=3)+
  theme(axis.text.x = element_blank(), 
        panel.grid.major.y = element_line(color = "grey",linewidth = .2 , linetype = 1),
        panel.background = element_rect(fill = 'white')) +
  scale_x_discrete(name =NULL)+
  geom_text(nudge_y = -40, nudge_x = 0.1, colour = viridis_col ,
            size = 3,fontface = "bold")
p


library("gridExtra")
grid.arrange( p,rad,
             ncol=2, nrow=2, widths=c(1.5, 4), heights=c(4, 0))



#############ggridges
library(viridis)
h31luchon$month <- factor(h31luchon$month,      # Reordering group factor levels
                      levels = month.name)

ggplot(h31luchon,aes(x = TN , y = month,fill = month)) +
  ggridges::geom_density_ridges(scale = 1.5)

ggplot(h31luchon,aes(x = TN , y = month,fill =  ..x..)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_viridis(name = "TN", option = "C")+
  labs(title = 'Temperatures in Luchon city 2023') +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

ridge_density_plot <-                                # Draw densities in ridgeline plot
  h31luchon %>%
  ggplot(aes(x = TN , y = month,  fill = month)) +
  ggridges::geom_density_ridges(scale = 2)           # Overlap among histograms
ridge_density_plot


#passage de 0-23 à 1-24h
h31luchon$hour <- (h31luchon$hour + 1)


is.numeric(h31luchon$TN)

#boucle améliorée
vect_h <- c()
df <- data.frame(matrix(ncol = 24, nrow = 0))
dataf <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(dataf) <-  c('month','hour','month_mean_temp')
for (i in 1:12)
{
  h_31mois <- h31luchon[h31luchon$month == i, ]
  for (h in 1:24)
  {
    h_31_h <- h_31mois[h_31mois$hour == h,] #h-1 pour eviter le passage 1-24
    h_31_h <- h_31_h$TN
    hmean <- mean(h_31_h, na.rm = TRUE)
    vect_h[h] <- hmean
  }
  same_24_month <- rep(month.name[i], 24)
  hour_same <- c(0:23)
  temp_df <- data.frame(same_24_month,hour_same,vect_h)
  colnames(temp_df) <-  c('month','hour','month_mean_temp')
  dataf <- rbind(dataf,temp_df)
  df[nrow(df) + 1,] = vect_h
}

df <- t(df)
df <- data.frame(df)
df$hour <- c(0:23)




data_january <- dataf[dataf$month=='January',]

dataf$month <- factor(dataf$month,      # Reordering group factor levels
                         levels = month.name)

p <- ggplot(data_january, aes(hour,month_mean_temp)) + 
  labs(x = 'hour', y = 'temperature') + # Pas d'Ã©tiquette d'axe
  theme(plot.title = element_text(size = 12)) # Titre taille 12
p #pour sortir le graphe sur le plot
p + geom_line() + ggtitle("type : Average January day") + # Dessin des points
  geom_area(fill = "#1b98e0")



facet_p <- ggplot(dataf, aes(hour,month_mean_temp,fill = month, alpha = 0.7))+ #, color = month)) + 
  labs(x = 'hour', y = 'temperature') + # Pas d'Ã©tiquette d'axe
  theme(plot.title = element_text(size = 12)) +
  geom_area() + geom_line()+ ggtitle("type : average months day") + 
  facet_wrap(~ month, nrow = 4)
facet_p


#create Min Max dataframe
vect_m <- c()
min_max_month <- data.frame(matrix(ncol = 3, nrow = 0))
for (i in 1:12)
{
  data_month <- dataf[dataf$month == month.name[i], ]
  vect_m[1] <- data_month$month
  vect_m[2] <- min(data_month$month_mean_temp)
  vect_m[3] <- max(data_month$month_mean_temp)
  min_max_month[nrow(min_max_month) + 1,] = vect_m
}
colnames(min_max_month) <-  c('month','month_mean_temp','month_mean_temp')
min_max_month$month <- month.name[min_max_month$month]
min_max_month$month <- factor(min_max_month$month,      # Reordering group factor levels
                      levels = month.name)

facet_p <- ggplot(dataf, aes(hour,month_mean_temp,fill = month, alpha = 0.8))+ #, color = month)) + 
  labs(x = 'hour', y = 'temperature') + # Pas d'Ã©tiquette d'axe
  theme(plot.title = element_text(size = 12)) +
  geom_area()+
  #geom_line()+
  ggtitle("Average day by month in Luchon city ") + 
  theme(legend.position = "none")+
  geom_hline(aes(yintercept = month_mean_temp, colour = 'blue'),min_max_month[,-2]) +
  geom_hline(aes(yintercept = month_mean_temp, colour = 'red'),min_max_month[,-3])+
  facet_wrap(~ month, nrow = 4)
facet_p
##Save in 2 000x800

facet_p <- ggplot(dataf, aes(hour,month_mean_temp,fill = month_mean_temp))+ #, color = month)) + 
  labs(x = 'hour', y = 'temperature') + # Pas d'Ã©tiquette d'axe
  theme(plot.title = element_text(size = 12)) +
  geom_area() + geom_line()+ ggtitle("Average months day")+
  scale_fill_gradient2(position="bottom" , low = "blue",  high = "red", 
                       midpoint = 14) + 
  facet_wrap(~ month, nrow = 4)
facet_p

dtest <- dataf
dtest$label <- NA
dtest$label[which(dtest$hour == 0)] <- dtest$month[which(dtest$hour == 0)]
dtest$label <- month.name[dtest$label]

facet_p <- ggplot(dtest,  aes(x = hour,y = month_mean_temp,
  color = month, alpha = 0.7)) + 
  labs(x = 'hour', y = 'temperature') + # etiquette d'axe
  theme(plot.title = element_text(size = 12)) +
  geom_line() + ggtitle("Average Monthly day")+
  geom_label_repel(aes(label = label)) +
  theme(legend.position = "none")
facet_p


###bugger
facet_p <- ggplot(dataf,  aes(x = hour,y = month_mean_temp,
  fill = month, color = month, alpha = 0.5)) + 
  labs(x = 'hour', y = 'temperature') + # etiquette d'axe
  theme(plot.title = element_text(size = 12)) +
  geom_line()+ geom_area() + ggtitle("Average Monthly day")
facet_p


library(viridis)
# Get colors from the 'viridis' palette
viridis_col <- viridis_pal()(12)


ggplot(dataf, aes(x = hour,y = month_mean_temp,
  height = abs(month_mean_temp), group = month, fill = month,alpha = 0.2)) +
  geom_ridgeline()

