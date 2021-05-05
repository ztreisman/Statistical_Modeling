library(dplyr)
library(tidyr)

veg_data <- read.csv("data/West_Fork_Plants.csv")
View(veg_data)

# create data matrix species cover

spp_cov<-as_tibble(veg_data [c("Plot_ID", "Code", "Cover")])
spp_cov<-with(spp_cov, spp_cov[order(spp_cov$Code), ])
View(spp_cov)
spp_cov<-spp_cov[!(spp_cov$Code == "Unknown"),] # get rid of unknowns
spp_cov_matrix<-spp_cov %>% pivot_wider(names_from = Code, 
                                        values_from = Cover,
                                        values_fn = sum)
spp_cov_matrix[is.na(spp_cov_matrix)] <- 0 # zeros instead of NAs
View(spp_cov_matrix)

climate71_00 <- read.csv("data/mean_71_00.csv")

climate71_00 <- merge(climate71_00, plot_groups, by.x = "ID2", by.y = "Plot_ID")
climate71_00 <- na.omit(climate71_00)

pca.climate <- prcomp(climate71_00[,-c(1:5,29)], scale = TRUE)

biplot(pca.climate)

ggbiplot(pca.climate, obs.scale = 1, var.scale = 1,
         groups = climate71_00$bc_6, ellipse = TRUE, circle = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')

data(wine)
wine.pca <- prcomp(wine, scale. = TRUE)
print(ggbiplot(wine.pca, obs.scale = 1, var.scale = 1, groups = wine.class, ellipse = TRUE, circle = TRUE))


ggscreeplot(pca.climate)

summary(pca.climate)$importance[,1:5]

get_codes <- function(x)unique(veg_data[veg_data$bc_6==x,]$Code)
get_codes(2)
veg_data$bc_6==2

only2 <- get_codes(2)
for(i in setdiff(1:6,2)) {
  only2 <- setdiff(only2, get_codes(i))
}
only2

veg_data %>% group_by(bc_6) %>%
  summarise(num_sites = length(unique(Plot_ID)))



plants <- read.csv("data/Siberia_plant_data.csv")
plants$transect <- factor(plants$transect)
plants[plants$month == "June",]$month <- "june"
plants[plants$month == "July",]$month <- "july"
plants <- droplevels(plants) 
plants$first_hit<-!is.na(plants$first_hit)
plants$ground_cover <- factor(trimws(tolower(plants$ground_cover)))
plants$species <- factor(trimws(plants$species))
plant_plots <- plants %>%  
  dplyr::group_by(site, treatment, plot, Func_group) %>%
  dplyr::summarize(hits = sum(hits_count),
            pins = n()) 
plant_plot_matrix <- plants %>% 
  dplyr::group_by(plot, Func_group) %>%
  dplyr::summarise(hits = sum(hits_count)) %>% 
  pivot_wider(names_from = Func_group, 
              values_from = hits,
              values_fill = list(hits = 0))

heatmap(as.matrix(plant_plot_matrix[,-1]))
plant_count_2<-cutree(hclust(dist(as.matrix(plant_plot_matrix[,-1]))), k=2)
plant_plot_matrix$pc2 <- factor(plant_count_2)
plant_count_2m <- cutree(hclust(dist(plant_plot_matrix[,-c(1,13)], method = "manhattan")), k=2)
plant_plot_matrix$pc2m <- factor(plant_count_2m)
plant_count_2b <- cutree(hclust(vegdist(plant_plot_matrix[,-c(1,13,14)],"bray")), k=2)
plant_plot_matrix$pc2b <- factor(plant_count_2b)

plot(jitter(plant_count_2)~plant_plot_matrix$GRAM)
plot(jitter(plant_count_2)~plant_plot_matrix$EVSH)
