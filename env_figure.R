# UMB database environment figure
# "Thu Jul 23 08:38:40 2020"
# A. Putt | I. Alamilla

# Load packages
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("dplyr")
install.packages("extrafont")
install.packages("RColorBrewer")
library(ggplot2)
library(ggrepel)
library(dplyr)
library(extrafont)
library(RColorBrewer)


# This allows the data to be read for easy identification in later code. 
# This is extremely important since the figure will be made from data within the file.
umb_data<- read.csv("C:/Users/ialamill/Desktop/R/UMBDatabase.csv", fileEncoding="UTF-8-BOM")

# Checks the data structure. It will make the columns within the data file a factor that will be utilized in the figure. 
sapply(umb_data,class)
View(umb_data)

# Sees column names. These will be use to call the environment, taxa, etc. for the figure.
data_cols <-colnames(umb_data)

# Prints the column names so it can ensure that the data is read properly.
print(data_cols)

# Groups the data by environment. 
# Viewing will ensure that all of the environments are collectively together with respect to taxa within that environment.
by_env_tax <- umb_data %>% group_by(simple_environment, simple_taxonomy)
View(by_env_tax)

# Condenses the respecive taxa within an environment that will be utilized for the figure.
by_env <- by_env_tax %>% summarise(N = n())
View(by_env)

# Produces a heat map of environment vs taxa.
umb_heatmap <- ggplot(data = remove_missing(by_env, na.rm = TRUE), aes(y = simple_taxonomy, x = simple_environment, fill = N)) +
  geom_tile(colour = "white") + scale_fill_gradient2(midpoint = 30,
                                                     low = "#d73027", 
                                                     mid = "#fee090", 
                                                     high = "#4575b4") +
  theme_linedraw(base_family = "serif", base_size = 15, base_line_size = 1) + 
  xlab("Environment") + ylab("Taxonomy") + labs(fill = "Number of Taxa") +
  theme(panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(angle = -60, hjust = 1, color = "black"), 
        axis.title.x = element_text(size = 20, face = "bold", color = "black"), 
        axis.title.y.left = element_text(size = 20, face = "bold", color = "black"),
        legend.position = "bottom") +
  theme(legend.title = element_text(size=15, face="bold", hjust = 0.5),
        legend.text = element_text(size = 12),
        legend.key.width = unit(25, "pt")) +
  guides(fill = guide_colorbar(title.position="top", ticks.linewidth = 1.5, ticks.colour = "black")) + 
  scale_x_discrete(position = "top")
print(umb_heatmap)

# Produces plot using different sized points along with heatmap of enviornment vs taxa.
p <- ggplot(data = remove_missing(by_env, na.rm = TRUE), aes(y = simple_taxonomy, x = simple_environment, size = N, color = N)) +
  theme_bw(base_family = "serif", base_size = 15, base_line_size = 1) + geom_point() +
  scale_color_gradient2(midpoint = 30, low = "#d73027", mid = "#fee090", high = "#4575b4") + 
  theme(axis.text.y = element_text(color = "black"), 
        axis.text.x = element_text(angle = -60, hjust = 1, color = "black"),
        axis.title.x = element_text(size = 20, face = "bold"), 
        axis.title.y.left = element_text(size = 20, face = "bold"), 
        plot.title = element_text(face = "bold", size = 30, hjust = 0.5),
        legend.position = "bottom") + 
  scale_x_discrete(position = "top") +
  xlab("Environment") + ylab("Taxonomy") + labs(color = "Number of Taxa", size ="") + 
  theme(legend.title = element_text(size = 15, face="bold", vjust = 2), 
        legend.text = element_text(size = 12),
        legend.key.width = unit(25, "pt")) +
  guides(color = guide_colorbar(title.position = "top", title.hjust = -3, ticks.linewidth = 1.5, ticks.colour = "black"),
         size = guide_legend(label.position = "bottom", title.position = "top"))
print(p)


# Produces a png file of each corresponding figure.
ggsave("umb_heatmap.png", width = 20, height = 15, scale = 0.5)
ggsave("p.png", width = 20, height = 15, scale = 0.5)

