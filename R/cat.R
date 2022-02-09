set.seed(1)
a <- rgeom(100, 0.2)

hist_info <- hist(a, plot = F)
hist_info$percent <- hist_info$counts/sum(hist_info$counts)*100
labs <- paste0(round(hist_info$percent), "%")
plot(hist_info, freq = T, labels = labs)        
text()

histPercent <- function(x, ...) {
  H <- hist(x, plot = F)
  H$density <- with(H, counts/sum(counts)*100)
  labs <- paste0(round(H$density), "%")
  plot(H, freq = TRUE, labels = labs,
       ylim = c(0, 1.08*max(H$density)), ...)
}

barPercent <- function(x, ...) {
  H <- barplot(table(x), plot = F)
  H$density <- with(H, counts/sum(counts)*100)
  labs <- paste0(round(H$density), "%")
  plot(H, freq = TRUE, labels = labs,
       ylim = c(0, 1.08*max(H$density)), ...)
}

histPercent(a, col = "green")

data("mtcars")
mtcars[order(-mtcars$mpg), ]

b <- barplot(sort(table(dt$plant), decreasing = F), las = 2, horiz = F, ylim = c(0, 28))
a <- as.matrix(sort(table(dt$plant))/sum(sort(table(dt$plant))))
text(a,b+2,labels = as.character(b))

H <- barplot(table(a), plot = F)
barplot(table(a))


plant_percent <- dt %>% 
  group_by(plant) %>% 
  summarise(count = n()) %>% 
  mutate(plant = factor(plant),
         percent = count/sum(count),
         label = scales::percent(percent)) %>% 
  arrange(desc(percent))

ggplot(plant_percent, aes(x=reorder(plant,percent), y=percent, label=label))+
  geom_bar(stat = "identity")+
  geom_text(hjust = -0.3)+
  coord_flip()+
  theme_minimal()+
  scale_y_continuous(labels = scales::percent_format())+
  xlab("Cây trồng") + ylab("Phần trăm")



new_plant <- dt %>% 
  filter(plant == "bơ"|plant == "bưởi"|plant == "mãng cầu",
         plant.year >= 2010)
ggplot(new_plant, aes(x = plant.year, y = yield, color = plant)) +
  geom_jitter(size = 3)+
  theme_bw() +
  ggrepel::geom_text_repel(aes(x = plant.year, y = yield, label = plant), data = new_plant)