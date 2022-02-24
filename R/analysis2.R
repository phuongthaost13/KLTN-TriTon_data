library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(readr)
library(ggpubr)
Sys.setlocale("LC_ALL", "Vietnamese") ## DCm cái này quan trọng lắm

## import TE data
dt <- read_csv(here::here("data/TE2.csv"), locale = locale(encoding = "UTF-8")) ## Te2.csv là file csv UTF-8 encoding
dt <- dt %>% 
  filter(!(plant == ""))

## total
plant_total <- dt %>% 
  count(plant)

model_total <- nrow(unique(dt[c("record", "index")])) ## học hỏi

## Tỉ lệ cây trồng trên tổng số mô hình
plant_percent <- dt %>% 
  group_by(plant) %>% 
  summarise(count = n()) %>% 
  mutate(plant = factor(plant),
         percent = count/model_total,
         label = scales::percent(percent)) %>% 
  arrange(desc(percent))


p1 <- ggplot(plant_percent, aes(x=reorder(plant,percent), y=percent, label=label))+
  geom_bar(stat = "identity")+
  geom_text(hjust = -0.3)+
  coord_flip()+
  theme_bw()+
  scale_y_continuous(labels = scales::percent_format())+
  xlab("Cây trồng") + ylab("Phần trăm")+
  ggtitle("Biểu đồ 1. Tỉ lệ xuất hiện của cây trồng trong tổng số mô hình")

## Cây trồng cho năng suất
plant_has_yield <- dt %>% 
  filter(!(yield == c(0, NA))|!(income.individual == c(0, NA))) %>% 
  group_by(plant) %>% 
  summarise(count = n()) %>% 
  mutate(plant = factor(plant),
         percent = count/sum(count),
         label = scales::percent(percent)) 


p2 <- ggplot(plant_has_yield, aes(x=reorder(plant,percent), y=percent, label=label))+
  geom_bar(stat = "identity")+
  geom_text(hjust = -0.3)+
  coord_flip()+
  scale_y_continuous(labels = scales::percent_format())+
  xlab("Cây trồng") + ylab("Phần trăm")+
  ggtitle("Biểu đồ 2. Tỉ trọng cây trồng cho năng suất")


## Diện tích cây trồng cho năng suất
plant_has_yield_area <- dt %>% 
  filter(!(yield == c(0, NA))|!(income.individual == c(0, NA))) %>%
  group_by(plant) %>% 
  summarise(area = sum(area, na.rm = TRUE)) %>% 
  mutate(plant = factor(plant),
         percent.area = area/sum(area),
         label = scales::percent(percent.area))
p3 <- ggplot(plant_has_yield_area, aes(x = reorder(plant,percent.area), y = (percent.area), label = label))+
  geom_bar(stat = "identity") + coord_flip()+
  scale_y_continuous(labels = scales::percent_format())+
  xlab("Cây trồng") + ylab("Phần trăm")+
  ggtitle("Biểu đồ 3. Diện tích cây trồng cho năng suất")


## Cây trồng cho năng suất phân theo xã
plant_has_yield_by_village <- dt %>% 
  filter(!(yield == c(0, NA))|!(income.individual == c(0, NA))) %>%
  group_by(village, plant) %>% 
  summarise(count = n()) %>% 
  mutate(plant = factor(plant),
         percent = count/sum(count),
         label = scales::percent(percent))

p4 <- ggplot(plant_has_yield_by_village, aes(x=reorder(plant,percent), y=percent, label=label, fill = village))+
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,0.6))+
  geom_text(hjust = -0.3)+
  xlab("Cây trồng") + ylab("Phần trăm")+
  coord_flip() +
  facet_grid(.~village)+
  ggtitle("Biểu đồ 4. Tỉ trọng cây trồng cho năng suất phân theo xã")+
  theme(legend.title = element_blank())

## Lịch sử tồn tại và xuất hiện của cây trồng
plant_history <- dt %>% 
  filter(!(yield == c(0, NA))|!(income.individual == c(0, NA))) %>%
  group_by(plant.year,plant) %>% 
  summarise(count = n()) 


p5 <- ggplot(plant_history, aes(x = plant.year, y = count, color = plant)) +
  geom_jitter(height = 0.1)+
  theme_bw() + theme(legend.position = "none")+
  ggrepel::geom_text_repel(aes(x = plant.year, y = count, label = plant), data = plant_history)+
  xlab("Năm trồng") + ylab("Tổng số mô hình trồng loại cây")+
  ggtitle("Biểu đồ 5. Thời điểm cây được trồng")


## Đánh giá về độ hiệu quả kinh tế
economic_plant_dt <- read_csv(here::here("data/economicPlant.csv"))

## reshape data: from wide to long table
economic_plant_dt_long <- economic_plant_dt %>% 
  select(!(ends_with("reason"))) %>% 
  gather(plant,n,2:ncol(.), factor_key = T) %>% 
  drop_na()


economic_plant <- economic_plant_dt_long %>% 
  group_by(plant) %>% 
  summarise(count = n()) %>% 
  mutate(plant = factor(plant),
         percent = count/sum(count),
         label = scales::percent(percent)) %>% 
  arrange(desc(percent))


p6 <- ggplot(economic_plant, aes(x=reorder(plant,percent), y=percent, label=label))+
  geom_bar(stat = "identity")+
  geom_text(hjust = -0.3)+
  coord_flip()+
  theme_minimal()+
  scale_y_continuous(labels = scales::percent_format())+
  xlab("Cây trồng") + ylab("Phần trăm")+
  ggtitle("Biểu đồ 6. Cây trồng mang lại giá trị kinh tế cho nông hộ")


## Hiệu quả kĩ thuật


## Dự đoán cây trồng chủ đạo trong tương lai
## plant history
plant_history <- dt %>% 
  group_by(plant.year,plant) %>% 
  summarise(count = n()) 


p7 <- ggplot(plant_history, aes(x = plant.year, y = count, color = plant)) +
  geom_jitter(height = 0.1)+
  theme_bw() + theme(legend.position = "none")+
  ggrepel::geom_text_repel(aes(x = plant.year, y = count, label = plant), max.overlaps = 20, data = plant_history)+
  annotate("point", x = 1993, y = 2.5, colour = "black", size = 3)+
  annotate("text", x = 1993, y = 2.5, label = "1993", colour = "black", vjust = -0.5)+
  xlab("Năm trồng") + ylab("Tổng số mô hình trồng loại cây")+
  ggtitle("Biểu đồ 7. Thời điểm cây được trồng")

## các cây trồng mới sau năm 2010
new_plant <- dt %>% 
  filter(plant == "bơ"|plant == "bưởi"|plant == "mãng cầu")

p8 <- ggplot(new_plant, aes(x = plant.year, y = yield, color = plant)) +
  geom_jitter(size = 3)+
  theme_bw() +
  ggrepel::geom_text_repel(aes(x = plant.year, y = yield, label = plant), data = new_plant)+
  xlab("Năm trồng") + ylab("Năng suất")+
  ggtitle("Biểu đồ 8. Tương quan giữa năm trồng và năng suất của cây bơ, bưởi, mãng cầu")

## Lịch sử phát triển của từng loại cây trồng
p9 <- ggplot(plant_history, aes(x = plant.year, y = count, color = plant)) +
  geom_jitter(size = 1)+
  theme_bw() + theme(legend.position = "none")+
  facet_wrap(~plant)+
  xlim(c(1970,2018))+
  ggtitle("Biểu đồ 9. Lịch sử các loài cây trồng ở khu vực Núi Dài, Tri Tôn")

## Số lượng cây trồng và diện tích trồng

## Số lượng cây trồng
## cây ăn quả
caq_number <- dt %>% 
  filter(plant %in% c("bưởi", "xoài", "xoài c", "xoài b", "xoài tc", "xoài dl", "cam", "quýt", "sầu riêng", "mãng cầu", "vú sữa", "bở", "chuối", "mít", "đu đủ", "sapo", "dừa")) %>% 
  group_by(plant) %>% 
  summarise(number = sum(plant.number, na.rm = TRUE))

caq_number_plot <- ggplot(caq_number, aes(x = reorder(plant, number), y = number)) +
  geom_bar(stat = "identity") + coord_flip() + 
  xlab("Cây trồng") + ylab("Số lượng (cây)")+
  ggtitle("Số lượng cây ăn quả")


## cây thân thảo
herb_number <- dt %>% 
  filter(plant %in% c("nghệ", "ngải bún", "dưa leo", "củ lùn", "khổ qua", "chanh", "chúc", "đậu rồng")) %>% 
  group_by(plant) %>% 
  summarise(number = sum(plant.number, na.rm = TRUE))

herb_number_plot <- ggplot(herb_number, aes(x = reorder(plant, number), y = number)) +
  geom_bar(stat = "identity") + coord_flip()+
  xlab("Cây trồng") + ylab("Số lượng (cây)")+
  ggtitle("Số lượng cây rau và dược liệu")

## cây rừng
forest_number <- dt %>% 
  filter(plant %in% c("sao", "tầm vông", "giáng hương", "dó bầu", "tràm", "sưa", "tóc", "keo")) %>% 
  group_by(plant) %>% 
  summarise(number = sum(plant.number, na.rm = TRUE))

forest_number_plot <- ggplot(forest_number, aes(x = reorder(plant, number), y = number)) +
  geom_bar(stat = "identity") + coord_flip()+
  xlab("Cây trồng") + ylab("Số lượng (cây)")+
  ggtitle("Số lượng cây rừng")


p10 <- ggarrange(caq_number_plot, herb_number_plot, forest_number_plot,
          ncol = 2, nrow = 2) %>% 
  annotate_figure(p10, top = text_grob(" "),
                fig.lab.pos = "top.left",
                fig.lab = "Biểu đồ 10. Số lượng cây trồng",
                fig.lab.size = 13)

## Diện tích cây trồng
# cây ăn quả
caq_area <- dt %>% 
  filter(plant %in% c("bưởi", "xoài", "xoài c", "xoài b", "xoài tc", "xoài dl", "cam", "quýt", "sầu riêng", "mãng cầu", "vú sữa", "bở", "chuối", "mít", "đu đủ", "sapo", "dừa")) %>% 
  group_by(plant) %>% 
  summarise(area = sum(area, na.rm = TRUE))

caq_area_plot <- ggplot(caq_area, aes(x = reorder(plant, area), y = area)) +
  geom_bar(stat = "identity") + coord_flip() + 
  xlab("Cây trồng") + ylab("Diện tích (ha)")+
  ggtitle("Diện tích cây ăn quả")


## cây thân thảo
herb_area <- dt %>% 
  filter(plant %in% c("nghệ", "ngải bún", "dưa leo", "củ lùn", "khổ qua", "chanh", "chúc", "đậu rồng")) %>% 
  group_by(plant) %>% 
  summarise(area = sum(area, na.rm = TRUE))

herb_area_plot <- ggplot(herb_area, aes(x = reorder(plant, area), y = area)) +
  geom_bar(stat = "identity") + coord_flip() + 
  xlab("Cây trồng") + ylab("Diện tích (ha)")+
  ggtitle("Diện tích cây rau và dược liệu")


## cây rừng
forest_area <- dt %>% 
  filter(plant %in% c("sao", "tầm vông", "giáng hương", "dó bầu", "tràm", "sưa", "tóc", "keo")) %>% 
  group_by(plant) %>% 
  summarise(area = sum(area, na.rm = TRUE))

forest_area_plot <- ggplot(forest_area, aes(x = reorder(plant, area), y = area)) +
  geom_bar(stat = "identity") + coord_flip()+
  xlab("Cây trồng") + ylab("Diện tích (ha)")+
  ggtitle("Diện tích cây rừng")

p11 <- ggarrange(caq_area_plot, herb_area_plot, forest_area_plot,
          ncol = 2, nrow = 2) %>% 
  annotate_figure(p10, top = text_grob(" "),
                  fig.lab.pos = "top.left",
                  fig.lab = "Biểu đồ 11. Diện tích cây trồng",
                  fig.lab.size = 13)


# Các nhân tố ảnh hưởng đến quyết định trồng cây của người nông dân
## Nhân tố văn hóa và xã hội
general_info <- read_csv(here::here("data/overview.csv"))

## Tuổi
age_plot <- ggplot(general_info, aes(x=age, y = ..density..))+
  geom_histogram()+ theme_bw() +
  scale_y_continuous(labels = scales::percent_format())+
  xlab("Tuổi") + ylab("Phần trăm") +
  ggtitle("Biểu đồ 12. Tuổi của chủ hộ") + geom_density(size=2, color = "red")+
  geom_vline(xintercept = 52, color = "red", size = 2)

## adding mean and sd to the plot
table <- data.frame("mean" = round(mean(general_info$age, na.rm = T)),
                    "sd" = round(sd(general_info$age, na.rm = T)))
table.p <- ggtexttable(table, rows = NULL, 
                       theme = ttheme(("mOrange")))

p12 <- age_plot + annotation_custom(ggplotGrob(table.p),
                             xmin = 65, ymin = 0.05,
                             xmax = 80)

## Kinh nghiệm làm nông
agri_working_plot <- ggplot(general_info, aes(x=agri.working, y = ..density..))+
  geom_histogram()+ theme_bw() +
  scale_y_continuous(labels = scales::percent_format())+
  xlab("Thơi gian (năm)") + ylab("Phần trăm") +
  ggtitle("Biểu đồ 13. Kinh nghiệm làm nông nghiệp") + geom_density(size=2, color = "red")+
  geom_vline(xintercept = 27, color = "red", size = 2)

## adding mean and sd to the plot
table_agri_working <- data.frame("mean" = round(mean(general_info$agri.working, na.rm = TRUE)),
                                 "sd" = round(sd(general_info$agri.working, na.rm = TRUE)))

table_agri_working <- ggtexttable(table_agri_working, rows = NULL, 
                                  theme = ttheme(("mOrange")))

p13 <- agri_working_plot + annotation_custom(ggplotGrob(table_agri_working),
                                      xmin = 40, ymin = 0.08,
                                      xmax = 50)  

## Tỉ lệ lao động nông nghiệp
agri_labor <- general_info %>% 
  mutate(agri.percent = labor.agri/labor.total,
         non.agri.percent = 1 - agri.percent)

p14 <- ggplot(agri_labor, aes(x = agri.percent))+
  geom_histogram(aes(y=..density.., fill = village), color = "black")+ theme_bw()+
  facet_grid(.~village)+
  theme(legend.position = "none")+
  ggtitle("Biểu đồ 14. Tỉ lệ lao động nông nghiệp")


## Dân tộc
p15 <- ggplot(general_info, aes(x = ethnic, group = village))+
  geom_bar(aes(y=..prop.., fill = factor(..x..)), stat = "count", color = "black")+ theme_bw()+
  facet_grid(.~village)+
  scale_y_continuous(labels = scales::percent_format())+
  xlab("Dân tộc") + ylab("Phần trăm")+
  ggtitle("Biểu đồ 15. Tỉ trọng dân tộc ở ba xã Ba Chúc, Lê Trì, Ô Lâm thuộc núi Dài, Tri Tôn")+
  theme(legend.position = "none")


# Sinh thái
land_property <- read_csv(here::here("data/landProperty.csv"))

## tình trạng nước
water_status <- land_property %>% 
  filter(!(water.status == "NA"))

water_status_plot <- ggplot(water_status, aes(x = water.status, group = village))+
  geom_bar(aes(y=..prop.., fill = factor(..x..)), stat = "count", color = "black")+ theme_bw()+
  facet_grid(.~village)+
  scale_y_continuous(labels = scales::percent_format())+
  xlab("Tình trạng nước") + ylab("Phần trăm")+
  ggtitle("Đặc điểm nguồn nước Tri Tôn")+
  theme(legend.position = "none")


## tình trạng đất
soil_status <- land_property %>% 
  filter(!(soil.characteristic == "NA"))

soil_status_plot <- ggplot(soil_status, aes(x = soil.characteristic, group = village))+
  geom_bar(aes(y=..prop.., fill = factor(..x..)), stat = "count", color = "black")+ theme_bw()+
  facet_grid(.~village)+
  scale_y_continuous(labels = scales::percent_format())+
  xlab("Đặc điểm đất") + ylab("Phần trăm")+
  ggtitle("Đặc điểm đất ở Tri Tôn")+
  theme(legend.position = "none")


## tình trạng độ dốc
slope_status <- land_property %>% 
  filter(!(land.slope == "NA"))

slope_status_plot <- ggplot(slope_status, aes(x = land.slope, group = village))+
  geom_bar(aes(y=..prop.., fill = factor(..x..)), stat = "count", color = "black")+ theme_bw()+
  facet_grid(.~village)+
  scale_y_continuous(labels = scales::percent_format())+
  xlab("Tình trạng dốc") + ylab("Phần trăm")+
  ggtitle("Đặc điểm độ dốc ở Tri Tôn")+
  theme(legend.position = "none")


p16 <- ggarrange(water_status_plot, soil_status_plot, slope_status_plot,
          ncol = 2, nrow = 2) %>% 
  annotate_figure(p10, top = text_grob(" "),
                  fig.lab.pos = "top.left",
                  fig.lab = "Biểu đồ 16. Đặc điểm sinh thái Tri Tôn",
                  fig.lab.size = 13)


## correlation between age and plant



## plots export
for(i in 16) {
  png(paste0("fig/plot", i,".png"),
      width = 1056, height = 545)
  p <- get(paste("p",i,sep=""))
  plot(p)
  dev.off()
}


