## divide the dataset into 2 subset: Tri Ton data and Tinh Bien data
## It's safer to change all the text to lowercase
to.dataframe$district <- tolower(to.dataframe$district)

TinhBien <- to.dataframe %>% 
  filter(district == "tinh bien")

TriTon <- to.dataframe %>% 
  filter(district == "tri ton")

#write.csv(TinhBien, "data/tinhbien.csv", row.names = F)
#write.csv(TriTon, "data/triton.csv", row.names = F)
