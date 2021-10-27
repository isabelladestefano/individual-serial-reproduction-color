require(tidyverse)

test_data_exp1 = read_csv("data/exp1/test_data_exp1.csv")
test_data_exp1 %>% mutate(imgNum = imgNum%%360,
                          items = imgNum%%360)
