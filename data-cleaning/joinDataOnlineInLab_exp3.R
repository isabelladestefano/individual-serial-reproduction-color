
test_data_exp3_online = read_csv("data/exp3/online/test_data_exp3_online.csv") %>% mutate(curID = as.character(curID))
test_data_exp3_inlab = read_csv("data/exp3/inlab/test_data_exp3_inlab.csv") %>% mutate(curID = as.character(curID))


test_data_exp3 = bind_rows(test_data_exp3_online, test_data_exp3_inlab)

subjectIDs = test_data_exp3 %>% 
  group_by(curID) %>% 
  summarise() %>% 
  mutate(subject = 1:nrow(.))

test_data_exp3 = subjectIDs %>% 
  inner_join(test_data_exp3 %>% select(-subject), by = c("curID"))


chain_data_exp3_online = read_csv("data/exp3/online/chain_data_exp3_online.csv") %>% mutate(curID = as.character(curID))
chain_data_exp3_inlab = read_csv("data/exp3/inlab/chain_data_exp3_inlab.csv") %>% mutate(curID = as.character(curID))

chain_data_exp3 = bind_rows(chain_data_exp3_online, chain_data_exp3_inlab)

chain_data_exp3 = subjectIDs %>% 
  inner_join(chain_data_exp3 %>% select(-subject), by = c("curID"))

subject_fix = chain_data_exp3$subject %>% unique %>% data.frame(subject=.) %>% mutate(sub = 1:70)
chain_data_exp3 = chain_data_exp3 %>% inner_join(subject_fix, by = c("subject")) %>% mutate(subject = sub) %>% select(-sub)


print(paste0("test_data_exp3 subjects: ", test_data_exp3 %>% pull(subject) %>% unique() %>% length()))
print(paste0("chain_data_exp3 subjects: ", chain_data_exp3 %>% pull(subject) %>% unique() %>% length()))
chain_data_exp3$colorVal = sapply(as.integer(chain_data_exp3$imgNum),convert360ToColorVal)

write_csv(test_data_exp3, "data/exp3/test_data_exp3.csv")
write_csv(chain_data_exp3, "data/exp3/chain_data_exp3.csv")

