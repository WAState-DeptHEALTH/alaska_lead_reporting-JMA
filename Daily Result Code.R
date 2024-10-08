CDC.venous.order <- final %>% lazy_dt(immutable=TRUE) %>% group_by(CDC_Child_ID, CDC_TestDate) %>% arrange(CDC_SampleTy, desc(result_num_final), .by_group = TRUE)%>% collect()

#Use a 1 to indicate test result representative of that day
cases <- CDC.venous.order %>% lazy_dt(immutable=TRUE) %>% group_by(CDC_Child_ID, CDC_TestDate) %>% mutate(MaxVenous = row_number())%>% collect()

CDC.cap.order <- cases %>% lazy_dt(immutable=TRUE) %>% group_by(CDC_Child_ID, CDC_TestDate) %>% arrange(result_num_final, .by_group = TRUE)%>% collect()

cases <- CDC.cap.order %>% lazy_dt(immutable=TRUE)  %>% group_by(CDC_Child_ID, CDC_TestDate) %>% mutate(MinResult = row_number())%>% collect()

##Is there a venous test in this group?
cases <- cases %>% lazy_dt(immutable=TRUE) %>% group_by(CDC_Child_ID, CDC_TestDate) %>% mutate(use.venous = min(CDC_SampleTy))

cases <- cases%>% lazy_dt(immutable=TRUE) %>% 
  ungroup() %>%
  mutate(DailyResult = ifelse(use.venous == 1, MaxVenous, MinResult)) %>%
  dplyr::select(TestID, DailyResult)%>% collect()

##Joins daily status to the original data file
final <- final %>% lazy_dt(immutable=TRUE) %>%
  dplyr::select(-DailyResult) %>%
  left_join(cases, by = "TestID")%>% collect()

final <- final %>% lazy_dt(immutable=TRUE) %>%
  mutate(DailyResult = ifelse(DailyResult != 1, 0, DailyResult)) %>% collect()

final <- final %>% lazy_dt(immutable=TRUE) %>%
  dplyr::select(-NameDOB)%>% collect()