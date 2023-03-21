# homogeneity of variance tests
library(car)
library(stats)

rm(list=ls())

# metaphor generation -----
met <- read.csv("final_datasets/event_blocks.csv")

# remove order_id since have normalized order_id
met <- met %>% select(-c(order_id))

# convert all character columns to factor (categorical) variables, so that R can handle them
met <- met %>% mutate_if(is.character, as.factor)

# removed two extra

metrics <- names(met)[c(6,7,9,14:17)]

fligner.test(elapsed_time~model, data=met)
fligner.test(num_queries~model, data=met) #p-value = 0.03831
fligner.test(acceptance~model, data=met)
fligner.test(apt~model, data=met)
fligner.test(specific~model, data=met)
fligner.test(imageable~model, data=met)
fligner.test(overall~model, data=met)



# iqa -----
rm(list=ls())
iqa <- read.csv("final_datasets/event_blocks_question.csv")

# convert all character columns to factor (categorical) variables, so that R can handle them
iqa <- iqa %>% mutate_if(is.character, as.factor)

# remove attention check questions
iqa <- subset(iqa, iqa$question_type != "attn")
iqa <- droplevels(iqa)

iqa$lm_used <- factor(iqa$lm_used, levels = c(0,1), labels = c(FALSE, TRUE))

metrics <- names(iqa)[c(22:24,21)]

fligner.test(elapsed_time~model, data=iqa)
fligner.test(num_queries~model, data=iqa)
fligner.test(num_events~model, data=iqa)
fligner.test(user_correct~model, data=iqa)

# summarization -----
# read in data
rm(list=ls())
survey <- read.csv("final_datasets/survey_responses_summarization.csv")
res <- read.csv("final_datasets/event_blocks_summarization.csv")

summary(res)
summary(survey)

# convert all character columns to factor (categorical) variables, so that R can handle them
res <- res %>% mutate_if(is.character, as.factor)
res$prompt <- as.factor(res$prompt)
res <- res %>% mutate_if(is.integer, as.numeric)

survey <- survey %>% mutate_if(is.character, as.factor)
survey$prompt <- as.factor(survey$prompt)
survey <- survey %>% mutate_if(is.integer, as.numeric)

cols.num <- c("original_consistency","original_coherency", "original_relevance",
              "edited_consistency","edited_coherency", "edited_relevance")
res[cols.num] <- sapply(res[cols.num],as.numeric)
res[cols.num] <- res[cols.num] - 1


metrics <- names(res)[c(6,16,17:22, 9:11,13:15)]

fligner.test(elapsed_time~model, data=res)
fligner.test(distance~model, data=res)
fligner.test(original_consistency~model, data=res)
fligner.test(original_coherency~model, data=res)
fligner.test(original_relevance~model, data=res)
fligner.test(edited_consistency~model, data=res)
fligner.test(edited_coherency~model, data=res)
fligner.test(edited_relevance~model, data=res)
fligner.test(original_consistency_third_party~model, data=res)
fligner.test(original_coherency_third_party~model, data=res)
fligner.test(original_relevance_third_party~model, data=res)
fligner.test(edited_consistency_third_party~model, data=res)
fligner.test(edited_coherency_third_party~model, data=res)
fligner.test(edited_relevance_third_party~model, data=res)

metrics <- names(survey)[c(5:7)]

fligner.test(improvement~model, data=survey)
fligner.test(edit~model, data=survey)
fligner.test(helpfulness~model, data=survey)

# dialogue ------
rm(list=ls())
res <- read.csv("final_datasets/survey_responses_dialogue.csv")

# convert all character columns to factor (categorical) variables, so that R can handle them
res <- res %>% mutate_if(is.character, as.factor)
res$prompt <- as.factor(res$prompt)

res <- res %>%
  group_by(worker_id) %>%
  summarise(sensibility = mean(sensibility, na.rm = TRUE),
            specificity = mean(specificity, na.rm = TRUE),
            humanness = mean(humanness, na.rm=TRUE),
            interestingness = mean(interestingness, na.rm=TRUE),
            preference = mean(preference, na.rm=TRUE),
            quality = mean(quality, na.rm=TRUE),
            boringness = mean(boringness, na.rm=TRUE),
            fluency = mean(fluency, na.rm=TRUE),
            model=model) %>%
  select(c(worker_id, model, interestingness, boringness, preference, fluency,
           sensibility, specificity, humanness, quality))

res <- res[!duplicated(res),]

metrics <- names(res)[c(3:10)]

fligner.test(interestingness~model, data=res)
fligner.test(boringness~model, data=res)
fligner.test(preference~model, data=res)
fligner.test(fluency~model, data=res)
fligner.test(sensibility~model, data=res)
fligner.test(specificity~model, data=res)
fligner.test(humanness~model, data=res)
fligner.test(quality~model, data=res)

# crossword -----
rm(list=ls())
xw <- read.csv("final_datasets/survey_responses_xword.csv")

# convert all character columns to factor (categorical) variables, so that R can handle them
xw <- xw %>% mutate_if(is.character, as.factor)
xw$prompt <- as.factor(xw$prompt)
xw <- xw %>% mutate_if(is.integer, as.numeric)

xw <- subset(xw, xw$joy >=0) # ONLY for prompt 61

metrics <- names(xw)[5:8]
fligner.test(fluency~model, data=xw)
fligner.test(helpfulness~model, data=xw)
fligner.test(ease~model, data=xw)
fligner.test(joy~model, data=xw)
