library(stringr)
library(lubridate)
library(rvest)
library(tidyverse)

nobel <- read_csv("NobelPeace.csv", locale=locale(encoding = "latin1")) %>%
  select(!X1)
sr <- srps <- read_csv("data/srps.csv")


View(nobel) # opens a new window
nobel$AwardSpeech[1] # will show the fourth line of the text column of the nobel dataframe
cat(nobel$AwardSpeech[1]) # nicer formatting but now you're not seeing formatting tags in the text

test <- "     Here \n there and \t\t\t everywhere.\n"
t <- nobel$AwardSpeech[1] 

test_clean <- str_replace_all(test, "[(\\n)(\\r)(\\t)]", ' ')
test_clean <- str_replace_all(test_clean, " {2,}", " ")
test_clean <- str_replace_all(test_clean, "^ | $", "")

str_squish(test)

speach <- nobel$AwardSpeech[1] 
speach %>%
  str_replace_all("\\\u0092", "'") %>%
  str_replace_all("(\\\u0093)|(\\\u0094)", '"') %>%
  str_squish() %>%
  writeLines()

nobel %>%
  filter(Year >= 1950 & Year <= 1980)

for (i in 1:dim(nobel)[1]){
  nobel['AwardSpeech'][i,] <- nobel['AwardSpeech'][i,] %>%
    str_replace_all("\\\u0092", "'") %>%
    str_replace_all("(\\\u0093)|(\\\u0094)", '"') %>%
    str_squish()
}

(nobel <- nobel %>%
  mutate(text = str_replace_all(AwardSpeech, "\\\u0092", "'")) %>%
  mutate(text = str_replace_all(CleanText, "(\\\u0093)|(\\\u0094)", '"')) %>%
  mutate(text = str_replace_all(CleanText, "(\\\u0093)|(\\\u0094)", '"')) %>%
  mutate(text = str_squish(CleanText)))
  

nobel %>%
  filter(Year == 1950 | Year == 1980)
nobel %>%
  filter(Year >= 1950 & Year <= 1954 & Year != 1953)

nobel %>%
  mutate(after_WWII = Year > 1945)

nobel <- nobel %>%
  mutate(wc = str_count(text, '[\\w]+'))

nobel %>%
  ggplot(aes(x = Year, y = wc)) +
  geom_line()

nobel %>%
  summarize(average = sum(wc))

(1984%/%10) * 10

nobel %>%
  mutate(decade = (Year %/% 10) * 10) %>% # uses something called modulo division to get the decade
  group_by(decade) %>%
  summarize(mean(wc)) %>%
  ggplot(aes(decade, `mean(wc)`)) +
    geom_line()

nobel %>%
  mutate(peace = str_count(text, "peace")) %>%
  mutate(war = str_count(text, "war")) %>%
  mutate(humright = str_count(text, "human rights")) %>%
  group_by(Year) %>%
  summarize(peace = sum(peace), war = sum(war), human_rights = sum(humright)) %>%
  pivot_longer(c("peace", "war", "human_rights"), names_to = "word", values_to = "counts") %>%
  ggplot(aes(x = Year, y = counts, color = word)) +
    geom_line()

speach %>%
  str_replace_all("\\\u0092", "'") %>%
  str_replace_all("(\\\u0093)|(\\\u0094)", '"') %>%
  str_squish() %>%
  str_to_lower() %>%
  writeLines()

nobel %>%
  mutate(peace = str_count(text, "[Pp]eace")) %>%
  mutate(war = str_count(text, "[Ww]ar")) %>%
  mutate(humright = str_count(text, "[Hh]uman [Rr]ights")) %>%
  group_by(Year) %>%
  summarize(peace = sum(peace), war = sum(war), human_rights = sum(humright)) %>%
  pivot_longer(c("peace", "war", "human_rights"), names_to = "word", values_to = "counts") %>%
  ggplot(aes(x = Year, y = counts, color = word)) +
  geom_line()

nobel %>%
  mutate(peace = str_count(text, "[Pp]eace")) %>%
  mutate(war = str_count(text, "[Ww]ar")) %>%
  mutate(humright = str_count(text, "[Hh]uman [Rr]ights")) %>%
  filter(Year >= 1960 & Year <= 1975)
