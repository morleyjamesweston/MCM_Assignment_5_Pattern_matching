library(tidyverse)
library(rvest)

student_name <- "Claude Shannon"

# ██╗  ██╗ ██████╗ ███╗   ███╗███████╗██╗    ██╗ ██████╗ ██████╗ ██╗  ██╗
# ██║  ██║██╔═══██╗████╗ ████║██╔════╝██║    ██║██╔═══██╗██╔══██╗██║ ██╔╝
# ███████║██║   ██║██╔████╔██║█████╗  ██║ █╗ ██║██║   ██║██████╔╝█████╔╝ 
# ██╔══██║██║   ██║██║╚██╔╝██║██╔══╝  ██║███╗██║██║   ██║██╔══██╗██╔═██╗ 
# ██║  ██║╚██████╔╝██║ ╚═╝ ██║███████╗╚███╔███╔╝╚██████╔╝██║  ██║██║  ██╗
# ╚═╝  ╚═╝ ╚═════╝ ╚═╝     ╚═╝╚══════╝ ╚══╝╚══╝  ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝

king_lear_scenes <- "ACT I
Scene I. A Room of State in King Lear’s Palace
Scene II. A Hall in the Earl of Gloucester’s Castle
Scene III. A Room in the Duke of Albany’s Palace
Scene IV. A Hall in Albany’s Palace
Scene V. Court before the Duke of Albany’s Palace

ACT II
Scene I. A court within the Castle of the Earl of Gloucester
Scene II. Before Gloucester’s Castle
Scene III. The open Country
Scene IV. Before Gloucester’s Castle

ACT III
Scene I. A Heath
Scene II. Another part of the heath
Scene III. A Room in Gloucester’s Castle
Scene IV. A part of the Heath with a Hovel
Scene V. A Room in Gloucester’s Castle
Scene VI. A Chamber in a Farmhouse adjoining the Castle
Scene VII. A Room in Gloucester’s Castle
"
king_lear_scenes <- king_lear_scenes |> 
  str_split_1("\n") |> 
  as_tibble() |> 
  rename(text = value)


# QUESTION 1
# We will here make a new column called "act",
# Which will just be the act number of the play.
# Make a regex pattern that finds the word ACT, followed by a number.

# Your new column should look like this:
# act   
# <chr> 
# 1 ACT I 
# 2 ACT I 
# 3 ACT I 
# ...
# 7 ACT I 
# 8 ACT II
# 9 ACT II
# ...

act_pattern <- ""  # FILL IN A PATTERN HERE

king_lear_scenes <- king_lear_scenes |> 
  mutate(act = str_extract(text, act_pattern)) |> 
  fill(act, .direction="down")
king_lear_scenes

# QUESTION 2
# Now, we need the scene numbers! Make a second regex that extracts
# the scene numbers. Your 2 new columns should now look like this:
# act    scene     
# <chr>  <chr>     
# 1 ACT I  NA        
# 2 ACT I  Scene I  
# 3 ACT I  Scene II 
# 4 ACT I  Scene III

# (With or without the period are both OK)

scene_pattern <- ""

king_lear_scenes <- king_lear_scenes |> 
  mutate(scene = str_extract(text, scene_pattern))
king_lear_scenes



open_ai <- "Hundreds of OpenAI employees, (employees@openai.org) 
including other top executives (executives@cia.gov), 
threatened to join them at Microsoft (bill.gates@microsoft.com) in an open 
letter addressed to OpenAI’s four-member board (board@openai.com) that called
for the board’s resignation and Altman’s (sam_altman@openai.com) return."

# QUESTION 3
# This is a blurb from a news article that contains some email addresses.
# Write a regex pattern that can extract all the email addresses in parentheses.
# You should get 5 email addresses as output.
email_regex <- ""

email_addresses <- open_ai |> 
  str_extract_all(email_regex)

email_addresses

julia_wu_lyrics <-
"讓他們懷疑我們是不是有買榜 買榜
All day long it’s always my song, my song
打開排行就像在開獎 oh 我的天
天天過年
We just win win win

像有買榜 買榜
All day long it's always my song, my bad
其實我也不想這樣 霸佔排行
I be like 小老鼠 上燈台 偷油吃 下不來"


# QUESTION 4
# Here's some song lyrics that contain both English and Chinese lyrics.
# Write a regex that can extract all lines tht contain some English.

ascii_pattern <- ""

lines_with_english <- julia_wu_lyrics |> 
  str_split_1("\n") |>
  as_tibble() |> 
  filter(str_detect(value, ascii_pattern))
lines_with_english



us_history <- read_html("https://en.wikipedia.org/wiki/History_of_the_United_States")
us_history <- us_history |> 
  html_elements('p') |> 
  html_text() |> 
  paste(collapse = " ")
us_history

# QUESTION 5

# Here's the text of a Wikipedia article.
# I want to know which years are mentioned in the text.
# Write a regex that can extract all the years from the text.
# Don't worry about any years before 1000, we're a young country

year_regex <- ""

years_mentioned <- us_history |> str_extract_all(year_regex)
years_mentioned


# If it worked, here's a little graph for every century mentioned.
years_mentioned[[1]] |> 
  as_tibble() |> 
  rename(year = value) |> 
  mutate(year = as.integer(year)) |>
  mutate(century = floor(year / 100) * 100) |> 
  count(century) |> 
  ggplot() +
  geom_line(aes(x=century, y=n)) +
  labs(
    title="Years mentioned in the 'History of The US' Wikipedia article",
    x="Century",
    y="Total count")



