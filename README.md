# Working with Twitter Data in R

This repository holds the materials for a workshop on working with Twitter data in R, organised at [Campus Luzern](https://www.campus-luzern.ch/).

Below are the workshop materials kept in this repository. 

### Data

Twitter's [Developer terms](https://developer.twitter.com/en/developer-terms/agreement-and-policy) allow for up to 1,500,000 Tweet IDs or 50,000 public Tweets to be deposited online. Hence the materials (compare `status_ids.rds` and `tweets.rds` below) fallow these terms.

- `data/mps.csv`
  - a dataset on the members of parliament (MPs) in the British House of Commons, as of January 2021
  - it includes variables on electoral results as well as Twitter usernames

- `data/status_ids.rds`
  - a dataset with a single variable: `status_id`
  - lists the status IDs of all tweets posted by the MPs listed in `mps.csv`, during January 2021
  - has 86,125 observations
    
- `data/tweets.rds`
  - similar to `data/status_ids`, except that
    - the time period is now limited to 15 to 31 January, reducing the number of observations below 50,000, allowing for all variables to be posted


### Exercises


- `exercises/solutions.R`

  - solutions for exercises until the end of Part 4

- `exercises/tweets.Rmd` and `exercises/tweets_answers.Rmd`

  - exercises and solutions for Part 6

- `exercises/userss.Rmd` and `exercises/users_answers.Rmd`

  - exercises and solutions for Part 5


### Presentation

- `presentation/twtr_workshop.Rmd`, `presentation/twtr_workshop.html`, and `presentation/twtr_workshop.pdf`

  - slides in three formats

- `presentation/twtr_workshop_files/` and `presentation/libs/`

  - files necessary to produce the slides
