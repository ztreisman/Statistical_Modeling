## -----------------------------
## Setup
## -----------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(nlme)

set.seed(313)

## -----------------------------
## 1. Simulation parameters
## -----------------------------

n_schools    <- 50
students_per <- 20
grades       <- 3:6        # grade 3,4,5,6
n_grades     <- length(grades)

# Fixed effects
beta_0       <- 50         # baseline mean at grade 3
beta_1       <- 5          # avg yearly gain (no intervention)
beta_SES     <- 3          # SES effect on score
beta_int_eff <- 2          # one-time boost from intervention in grade 4

# Random effects SDs
sigma_school_intercept <- 4   # between-school baseline differences
sigma_school_slope     <- 1   # between-school growth differences
sigma_student_intercept <- 6  # between-student baseline differences
sigma_student_slope     <- 2  # between-student growth differences

# Residual SD (within-student, within-year)
sigma_eps <- 6

## -----------------------------
## 2. Simulate school-level structure
## -----------------------------

schools <- data.frame(
  school_id = factor(1:n_schools)
)

# Random intercepts and slopes for schools (nested level) [page:1]
Sigma_school <- matrix(c(
  sigma_school_intercept^2, 0,
  0, sigma_school_slope^2
), 2, 2)

school_ranef <- MASS::mvrnorm(n_schools, mu = c(0, 0), Sigma = Sigma_school)
colnames(school_ranef) <- c("u0_school", "u1_school")

schools <- cbind(schools, school_ranef)

## Assign half the schools to have the intervention available (policy change)
schools$intervention_school <- rbinom(n_schools, size = 1, prob = 0.5)

## -----------------------------
## 3. Simulate students and SES
## -----------------------------

students <- expand.grid(
  school_id = schools$school_id,
  student_in_school = 1:students_per
) %>%
  mutate(
    student_id = factor(paste0(school_id, "_", student_in_school)),
    # SES: standardized-ish, mean 0, SD 1
    SES = rnorm(n(), mean = 0, sd = 1)
  )

# Random intercepts and slopes for students [page:1]
Sigma_student <- matrix(c(
  sigma_student_intercept^2, 0,
  0, sigma_student_slope^2
), 2, 2)

student_ranef <- MASS::mvrnorm(nrow(students), mu = c(0, 0), Sigma = Sigma_student)
colnames(student_ranef) <- c("u0_student", "u1_student")

students <- cbind(students, student_ranef)

## -----------------------------
## 4. Expand to long format: student x grade
## -----------------------------

dat <- students %>%
  select(school_id, student_id, SES, u0_student, u1_student) %>%
  left_join(schools, by = "school_id") %>%
  expand(nesting(school_id, student_id, SES,
                 u0_student, u1_student,
                 u0_school, u1_school,
                 intervention_school),
         grade = grades) %>%
  mutate(
    year = grade - min(grade),   # 0,1,2,3 like in the paper [page:1]
    # Low-SES indicator (for targeting intervention)
    low_SES = ifelse(SES < 0, 1, 0),
    # Intervention happens in grade 4 for eligible students
    intervention = ifelse(grade == 4 &
                            intervention_school == 1 &
                            low_SES == 1, 1, 0)
  )

## -----------------------------
## 5. Generate scores under the model
## -----------------------------

# Linear growth model with random intercepts and slopes at school and student levels [page:1]
# Y_{ijkt} = (beta_0 + u0_school_j + u0_student_k)
#           + (beta_1 + u1_school_j + u1_student_k) * year_t
#           + beta_SES * SES_k
#           + beta_int_eff * intervention_{ijkt}
#           + eps_{ijkt}

dat <- dat %>%
  mutate(
    mu = (beta_0 + u0_school + u0_student) +
      (beta_1 + u1_school + u1_student) * year +
      beta_SES * SES +
      beta_int_eff * intervention,
    eps = rnorm(n(), mean = 0, sd = sigma_eps),
    score = mu + eps
  )

## -----------------------------
## 6. Fit the corresponding mixed model with nlme
## -----------------------------

# This mirrors the nested multivariate model: random slopes for year at school and student levels [page:1]
fit <- lme(
  fixed = score ~ year + SES + intervention,
  data  = dat,
  random = ~ year | school_id/student_id
)

summary(fit)

## -----------------------------
## 7. Simple checks / plots for students
## -----------------------------

# Quick plot of growth trajectories for a few randomly chosen students
sample_students <- sample(levels(dat$student_id), 12)

ggplot(filter(dat, student_id %in% sample_students),
       aes(x = grade, y = score,
           group = student_id, color = factor(intervention_school))) +
  geom_line(alpha = 0.7) +
  geom_point(aes(shape = factor(intervention))) +
  labs(color = "Intervention school",
       shape = "Got tutoring in grade 4",
       x = "Grade", y = "Test score") +
  theme_minimal()
