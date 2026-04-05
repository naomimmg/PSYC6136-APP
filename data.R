##### Import Data #####
# 1) Employment Status (table)
Employment <- vcd::Employment

# 2) Infection in Caesarean Births (table)
Caesar <- vcdExtra::Caesar

# 3) Titanic Data (df)
Titanic_df <- vcdExtra::Titanicp |>
    mutate(age = case_when(
        age >= 15 ~ "Adult",
        age < 15 ~ "Child",
        TRUE ~ NA
    ))
Titanic <- Titanic_df |>
    count(sex, age, survived, pclass) |>
    xtabs(formula = n ~ sex + age + survived + pclass)

# structable(pclass ~ sex + age + survived, 
#            data = Titanic)

# 4) Suicide Data
Suicide <- vcd::Suicide |>
    xtabs(formula = Freq ~ sex + method + age.group)

# 5) Berkeley Admission Data (table)
UCBAdmissions <- datasets::UCBAdmissions

# 6) Divorce Data
Divorce <- vcd::PreSex

# 7) Abortion Opinion Data
Abortion <- vcdExtra::Abortion

# 8) HairEyeSex Data
HairEyeSex_df <- datasets::HairEyeColor |>
    as.data.frame()
HairEyeSex <- xtabs(Freq ~ Eye + Hair + Sex, data = HairEyeSex_df)
# structable(HairEyeSex, Sex + Eye ~ Hair)

# 9) HairEye Data
HairEye <- datasets::HairEyeColor

# 10) Heart Disease Data
Heart <- vcdExtra::Heart

# Save data into list
data_sets <- list(Employment, Caesar, Titanic, Suicide, 
                  UCBAdmissions, Divorce, Abortion, HairEyeSex, 
                  Heart, HairEye)

names(data_sets) <- c("Employment", "Caesar", "Titanic", "Suicide", 
                      "UCBAdmissions", "Divorce", "Abortion", "HairEyeSex", 
                      "Heart", "HairEye")