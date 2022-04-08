## Fisher Kim, Kai Ying, Lucas Schroyer, Peter YH Kim
# Recode referral names script for call in main script


# Recode Berkeley email addresses as MIDS/Berkeley
non_mids_berkeley_emails <- c("dana.miller@berkeley.edu", "pharvey@berkeley.edu",
                              "patrick.lerchi@berkeley.edu", "valkarp@berkeley.edu",
                              "laurenhanlon@berkeley.edu", "sofia.hamilton@berkeley.edu")

d[, referral_person := ifelse(email_1 %like% "berkeley.edu" & 
                                !(email_1 %in% non_mids_berkeley_emails),
                              "Other UC Berkeley/School of Information connection",
                              referral_person)]

# Manually edit data that was incomplete in survey
d[, referral_person := ifelse(email_1 == "ewitt1993@gmail.com",
                              "Lucas or someone affiliated with Lucas",
                              referral_person)]

# Filter out bad entries that we know were not real submits.
d <- d[response_id != "R_237Pino5qLkCwPc",]
