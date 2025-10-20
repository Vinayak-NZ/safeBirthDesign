exp_int <- boxGrob(glue("Recruitment and invitation of study participants",
                        .sep = "\n"))

invited <- boxGrob(glue("Collection of informed consent and app registration",
                        "n = {pop}", 
                        pop = txtInt(c2),
                        .sep = "\n"))

meet_greet <- boxGrob(glue("Meet and greet",
                           "n = {pop}", 
                           "Date = 5th February", 
                           pop = txtInt(11),
                           .sep = "\n"))

workshop_one <- boxGrob(glue("Workshop one",
                             "n = {pop}", 
                             "Date = 6th February", 
                             pop = txtInt(11),
                             .sep = "\n"))

workshop_two <- boxGrob(glue("Workshop two",
                             "n = {pop}", 
                             "Date = 9th February", 
                             pop = txtInt(12),
                             .sep = "\n"))

excluded <- boxGrob(glue("Excluded (n = {tot}):",
                         " - not eligible: {ineligible}",
                         " - not selected based on interview: {noint}",
                         tot = 15,
                         ineligible = 3,
                         noint = 12,
                         .sep = "\n"),
                    just = "left")

png("output/flowchart_part_one.png", 
    width=500, 
    height = 500, 
    units = "px")

grid.newpage()

part_one <- spreadVertical(exp_int = exp_int,
                           invited = invited,
                           meet_greet = meet_greet,
                           workshop_one = workshop_one, 
                           workshop_two = workshop_two)

excluded <- moveBox(excluded,
                    x = .8,
                    y = coords(part_one$invited)$top + 
                      distance(part_one$exp_int, 
                               part_one$invited, 
                               half = TRUE, 
                               center = TRUE))

for (i in 1:(length(part_one) - 1)) {
  connectGrob(part_one[[i]], part_one[[i + 1]], type = "vert") %>%
    print
}

connectGrob(part_one$exp_int, excluded, type = "L")

part_one
excluded

dev.off()

# part two of flochart diagram

workshop_three <- boxGrob(glue("Recruitment and invitation of study participants",
                               .sep = "\n"))

workshop_four <- boxGrob(glue("Collection of informed consent and app registration",
                              "n = {pop}", 
                              pop = txtInt(c2),
                              .sep = "\n"))

workshop_proto <- boxGrob(glue("IG",
                               "Pre-test survey completed", 
                               "n = {pop}", 
                               pop = txtInt(5),
                               .sep = "\n"))

workshop_methods <- boxGrob(glue("CG",
                                 "Pre-test survey completed", 
                                 "n = {pop}", 
                                 pop = txtInt(6),
                                 .sep = "\n"))

test <- boxGrob(glue("App baseline participation",
                     "n = {pop}", 
                     pop = txtInt(6),
                     .sep = "\n"))

test2 <- boxGrob(glue("App baseline participation",
                     "n = {pop}", 
                     pop = txtInt(6),
                     .sep = "\n"))

png("output/flowchart_part_two.png", 
    width=500, 
    height = 500, 
    units = "px")

grid.newpage()

part_two <- spreadVertical(workshop_three = workshop_three,
                           workshop_four = workshop_four, 
                           grps = workshop_proto)

grps <- alignVertical(reference = part_two$grps,
                      workshop_proto, workshop_methods) %>%
  spreadHorizontal()

part_two$grps <- NULL

# part_three <- spreadVertical(test_pre = grps[[1]], 
#                              test = test, 
#                              test2 = test2)

for (i in 1:(length(part_two) - 1)) {
  connectGrob(part_two[[i]], part_two[[i + 1]], type = "vert") %>%
    print
}

connectGrob(part_two$workshop_four, grps[[1]], type = "N")
connectGrob(part_two$workshop_four, grps[[2]], type = "N")

# connectGrob(part_three$test_pre, part_three$test,  type = "N")
# connectGrob(part_three$test, part_three$test2, type = "N")

n_ig_access <- 999   # e.g., number who got app access
n_ig_post   <- 888   # e.g., IG post-test completes
n_cg_wait   <- 4     # e.g., wait period (weeks) — label only
n_cg_post   <- 777   # e.g., CG post-test (pre-app) completes

ig_mid  <- boxGrob(glue("IG\nApp access granted\nn = {txtInt(n_ig_access)}"))
ig_post <- boxGrob(glue("IG\nPost-test survey completed\nn = {txtInt(n_ig_post)}"))

cg_wait <- boxGrob(glue("CG\nWaitlist period ({n_cg_wait} weeks)"))
cg_post <- boxGrob(glue("CG\nPost-test survey completed\n(pre-app)\nn = {txtInt(n_cg_post)}"))

# Stack vertically under each branch root
ig_stack <- spreadVertical(ig_pre = grps[[1]],
                           ig_mid = ig_mid,
                           ig_post = ig_post)

cg_stack <- spreadVertical(cg_pre = grps[[2]],
                           cg_wait = cg_wait,
                           cg_post = cg_post)

# Connect down the IG branch
connectGrob(ig_stack$ig_pre, ig_stack$ig_mid,  type = "vert") %>% print
connectGrob(ig_stack$ig_mid, ig_stack$ig_post, type = "vert") %>% print

# Connect down the CG branch
connectGrob(cg_stack$cg_pre, cg_stack$cg_wait, type = "vert") %>% print
connectGrob(cg_stack$cg_wait, cg_stack$cg_post, type = "vert") %>% print

part_two
grps
# part_three

dev.off()


