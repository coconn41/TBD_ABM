#####
# Model parameters:
#####

# Taken from Halsey and Miller 2018
LA_probability = 0.103726853
NA_probability = 0.16788807
AA_probability = 0.011786875
L_feed_time = 24*3
N_feed_time = 24*5
A_feed_time = 24*10
L_unfed_DR_lt40 = 0.000213472
N_unfed_DR_lt40 = 0.000003159
A_unfed_DR_lt40 = 0.000000183
L_unfed_DR_o40 = 0.000517143
N_unfed_DR_o40 = 0.000363855
A_unfed_DR_o40 = 0.000360880
L_rep_DR = 0.0001317776
N_rep_DR = 0.0000958566
A_rep_DR = 0.0000912404
L_molt_success = 0.415
N_molt_success = 0.276
mouse_GR = 0.00734 
deer_GR = 0.00495
Groom_survival = 0.2
egg_mort_rate = 0.00003017683

#####
# Molting timing in days w/ explanation
#####
lay_egg = 80+31 # March 20th + April 20th (31 days)
egg_to_larvae = 171+31 # June 20th + July 20th
larvae_to_nymph_min = 80+31 # March 20th + April 20th (31 days)
larvae_to_nymph_max = 171 # June 20th
nymph_to_adult_min = 258 # September 15th
nymph_to_adult_max = 258 + 30 # September 15th + 30 day = October 15th

#####
# Parameters from other sources
#####
mouse_infect_tick_ha = 0.0665 # Keesing ASTMH 2014
#mouse_infect_tick_v1 = 0.0094 # Keesing ASTMH 2014
tick_infect_mouse_ha = 1 # 0.0665 # Need a citation for this?
#tick_infect_mouse_v1 = 1 # 0.0094 # Need a citation for this?
deer_infect_tick_v1 = 0.0094 # Massung EID 2005
#deer_infect_tick_ha = 0.0665 # Massung EID 2005
tick_infect_deer_v1 = 1 # 0.0094 # Need a citation for this?
#tick_infect_deer_ha = 1 # 0.0665 # Need a citation for this?

 # Reichard Vector-Borne and Zoonotic Diseases 2009, WTD became PCR negative after 28 days
 # Hodzic 1998 lab mice can remain infected for max 55 days

#####
# Need new source for below:
#####

eggs_per_female = 1000 # Sonenshine 1991, but still apriori
deer_attach_prob = .1 # A priori
mouse_attach_prob = .1  # A priori

# A priori:
# Larval drop off within 12 hours
# Probability of 1 within 12 hours: 1/12
# function is rbinom(n = num_ticks,size = 1,prob = 1/12)






