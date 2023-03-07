# Life cycle variation and resource dynamics ABM: Code ----

## Resource production ----

#Habitat quality
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/habitat_quality.R?token=GHSAT0AAAAAAB5C6IJPFS4DAW6ELJXQMDPIZAHJ5HQ")

#Stage-specific probabilities of production
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/production_prob.R?token=GHSAT0AAAAAAB5C6IJOR7WHWDLGLPYCEOBUZAHJ5ZA")

#Production function
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/production_fx.R?token=GHSAT0AAAAAAB5C6IJPTTBDHAP66WSMVNGQZAHJ5TA")

#Production amount
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/production_amount.R?token=GHSAT0AAAAAAB5C6IJPHSD6FZ5T4LUBK5XSZAHJ5NA")

## Maternal investment ----

#Identify if the mother has surplus of resources
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/mat_invest_mom_surp_identify.R?token=GHSAT0AAAAAAB5C6IJPMFWT5XPBUGSQMVM4ZAHJ7TA")

#Identify the amount of surplus of the mother
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/mat_invest_mom_surp_amount.R?token=GHSAT0AAAAAAB5C6IJPZIEAHCWDQ74N4HWKZAHJ7JQ")

#Identify if the descendants need resources
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/mat_invest_desc_need_identify.R?token=GHSAT0AAAAAAB5C6IJPTCAB7PBQ7YGUZ6JOZAHKACQ")

#Identify the amount of need for each descendant
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/mat_invest_desc_need_amount.R?token=GHSAT0AAAAAAB5C6IJPAD5AHTTHV544RUEKZAHKAUA")

#Order the descendants by need and mother id
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/mat_invest_desc_order.R?token=GHSAT0AAAAAAB5C6IJOKHUDEMRGIGXPRBTKZAHJ66Q")

#Mother invest in her descendants
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/mat_invest_fx.R?token=GHSAT0AAAAAAB5C6IJOKGTVL4OSATFFDYZUZAHJ7DA")

## Resource transfers ----

# Specify the number of nodes and the number of blocks
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/transfers_param_sbm.R?token=GHSAT0AAAAAAB5C6IJOKC6F6S4DXMX62HKAZAHKBTA")

#Define the block matrix
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/transfers_block_matrix.R?token=GHSAT0AAAAAAB5C6IJOYG3WSWIMRAKEEVWEZAHKCCA")

#Define the surplus for transfers (max out degree in the network)
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/transfers_surplus_sbm.R?token=GHSAT0AAAAAAB5C6IJO3JYAV2AWHRAAUBMMZAHKDFQ")

#Generate the network
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/transfers_sbm_fx.R?token=GHSAT0AAAAAAB5C6IJOPZBDSJDSP5OIFA4QZAHKDPQ")

#Record the resources transferred
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/transfers_amount.R?token=GHSAT0AAAAAAB5C6IJP6VFYIFHPEAQM45F4ZAHKD4A") 

## Reproduction ----

#Number of descendants per reproduction
source()

#Reproductive threshold
source()

#Reproductive cost
source()

#Reproduction
source()

#Discount of reproductive cost
source()

#Lifetime reproductive output
source()

#Add newborns...not finished
source()

## Transition ----

#Time since last birth
source()

#Transition
source()

## Survival ----

#Survival cost
source()

#Survival
source()

#Discount of survival cost
source()

#Age
source()
