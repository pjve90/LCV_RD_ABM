# Life cycle variation and resource dynamics ABM: Code ----

## Resource production ----

#Habitat quality
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/habitat_quality.R?token=GHSAT0AAAAAAB5C6IJP4LPU5FW5TFDKNVNUY77EEBQ")

#Production
#stage-specific probabilities of production
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/production_prob.R?token=GHSAT0AAAAAAB5C6IJOW6AVVENY37LYDMQAY77EEGQ")
#production function
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/production_fx.R?token=GHSAT0AAAAAAB5C6IJOSYCPF5WEIB7RJOZUY77EEMA")
#production amount
source("https://raw.githubusercontent.com/pjve90/LCV_RD_ABM/main/Model%20code/production_amount.R?token=GHSAT0AAAAAAB5C6IJPPWOGQQYX7E5W6JK4Y77EEQQ")

## Maternal investment ----

#Identify if the mother has surplus of resources
source()

#Identify the amount of surplus
source()

#Identify if the descendants need resources
source()

#Identify the amount of need for each descendant
source()

#Order the descendants by need and mother id
source()

#Mother invest in her descendants
source()

## Resource transfers ----

# Specify the number of nodes and the number of blocks
source()

#Define the block matrix
source()

#Define the surplus for transfers (max out degree in the network)
source()

#Generate the network
source()

#Record the resources transferred
source() 

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

#Add newborns
source()

## Transition ----

#Transition
source()

## Survival ----

#Survival
source()

