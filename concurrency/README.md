# WARNING:

This verison of the project is functional and more efficeint, however it has no current
solution to an error where the final sums of sent and received messages does not add up
to 100 despite the message list being of lenth 100. the current solution can be found at:
https://github.com/Dranelmek/Haskell-Concurrency under the cmctest or final branch.
That solution counts messages sent and received centrally after the simulation ends rather
than locally in each branch during the simulation. This adds an additional n^2 of timecomplexity
to the programs execution but eliminates the afromentioned error. 
I the author will cease developement on this branch for the time being and might revisit it later
if you are reading this and want to help optimise (for free) why not help me on a more relevant 
project instead you can find me at: https://twitter.com/drane_lmek