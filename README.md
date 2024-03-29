# concerns
The concerns project investigates parents' concerns for their children's multilingual development in parents of young multilinguals in Quebec. It is part of a larger project that also investigates attitudes towards childhood multilingualism, engagement with resources, and language practices in these same parents, which is described in detail at: https://osf.io/ydqvb/

In the data/input_files, there are two anonymized source files, which two processes have already been applied to: emails have already been removed and dobs have been cleaned. The scripts should be run in this order: 

1. data_clean_concerns : This script applies the exclusion criteria of the larger project (steps 1-5) and the concerns project specifically (steps 6-7)
2. descriptives_concerns : This script generates three files which are written to the concerns/descriptive_stats directory, which summarize the characteristics of participants (e.g. languages transmitting and L1 of parents). 
3. analyses_concerns : This script includes all analyses described in our pre-registration document: https://osf.io/6zuv9/ 

