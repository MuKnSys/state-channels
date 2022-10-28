Examples (.ss files)

guile --no-auto-compile --debug -l src/llruntime.ss examples/mp1_1.ss
guile --no-auto-compile --debug -l src/llruntime.ss examples/mp2_1.ss


Examples (.scsh files) [TODO: fix guile call, needs the -l src/llruntime.ss ; use scm ?]

guile --debug src/clish.ss mp1 mp1_1
guile --debug src/clish.ss mp1 mp1_2
guile --debug src/clish.ss mp2 mp2_1


Demo (at the moment):

guile --debug src/clish.ss mp0 mp0_1
guile --debug src/clish.ss mp1 mp1_3
guile --debug src/clish.ss mp2 mp2_3

guile --debug src/clish.ss mp0 mp0_1_0
guile --debug src/clish.ss mp1 mp1_3_0
guile --debug src/clish.ss mp2 mp2_3_0
