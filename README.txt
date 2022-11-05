1) Initial build

For the thing to run after you cloned the repository, do:
backends/build guile

Or alternatively:
backends/build gerbil

Additionally, the bin/ directory must exist, before you run the build.


2) Examples (.ss files)

guile --no-auto-compile --debug -l src/llruntime.ss examples/mp1_1.ss
bin/scm examples/mp1_1.ss
bin/scm examples/mp2_1.ss


Examples (.scsh files) [TODO: fix guile call, needs the -l src/llruntime.ss ; use scm ?]

src/clish.ss mp1 mp1_1 [Broken]
src/clish.ss mp1 mp1_2 [Broken]
src/clish.ss mp2 mp2_1 [Broken]


Demo (at the moment):

guile --no-auto-compile --debug -l src/llruntime.ss src/clish.ss mp0 mp0_1
src/clish.ss mp0 mp0_1
src/clish.ss mp1 mp1_3
src/clish.ss mp2 mp2_3

src/clish.ss mp0 mp0_1_0
src/clish.ss mp1 mp1_3_0
src/clish.ss mp2 mp2_3_0
