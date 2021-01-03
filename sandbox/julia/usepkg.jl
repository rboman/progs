using Pkg
Pkg.activate("TestPackage1")

#%%
using TestPackage1

TestPackage1.greet()
