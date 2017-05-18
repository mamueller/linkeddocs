The purpose of this prototype is to find out which problems may arise if the documented package
exports a function with a name also exported by the documenting package.
This actually works without problem due to the namespace environment of the documenting package [ http://adv-r.had.co.nz/Environments.html#function-envs]
To see what is going on run the script and lood at the main function of the documentign package
