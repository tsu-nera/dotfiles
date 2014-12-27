require 'rake/clean'
CLEAN.include(["**/*~$", "**/#*"])
CLOBBER.include(["inits/*.el","**/*.elc"])
