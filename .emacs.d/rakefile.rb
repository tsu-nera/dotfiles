require 'rake/clean'
CLEAN.include(["**/*~$", "**/#*"])
CLOBBER.include(["inits/*.elc","inits/*.el"])
