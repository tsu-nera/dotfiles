require 'rake/clean'
CLEAN.include(["//*.~$", "//*.el\#$" ])
CLOBBER.include(["inits/*.elc"])
