require 'rake/clean'
CLEAN.include(["**/*~$", "**/#*"])
CLOBBER.include(["inits/*.el","**/*.elc"])

desc "reboot emacs"
task :reboot => :clobber do
  system("emacsclient -e \"(kill-emacs)\";emacs --daemon")
end

desc "reboot emacs twice"
task :reboot2 => :reboot do
  system("emacsclient -e \"(kill-emacs)\";emacs --daemon")
end

