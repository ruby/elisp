;; submode for ruby scripts embedded in GitHub Actions YAML files.

(require 'ruby-mode)
(require 'mmm-vars)
(require 'mmm-region)

(mmm-add-classes
 '((ruby-gha :submode ruby-mode :front "^ *#!.*ruby" :back "^ *shell: ruby")))
