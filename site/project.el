(require 'org-publish)

(setq org-publish-project-alist
      '(("gitto-files"
         :base-directory "./"
         :publishing-directory "_publish/"
         :recursive nil
         :base-extension "css"
         :publishing-function org-publish-attachment)
        ("gitto-org"
         :base-directory "./"
         :publishing-directory "_publish"
         :recursive nil
         :base-extension "org"
         :publishing-function org-publish-org-to-html)
        ("gitto-site"
         :components ("gitto-org" "gitto-files"))))
