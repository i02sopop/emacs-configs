(setq package-install-upgrade-built-in t)

(use-package gptel
  :ensure t
  :config
  (gptel-make-gh-copilot "Copilot")
  )

(use-package mcp
  :ensure t
  :after gptel
  :custom (mcp-hub-servers
		   `(("filesystem" . (:command "npx"
   									   :args ("-y" "@modelcontextprotocol/server-filesystem")
   									   :roots ("/home/ritho/Documents/mcp/")))
			 ("atlassian" . (:url "http://localhost:9000/mcp/"))))
  :config
  (require 'mcp-hub)
  (setq mcp-default-directory "~/Documents/mcp/")
  (setq mcp-file-extension "md")
  (setq mcp-auto-save t)
  (setq mcp-enable-sync t)
  ;; :hook (after-init . mcp-hub-start-all-servers)
  )
