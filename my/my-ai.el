(use-package gptel
  :commands gptel)

(defun configure-gptel ()
  "Configure GPTel for Ollama integration."
  (setq gptel-model "llama3.1:latest")
  (setq gptel-backend
        (gptel-make-ollama "Ollama"
                           :host "pn50.home:11434"
                           :stream t
                           :models '("deepseek-coder-v2:latest"
				     "starcoder2:15b"
				     "llama3.1:latest"))))

(add-hook 'emacs-startup-hook #'configure-gptel)

(provide 'my-ai)
