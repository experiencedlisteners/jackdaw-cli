(with-open-file (*standard-output* "/dev/null" :direction :output
					       :if-exists :supersede)
  (ql:quickload :jackdaw-cli))
;;  (asdf:load-system :jackdaw-cli))
(jackdaw-cli::run-global)
