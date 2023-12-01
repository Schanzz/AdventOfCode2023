(defsystem "advent-of-code-2023"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "advent-of-code-2023/tests"))))

(defsystem "advent-of-code-2023/tests"
  :author ""
  :license ""
  :depends-on ("advent-of-code-2023"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for advent-of-code-2023"
  :perform (test-op (op c) (symbol-call :rove :run c)))
