(in-package :cl-user)
(defpackage scriba-test
  (:use :cl :fiveam))
(in-package :scriba-test)

(run! 'scriba-test.parser:parser)
(run! 'scriba-test.plump:plump)
(run! 'scriba-test.emitter:emitter)
