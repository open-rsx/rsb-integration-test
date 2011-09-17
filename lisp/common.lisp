(load-system :sb-posix)
(load-system :com.dvlsoft.clon)
(load-system :cl-protobuf)
(load-system :cl-spread)
(load-system :cl-rsb)
(map nil #'unintern '(for finally collect else with in)) ;; iterate bug

(use-package :split-sequence)
(use-package :alexandria)
(use-package :bind)
(use-package :iter)
(use-package :com.dvlsoft.clon)

(let ((descriptor (pbf:load/text #P"../../data/Image.proto")))
  (pbb:emit descriptor :class)
  (pbb:emit descriptor :packed-size)
  (pbb:emit descriptor :serializer)
  (pbb:emit descriptor :deserializer))

(defvar *client/server-test-uri* "spread:/rsbtest/clientserver")

(deftype cookie-type ()
  'non-negative-integer)

(defvar *cookie* nil
  "Contains the magic number we expect to receive from the
client. This is intended to protect against crosstalk between test
cases.")
