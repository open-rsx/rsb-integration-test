;; ============================================================
;;
;; This file is part of the RSB project
;;
;; Copyright (C) 2011 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This program is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation;
;; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; ============================================================

(ql:quickload :uuid)

(with-open-file (stream "./event-id-cases.txt"
			:direction :output)
  (loop :repeat 1024 :do
     (let* ((origin (uuid:make-v4-uuid))
	    (seqnum (random (ash 1 32)))
	    (id     (uuid:make-v5-uuid
		     origin (format nil "~(~8,'0X~)" seqnum))))
       (format stream "~(~A ~8,'0X ~A~)~%" origin seqnum id))))
