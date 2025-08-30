;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.genassem)

(defun json-value (obj key)
  (gethash key obj))

(defun json/true-value? (obj key)
  (eql (json-value obj key) 1))

(defun json/bitfield-value (obj key)
  (let ((bits (json-value obj key)))
    (json-bitfield-to-integer bits)))

(defun json/def-like-value (obj key)
  (let ((entry (json-value obj key)))
    (json/def-like-as-keyword entry)))

(defun json/def-like-as-keyword (entry)
  (let ((value (json-value entry "def")))
    (intern value :keyword)))

(defun pseudo-instruction? (obj)
  (or (json/true-value? obj "isPseudo")
      (json/true-value? obj "isCodeGenOnly")))

(defun asm-string (obj)
  (let ((value (json-value obj "AsmString")))
    (check-type value string)
    (when (plusp (length value))
      value)))

(defun has-encoding? (obj)
  (let ((value (json-value obj "Opcode")))
    (assert (eql 8 (length value)))
    value))

;; https://llvm.org/docs/TableGen/BackEnds.html#json-reference
;; "A bits array is ordered from least-significant bit to most-significant."
(defun json-bitfield-to-integer (bit-vector)
  (let ((result 0))
    (loop :for i :from (1- (length bit-vector)) :downto 0
          :for bit = (aref bit-vector i)
          :do (progn
                (setf result (ash result 1))
                (eswitch (bit)
                  (1 (setf result (logior result 1)))
                  (0 (values)))))
    result))

(defun drop-substitution-syntax (str)
  (assert (eql #\$ (elt str 0)))
  (let ((start 1)
        (end (length str)))
    (when (eql #\{ (elt str 1))
      (assert (eql #\} (elt str (1- end))))
      (incf start)
      (decf end))
    (subseq str start end)))

(defun normalize-instruction (obj)
  (assert (equal "" (json-value obj "TwoOperandAliasConstraint")))
  (let ((result ()))
    (macrolet ((set-field (name value)
                 `(setf (getf result ,name) ,value))
               (get-field (name)
                 `(getf result ,name)))
      (set-field :name (json-value obj "!name"))

      (let* ((form-entry (json-value obj "Form"))
             (form-value (json-value form-entry "def")))
        (set-field :form (intern form-value :keyword))
        (set-field
         :form-category
         (cond
           ((starts-with-subseq "RawFrm" form-value)
            :raw)
           ((equal form-value "AddRegFrm")
            :add-reg)
           ((starts-with-subseq "MRM" form-value)
            :mrm)
           (t
            (error "Unexpected Form value ~S" form-value)))))

      ;;(set-field :form-bits (json/bitfield-value obj "FormBits"))
      (set-field :opcode (json/bitfield-value obj "Opcode"))
      (set-field :has-position-order (json/true-value? obj "HasPositionOrder"))
      (set-field :has-rex.w (json/true-value? obj "hasREX_W"))
      (set-field :immt (json/def-like-value obj "ImmT"))

      (let ((preds (json-value obj "Predicates")))
        (loop :with kwpkg = (find-package :keyword)
              :for pred :across preds
              :do (let ((name (json-value pred "def")))
                    (push (intern name kwpkg)
                          (getf result :predicates)))))

      (let ((value (json/def-like-value obj "OpSize")))
        (assert (member value '(:|OpSize16| :|OpSize32| :|OpSizeFixed|)))
        (set-field :op-size value))

      (let ((value (json/def-like-value obj "OpMap")))
        (assert (member value '(:|OB| ; One Byte
                                :|TA|
                                :|TB|
                                :|T8|
                                :|T_MAP4| :|T_MAP5| :|T_MAP6| :|T_MAP7|
                                :|XOPA|
                                :|XOP8| :|XOP9|
                                :|ThreeDNow|)))
        (set-field :op-map value))

      (let ((value (json/def-like-value obj "OpPrefix")))
        (unless (eq value :|NoPrfx|)
          (set-field :op-prefix value)))

      (let ((value (json/def-like-value obj "explicitOpPrefix")))
        (unless (eq value :|NoExplicitOpPrefix|)
          (assert (member value '(:|PD| :|PS| :|SD| :|ExplicitEVEX|
                                  :|ExplicitREX2| :|ExplicitVEX|)
                          :test 'eq))
          ;; PD: 0x66
          ;; PS: 0xF3
          ;; SD: 0xF2
          (set-field :op-prefix/explicit value)))

;;;
;;; params
;;;

      ;; InOperandList/OutOperandList = visible, explicit operands.
      ;; Uses/Defs = implicit operands (hidden side effects: registers, flags, memory, ports).
      (labels
          ((get-param-list (json-key)
             (let ((operands (json-value obj json-key)))
               (assert (equal "dag" (json-value operands "kind")))
               (json-value operands "args")))
           (normalize-param-list (params)
             (loop :for param :across params
                   :for type = (elt param 0)
                   :for name = (elt param 1)
                   :unless (eq 'null name)
                     :collect (cons (intern/asm name)
                                    (json/def-like-as-keyword type)))))
        (let ((output-params (normalize-param-list (get-param-list "OutOperandList")))
              (input-params  (normalize-param-list (get-param-list "InOperandList"))))
          ;; TODO
          #+nil
          (when (string= (json-value obj "Constraints")
                         "$src = $dst")
            (assert (string= "src" (car (first input-params))))
            (assert (string= "dst" (car (first output-params))))
            (pop output-params))
          (set-field :inputs input-params)
          (set-field :outputs output-params)))

      (let ((asm-string (asm-string obj)))
        (when asm-string
          (set-field :asm-string asm-string)
          (bind (((:values mnemonic params)
                  (aif (position #\Tab asm-string)
                       (values (subseq asm-string 0 it)
                               (subseq asm-string (1+ it)))
                       (values asm-string ""))))
            (awhen (position #\{ mnemonic)
              (let (;;(suffix (subseq mnemonic it))
                    )
                (setf mnemonic (subseq mnemonic 0 it))
                ;; (assert (= 3 (length suffix)))
                ;; (assert (member (elt suffix 1) '(#\b #\w #\l #\q #\s)))
                ))
            (set-field :mnemonic mnemonic)
            (when (starts-with-subseq "{*}" params)
              (setf params (subseq params 3)))
            (awhen (position #\{ params)
              (assert (zerop it))
              (assert (eql #\} (elt params (1- (length params)))))
              (let ((separator (position #\| params)))
                (assert separator)
                ;; here we chose the AT&T syntax
                (setf params (subseq params 1 separator))))
            (let* ((params (mapcar (lambda (el)
                                     (string-trim '(#\Space) el))
                                   (split-sequence:split-sequence
                                    #\, params
                                    :remove-empty-subseqs t)))
                   (inputs  (get-field :inputs))
                   (outputs (get-field :outputs))
                   (all-params (append inputs outputs))
                   (filtered-params
                     (mapcar (lambda (param)
                               (let ((postfix nil)
                                     (broadcast nil)
                                     (sae nil))
                                 (declare (ignorable postfix broadcast sae))
                                 (cond
                                   ((eql #\$ (elt param 0))
                                    (acond
                                      ((and (eql #\{ (elt param 1))
                                            (eql #\} (elt param (1- (length param))))
                                            (position #\} param))
                                       ;; "${src2}{1to8}"
                                       (setf broadcast (subseq param it))
                                       (setf param (subseq param 2 it)))
                                      ((position #\Space param)
                                       ;; "$dst {${mask}}"
                                       ;; "${dst} {${mask}}"
                                       (setf postfix (subseq param it))
                                       (setf param (drop-substitution-syntax
                                                    (subseq param 0 it))))
                                      (t
                                       (setf param (subseq param 1))))
                                    ;; TODO parse and represent postfix and broadcast
                                    (or (assoc (string-upcase param)
                                               all-params :test 'string=)
                                        (cerror "ignore" "failed to look up param ~S" param)))
                                   ((eql #\% (elt param 0))
                                    ;; it's some implicit register, we ignore those
                                    nil)
                                   ((equal param "{sae}")
                                    (setf sae t)
                                    (setf param nil))
                                   (t
                                    (cerror "Ignore" "TODO: Unrecognized pattern, param is: ~S" param)
                                    nil))))
                             params)))
              (set-field :parameters (remove nil filtered-params))))))
      result)))

;; (defparameter *supported-encodings* '("EncNormal"))

;; called on the raw json
(defun json/include-instruction? (obj)
  (and (not (ends-with-subseq "_PREFIX" (json-value obj "!name")))
       (asm-string obj)
       (not (pseudo-instruction?  obj))
       (has-encoding? obj)
       (let* ((tsflags (json-value obj "TSFlags"))
              (vector-flags (subseq tsflags 40)))
         ;; for now skip all the "fancy" instructions
         (every 'zerop vector-flags))))

;; called on the normalized plist form
(defun include-instruction? (obj)
  (declare (ignore obj))
  t)

(defun process-tablegen-json (stream instruction-emitter)
  (jzon:with-parser (parser stream)
    (let ((depth 0)
          top
          stack
          key-stack
          instr-names)
      (flet ((finish-value (value)
               (typecase stack
                 (null                 (setf top value))
                 ((cons list)          (push value (car stack)))
                 ((cons hash-table)    (let ((key (pop key-stack)))
                                         (if (= depth 1)
                                             (values) ; don't collect toplevel entries
                                             (setf (gethash key (car stack)) value)))))))
        (loop
          (multiple-value-bind (event value)
              (jzon:parse-next parser)
            (ecase event
              ((nil)
               (assert (zerop (hash-table-count top)))
               (return instr-names))

              (:value
               (finish-value value))

              (:begin-array
               (push (list) stack))

              (:end-array
               (finish-value (coerce (the list (nreverse (pop stack))) 'simple-vector)))

              (:begin-object
               ;; (when (< depth 2)
               ;;   (format t ":begin-object depth ~A, key stack ~A~%" depth key-stack))
               (incf depth)
               (push (make-hash-table :test 'equal) stack))

              (:end-object
               (decf depth)
               (let ((obj (pop stack))
                     (key (first key-stack)))
                 (when (eql depth 1)
                   ;; (format t ":end-object depth ~A, key stack ~A, obj ~A~%" depth key-stack obj)
                   (cond
                     ((equal "!instanceof" key)
                      (assert (not instr-names))
                      (check-type obj hash-table)
                      (let ((name-vector (gethash "X86Inst" obj)))
                        (setf instr-names (make-hash-table :test 'equal))
                        (loop :for name :across name-vector
                              :do (setf (gethash name instr-names) t))))
                     ((and (gethash key instr-names)
                           (json/include-instruction? obj))
                      (let ((instr (normalize-instruction obj)))
                        (when (include-instruction? instr)
                          (assert (equal "X86" (json-value obj "Namespace")))
                          ;; (format *error-output* "~&; calling emitter for ~S~%" (getf instr :mnemonic))
                          (funcall instruction-emitter instr))))))
                 (finish-value obj)))

              (:object-key
               (push value key-stack)))))))))

;; (defun to-hex-byte-string (val)
;;   (assert (<= val #xff))
;;   (format nil "~x" val))

(defun intern/asm (str)
  (intern (string-upcase str)))

(defun emit-asm-form (form)
  (pprint form))

(defun emit-comment (&rest parts)
  (fresh-line)
  (write-string ";; ")
  (map nil 'princ parts))
