;; 类型检查
(defun type-check (val type)
  (unless (typep val type)
    (error "类型错误 ~S 期望 ~S" val type))
  val)

;; 日志打印
(defun log-call (name args)
  (format t "→ 调用 ~A ~S~%" name args))

(defmacro defn (name args-list &rest rest)
  (let* ((ann (when (and rest (symbolp (car rest)))
                (car rest)))
         (body (if ann (cdr rest) rest))
         (logp (eq ann '@log))

         (type-checks
          (loop for arg in args-list
                when (and (consp arg)
                          (>= (length arg) 3)
                          (eq (cadr arg) '@type))
                collect `(type-check ,(car arg) ',(caddr arg))))

         (arg-names
          (mapcar (lambda (arg)
                    (if (symbolp arg)
                        arg
                        (car arg)))
                  args-list)))

    `(defun ,name ,arg-names
       ,@(when logp
           `((log-call ',name (list ,@arg-names))))
       ,@type-checks
       ,@body)))

(defn add ((a @type integer) (b @type integer)) @log
  (+ a b))
