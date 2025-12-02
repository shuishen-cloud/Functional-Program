;;(ql:quickload :sb-thread)  ; 如果需要加载

;; 创建线程
(defparameter *thread* 
  (sb-thread:make-thread 
    (lambda () 
      (loop for i from 1 to 5
            do (progn 
                 (sleep 1)
                 (format t "线程输出: ~D~%" i))))
    :name "worker-thread"))

;; 主线程继续执行
(format t "主线程继续...~%")

;; 等待线程结束（可选）
(sb-thread:join-thread *thread*)
