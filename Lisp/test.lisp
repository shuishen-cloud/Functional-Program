; this is a test lisp

(defun my_test_func (x)
    (+ 1 x))

(defun cs ()
    "跨平台 sbcl 清除屏幕"
    (let ((cmd (cond ((uiop:os-windows-p) "cls")
                (t "clear"))))
            (uiop:run-program cmd :output t)))

; 列表是否相等
(eql (cons 'A nil) (cons 'A nil))   ;NIL

(setf x '(A nil))
(eql x x)   ;True

; 我比较好奇的是，书里那么标准的缩进是怎么搞的——加一个插件？
(defun compress (x)
    "简单形式的列表压缩，游程编码"
    (if (consp x)
        (compr (car x) 1 (cdr x))
        x))

(defun compr (elt n lst)
    (if (null lst)
        (list (n-elts elt n))
        (let ((next (car lst)))
            (if (eql next elt)
                (compr elt (+ n 1) (cdr lst))
                (cons (n-elts elt n)
                        (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
    (if (> n 1)
        (list n elt)
        elt))

(defun our-subst (new old tree)
    "较为重要的树操作。"
    (if (eql tree old)
        new
        (if (atom tree)
            tree
            (cons (our-subst new old (car tree))
                    (our-subst new old (cdr tree))))))

; > (our-subst 'X 'Y '(X Y (X Y) X))

; 这个的思路是什么呢？使用数据结构？我的问题在于没有和过去的知识联系起来
(defun occurrences (lst)
    "接受一个列表并返回一个列表，指出相等元素出现的次数，并由最常见至最少见的排序"
    (let ((counts (make-hash-table :test #'equal)))
        ; 统计每个元素的出现次数
        (dolist (elem lst)
            (incf (gethash elem counts 0)))
        ; 将哈希表转换为列表，并按次数降序排序
        (sort 
            (loop for key being the hash-keys of counts
                for val being the hash-values of counts
                collect (cons key val))
        #'> :key #'cdr)))


; 感觉算法转化上不是很熟悉；首先熟悉一下算法的思想，再慢慢转化
; 给 str，还有条件，切割，这个的核心是定位，阅读算法的核心在于从头开始

(defun tokens (str test start)
    (let ((p1 (position-if test str :start start))) ; 满足获得字串的
        (if 
            (let ((p2 (position-if #' (lambda (c)
                                        (not (funcall test c)))
                                        str :start p1)))
                (cons (subseq str p1 p2)    ; 积累符合的子串
                    (if p2
                        (tokens str test p2)
                        nil)))
            nil)))

(defun constituent (c)
    "检测是否为可见字符（例子，空格不是可见字符）"
    (and (graphic-char-p c)
        (not (char= c #\))))    ; 这一行是什么意思？

