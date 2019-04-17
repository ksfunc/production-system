(setq *sample-props* '((A "ソクラテスは人間である")
                       (B "人間はみな死ぬ")
                       (C "死んだものは天国に行く")
                       (D "ソクラテスは死ぬ")
                       (E "ソクラテスは天国に行く")))

(setq *sample-rules* '(((A B) D)
                       ((C D) E)))

(defun foldr (%f %v %xs)
  (if (null %xs) %v
    (funcall %f (car %xs) (foldr %f %v (cdr %xs)))))

(defun is-subset (%sub %super)
  (cond ((null %sub) t)
        ((member (car %sub) %super :test #'eq) (is-subset (cdr %sub) %super))
        (t nil)))

(defun wm-to-props-set (%wm)
  (mapcar #'(lambda (%x) (car %x)) %wm))

(defun wm-to-hist (%wm)
  (mapcar #'(lambda (%x) (cadr %x)) %wm))

;; 命題の集合 %props-set から導出を行えるかかどうかでルール %rules にフィルタをかける
(defun extract-applicable-rules (%props-set %rules)
  (cond ((null %rules) nil)
        ((is-subset (caar %rules) %props-set) (cons (car %rules) (extract-applicable-rules %props-set (cdr %rules))))
        (t (extract-applicable-rules %props-set (cdr %rules)))))

;; ルールの集合 %hist に含まれているかどうかでルール %rules にフィルタをかける
(defun exclude-in-hist (%hist %rules)
  (cond ((null %rules) nil)
        ((member (caar %rules) %hist :test #'equal) (exclude-in-hist %hist (cdr %rules)))
        (t (cons (car %rules) (exclude-in-hist %hist (cdr %rules))))))

;; ルールを一つ適用する
(defun apply-rule (%rule %wm)
  (cons (list (cadr %rule) (car %rule)) %wm))

;; 推論を行う
(defun infer (%wm %rules)
  (let* ((=applicable-rules (extract-applicable-rules (wm-to-props-set %wm) %rules))
         (=hist (wm-to-hist %wm))
         (=rules (exclude-in-hist =hist =applicable-rules)))
    (if (null =rules) %wm
      (infer (foldr #'apply-rule %wm =rules) %rules))))

(defun props-set-to-wm (%props-set)
  (mapcar #'(lambda (%prop) (list %prop nil)) %props-set))

(defun print-props (%props)
  (cond ((null %props) nil)
        (t (format t "~s: ~a~%" (caar %props) (cadar %props))
           (print-props (cdr %props)))))

;; 初期条件を選択するインタフェース
(defun select-initial-props (%props)
  (format t "Please choose some initial propositions from the following and input a list of symbols.~%")
  (print-props %props)
  (let ((=input (read)))
    (if (and (listp =input)
             (not (null =input))
             (every #'(lambda (%prop) (assoc %prop %props)) =input))
        =input
      (select-initial-props %props))))

;; 推論結果を選択するインタフェース
(defun select-result-prop (%props)
  (format t "Please choose a result proposition from the following and input a symbol.~%")
  (print-props %props)
  (let ((=input (read)))
    (if (assoc =input %props) =input (select-result-prop %props))))

(defun get-prop-str (%prop %props)
  (cadr (assoc %prop %props)))

(defun print-trace (%wm %prop %props)
  (let ((=result (assoc %prop %wm)))
    (if (null (cadr =result))
        (format t "~s is given.~%" (get-prop-str (car =result) %props))
      (progn
        (format t "~s is derived from" (get-prop-str (car =result) %props))
        (format t " ~s" (get-prop-str (car (cadr =result)) %props))
        (mapcar #'(lambda (%res-prop) (format t " and ~s" (get-prop-str %res-prop %props))) (cdr (cadr =result)))
        (format t ".~%")
        (mapcar #'(lambda (%res-props) (print-trace %wm %res-props %props)) (cadr =result))))))

;; 初期条件を選択するインタフェースおよび推論結果を選択するインタフェースより条件を取得し，推論を行うインタフェース
;; 使用する命題およびルールは引数に与える
(defun start-infer (%props %rules)
  (let* ((=wm (infer (props-set-to-wm (select-initial-props %props)) %rules))
         (=question (select-result-prop %props)))
    (print-trace =wm =question %props)))

(start-infer *sample-props* *sample-rules*)
;; Please choose some initial propositions from the following and input a list of symbols.
;; A: ソクラテスは人間である
;; B: 人間はみな死ぬ
;; C: 死んだものは天国に行く
;; D: ソクラテスは死ぬ
;; E: ソクラテスは天国に行く
;; (A B C)
;; Please choose a result proposition from the following and input a symbol.
;; A: ソクラテスは人間である
;; B: 人間はみな死ぬ
;; C: 死んだものは天国に行く
;; D: ソクラテスは死ぬ
;; E: ソクラテスは天国に行く
;; E
;; "ソクラテスは天国に行く" is derived from "死んだものは天国に行く" and "ソクラテスは死ぬ".
;; "死んだものは天国に行く" is given.
;; "ソクラテスは死ぬ" is derived from "ソクラテスは人間である" and "人間はみな死ぬ".
;; "ソクラテスは人間である" is given.
;; "人間はみな死ぬ" is given.
