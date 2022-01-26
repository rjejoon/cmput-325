(load "../unittest.lisp")
(load "1619980.lisp")

(deftest test-all ()
  (combine-results
   (test-xcount)
   (test-flatten)
   (test-remove-duplicate)
   (test-mix)
   (test-split)
   (test-split-mix)
   (test-allsubsets)
   (test-reached)
   (test-rank)))


(deftest test-xcount ()
  (check
   (= (xcount '(a (a b) ((c) a))))
   (= (xcount nil) 0)
   (= (xcount '(nil a b ())) 2)))

(deftest test-flatten ()
  (check
   (equal (flatten '(a (b c) d)) '(a b c d))
   (equal (flatten '((((a))))) '(a))
   (equal (flatten '(a (b c) (d ((e)) f))) '(a b c d e f))))

(deftest test-remove-duplicate ()
  (check
   (equal (remove-duplicate '(a b c a d b)) '(a b c d))))

(deftest test-mix ()
  (check
   (equal (mix '(a b c) '(d e f)) '(a d b e c f))
   (equal (mix '(1 2 3) '(a)) '(1 a 2 3))
   (equal (mix '((a) (b c)) '(d e f g h)) '((a) d (b c) e f g h))
   (equal (mix '(1 2 3) nil) '(1 2 3))
   (equal (mix '(1 2 3) '(nil)) '(1 nil 2 3))))

(deftest test-split ()
  (check
   (equal (split '(1 2 3 4 5 6)) '((1 3 5) (2 4 6)))
   (equal (split '((a) (b c) (d e f) g h)) '(((a) (d e f) h) ((b c) g)))
   (equal (split '()) '(nil nil))))

(deftest test-split-mix ()
  (check
   (equal (split (mix '(a b c) '(d e f))) '((a b c) (d e f)))
   (equal (split (mix '(1 2 3) '(4 5))) '((1 2 3) (4 5)))
   (equal (let ((L '(a d b e c f))) (mix (car (split L)) (cadr (split L)))) '(a d b e c f))
   (equal (let ((L nil)) (mix (car (split L)) (cadr (split L)))) nil)))

(deftest test-allsubsets ()
  (check
   (equal (allsubsets nil) '(nil))
   (equal (allsubsets '(a)) '(nil (a)))
   (equal (sort (flatten (allsubsets '(a b))) #'string-lessp)
          (sort (flatten '(nil (a) (b) (a b))) #'string-lessp))))

(deftest test-reached ()
  (check
   (equal (reached 'a '((b c) (b c))) nil)
   (equal (reached nil nil) nil)
   (equal (sort (reached 'google '((google shopify) (google aircanada) (amazon aircanada)))
                #'string-lessp)
          (sort (list 'SHOPIFY 'AIRCANADA) #'string-lessp))
   (equal (sort (reached 'google '((google shopify) (shopify amazon) (amazon google)))
                #'string-lessp)
          (sort (list 'SHOPIFY 'AMAZON) #'string-lessp))
   (equal (sort (reached 'google '((shopify amazon) (google shopify) (amazon google)))
                #'string-lessp)
          (sort (list 'SHOPIFY 'AMAZON) #'string-lessp))
   (equal (sort (reached 'google '((google shopify) (shopify amazon) (amazon indigo)))
                #'string-lessp)
          (sort (list 'SHOPIFY 'AMAZON 'INDIGO) #'string-lessp))
   (equal (sort (reached 'google '((google shopify) (google shopify) (shopify amazon) (amazon indigo) (shopify amazon)))
                #'string-lessp)
          (sort (list 'SHOPIFY 'AMAZON 'INDIGO) #'string-lessp))
   (equal (sort (reached 'google '((google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (google google)))
                #'string-lessp)
          (sort (list 'SHOPIFY 'AIRCANADA 'DELTA) #'string-lessp))))


(deftest test-rank ()
  (check
   (equal (rank '(google shopify aircanada amazon) '((google shopify) (google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (google google)))
          '(AIRCANADA SHOPIFY GOOGLE AMAZON))
   (equal (rank '(google shopify amazon) '((google shopify) (shopify amazon) (amazon google)))
          '(GOOGLE SHOPIFY AMAZON))
   (equal (rank '(google shopify amazon indigo) '((google shopify) (shopify amazon) (amazon indigo)))
          '(SHOPIFY AMAZON INDIGO GOOGLE))
   (equal (rank '(google shopify aircanada amazon delta) '((google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (google google)))
          '(AIRCANADA SHOPIFY DELTA GOOGLE AMAZON))
   (equal (rank '(a b c d) '((a b) (b c) (b c) (b c) (c d) (c d) (a b) (a c) (b d) (d c)))
          '(c d b a))
   (equal (rank '(a b) '((a b) (a a) (a a) (a a) (a a) (b b) (b b) (b b)))
          '(b a))
   (equal (rank nil nil) nil)))





(print (test-all))
