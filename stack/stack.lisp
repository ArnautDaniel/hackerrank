;;; J.Lucas 2022

(declaim (optimize (speed 3) (debug 0) (safety 0)))

;;;This will only work before the integer limit is hit.  Which on sbcl might as well be infinte.

;;;Read our max number of operations
(defparameter countsd (parse-integer (read-line)))

;;;Create a hash to store them in based on number of max operations
;;;Set test to eq instead of eql or equal
(defparameter *my-hash* (make-hash-table :size countsd :rehash-size 1.5 :rehash-threshold .9  :test 'eq))

;;;Our stack starts at 0
(defparameter small 0)

;;;Push the value l onto the stack
;;;with key n where n is the current op step
;;;Check the size of the table to make sure we 
;;;set small correctly
(defun push-queue (l n) 
  (setf (gethash n *my-hash*) l)
  (let ((hk (hash-table-count *my-hash*)))
    (if (= hk 1)
            (setf small n))))

;;;create an enviroment where we have the number of items
;;;on the stack in hk
;;;if hk is 1 that means our stack will be empty on the next operation
;;;note this will fail if we try to remove from an empty stack
;;;so we set small to 0
;;;if not we remove the item from the front of the stack
;;;and increment small until it hits the next op step value

(defun pull-queue ()
  (let ((hk (hash-table-count *my-hash*)))
    (cond
      ((= hk 1)
       (progn
           (remhash small *my-hash*)
           (setf small 0))) 
      (t
       (progn
        (remhash small *my-hash*)
        (reset-small (+ small 1)))))))

;;;recursively increment small until hitting an op step value
(defun reset-small (n)
  (if
   (gethash n *my-hash*)
   (setf small n)
   (reset-small (+ n 1))))

;;;whatever small is pointing to is the first item on our stack
(defun first-queue ()
  (gethash small *my-hash*))

;;;Utility function grabbed from the internet
;;;simply splits strings by space
(defun my-split (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

;;;main loop if our max-ops are equal to the max we stop
(defun evalq (max-ops)
      (declare (type fixnum max-ops)
           (optimize (speed 3)))
  (cond
    ((= max-ops countsd) '())
    (t
     ;;;the only case where split string is = 2 is
     ;;;when we are dealing with op-code 1
     (let ((split (my-split (read-line))))
       (if (= (length split) 2)
           (progn
               (push-queue (parse-integer
                                (second split)) 
                               max-ops)   
               (evalq (+ max-ops 1)))    
           (let
               ((op-code (parse-integer (first split))))
             (cond
               ((= op-code 2)
                (progn
                  (pull-queue)
                  (evalq (+ max-ops 1))))                  
               ((= op-code 3)
                (progn
                  (format t "~d ~%" (first-queue))
                  (evalq (+ max-ops 1))))
               (t (format t "invalid operation")))))))))
(evalq 0)
