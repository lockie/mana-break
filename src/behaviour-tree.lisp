(in-package #:mana-break)


(defmacro define-behaviour-tree-node (name (&rest slots) (&rest options)
                                      &body body)
  "Vars in body:
* status-completed
* status-succeeded
* 'name'-'slot'"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defun ,(symbolicate :% name :-slots) ()
       ',slots)
     (defun ,(symbolicate :% name :-options) ()
       ',options)
     (defun ,(symbolicate :% name :-body) ()
       (let ((body ',body))
         `(progn
            ,@body)))))

(define-behaviour-tree-node dummy-true () nil
  (complete-node t))

(define-behaviour-tree-node dummy-false () nil
  (complete-node nil))

(define-behaviour-tree-node idle
    ((time 1.0 :type single-float :documentation "Idle time in seconds."))
  (:arguments ((:dt single-float)))
  "Idle node succeeds after given period of time."
  (if (plusp idle-time)
      (decf idle-time dt)
      (complete-node t)))

(define-behaviour-tree-node selector
    ((current 0 :type array-index)) nil
  "Selector is like an OR logical element."
  (when (child-completed-p selector-current)
    (if (child-succeeded-p selector-current)
        (return-from ecs::current-entity (complete-node t))
        (when (>= (incf selector-current) children-count)
          (return-from ecs::current-entity (complete-node nil)))))
  (deactivate)
  (activate-child selector-current))

(define-behaviour-tree-node sequence
    ((current 0 :type array-index)) nil
  "Sequence node is like an AND logical element."
  (when (child-completed-p sequence-current)
    (if (not (child-succeeded-p sequence-current))
        (return-from ecs::current-entity (complete-node nil))
        (when (>= (incf sequence-current) children-count)
          (return-from ecs::current-entity (complete-node t)))))
  (deactivate)
  (activate-child sequence-current))

(define-behaviour-tree-node repeat () nil
  (when (child-completed-p 0)
    (reset-children))
  (deactivate)
  (activate-child 0))

(define-behaviour-tree-node repeat-until-fail () nil
  (when (child-completed-p 0)
    (if (child-succeeded-p 0)
        (reset-children)
        (return-from ecs::current-entity (complete-node t))))
  (deactivate)
  (activate-child 0))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-let+-expansion (&helper (name lambda-list &body function-body)
                             :uses-value? nil)
    `(flet ((,name ,lambda-list ,@function-body))
       (declare (ignorable (function ,name))
                (inline ,name))
       ,@let-plus::body)))

(defmacro define-behaviour-tree (name tree-spec &key (debug nil))
  "Same node types in tree should have different :name"
  (let+ ((nodes nil)
         (nodes-params nil)
         ((&labels child-accessors (children prefix suffix)
            (loop :for (type opts) :in children
                  :for child := (or (getf opts :name) type)
                  :for child-name := (symbolicate name :- (string-upcase child))
                  :for i :from 0
                  :collecting `(,i (,(symbolicate prefix child-name suffix)
                                    entity)))))
         ((&labels reset-children (spec)
            (let+ (((&ign &ign &rest children) spec))
              (mapcan
               #'(lambda (child)
                   (let+ (((type opts &rest &ign) child)
                          (child-name
                           (string-upcase (or (getf opts :name) type)))
                          (slots (funcall (symbolicate :% type :-slots))))
                     `((setf (,(symbolicate name :- child-name
                                                 :-status-completed)
                              entity) 0)
                       (,(symbolicate :with- name :- child-name) nil entity
                        (setf
                         ,@(mapcan
                            #'(lambda (slot) `(,(first slot)
                                          (or
                                           (getf ',opts
                                                 ,(make-keyword (first slot)))
                                           ,(second slot))))
                            slots)))
                       ,@(reset-children child))))
               children))))
         ((&labels delete-node (spec)
            (let+ (((type options &rest children) spec)
                   (n (or (getf options :name) type))
                   (node-name (symbolicate name :- (string-upcase n))))
              `(progn
                 (,(symbolicate :delete- node-name) entity)
                 (when (,(symbolicate :has- node-name :-active-p) entity)
                   (,(symbolicate :delete- node-name :-active) entity))
                 (,(symbolicate :delete- node-name :-status) entity)
                 ,@(mapcar #'delete-node children)))))
         ((&labels tree-node-components (parent spec)
            (declare (ignore parent))
            (let+ (((type options &rest children) spec)
                   (n (or (getf options :name) type))
                   (node-name (symbolicate name :- (string-upcase n)))
                   (slots (funcall (symbolicate :% type :-slots))))
              `(progn
                 (ecs:defcomponent ,node-name ,@slots)
                 (ecs:defcomponent ,(symbolicate node-name :-active))
                 (ecs:defcomponent ,(symbolicate node-name :-status)
                   (completed 0 :type bit)
                   (succeeded 0 :type bit))
                 ,@(mapcar
                    #'(lambda (child-spec)
                        (tree-node-components node-name child-spec))
                    children)))))
         ((&labels tree-node-systems (parent spec)
            (let+ (((type options &rest children) spec)
                   (n (or (getf options :name) type))
                   (node-name (symbolicate name :- (string-upcase n)))
                   (options* (copy-list options))
                   (slots (funcall (symbolicate :% type :-slots)))
                   (system-opts (funcall (symbolicate :% type :-options)))
                   (components-ro (getf system-opts :components-ro))
                   (components-rw (getf system-opts :components-rw))
                   (system-opts* (copy-list system-opts)))
              (remf options* :name)
              (remf system-opts* :components-ro)
              (remf system-opts* :components-rw)
              (push node-name nodes)
              (push options* nodes-params)
              `(progn
                 ,@(mapcar
                    #'(lambda (child-spec)
                        (tree-node-systems node-name child-spec))
                    children)
                 (ecs:defsystem ,node-name
                   (:components-ro (,(symbolicate node-name :-active)
                                    ,@components-ro)
                    :components-rw (,(symbolicate node-name)
                                    ,(symbolicate node-name :-status)
                                    ,@components-rw)
                    ,@system-opts*)
                   (let+ (((&symbol-macrolet
                            status-completed
                            ,(symbolicate node-name :-status-completed)))
                          ((&symbol-macrolet
                            status-succeeded
                            ,(symbolicate node-name :-status-succeeded)))
                          ,@(loop :for slot :in slots
                                  :for slot-name := (first slot)
                                  :collecting
                                  `((&symbol-macrolet
                                     ,(symbolicate type :- slot-name)
                                     ,(symbolicate node-name :- slot-name))))
                          ((&helper complete-node (success)
                             (setf status-completed 1
                                   status-succeeded (if success 1 0))
                             (,(symbolicate :delete- node-name :-active) entity)
                             ,(when parent
                                `(,(symbolicate :make- parent :-active) entity))
                             nil))
                          ((&helper child-completed-p (i)
                             ,(when children
                                `(plusp (case i ,@(child-accessors children
                                                   "" :-status-completed)
                                              (t 0))))))
                          ((&helper child-succeeded-p (i)
                             ,(when children
                                `(plusp (case i ,@(child-accessors children
                                                   "" :-status-succeeded)
                                              (t 0))))))
                          ((&helper deactivate ()
                             (,(symbolicate :delete- node-name :-active)
                              entity)))
                          ((&helper activate-child (i)
                             ,(when children
                                `(case i ,@(child-accessors children
                                            :make- :-active)))))
                          ((&helper reset-children ()
                             ,@(reset-children spec)))
                          ((&helper delete-tree ()
                             ,(delete-node tree-spec)))
                          (children-count ,(length children)))
                     (declare (ignorable children-count))
                     ,(when debug
                        `(format t "running ~a node ~s for entity ~a~%"
                                 ',type ',n entity))
                     ,(funcall (symbolicate :% type :-body)))))))))
    `(progn
      ,(tree-node-components nil tree-spec)
      ,(tree-node-systems nil tree-spec)
      (defun ,(symbolicate :make- name :-behaviour-tree) (entity)
        ,@(mapcar #'(lambda (node params)
                      `(,(symbolicate :make- node) entity ,@params))
                  nodes nodes-params)
        ,@(mapcar #'(lambda (node)
                      `(,(symbolicate :make- node :-status) entity))
                  nodes)
        (,(symbolicate :make- (car (last nodes)) :-active) entity)
        nil)
      (defun ,(symbolicate :delete- name :-behaviour-tree) (entity)
        ,(delete-node tree-spec)))))
