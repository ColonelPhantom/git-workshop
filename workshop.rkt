#lang slideshow/widescreen

(define (termshot . cmd)
    (or
        (apply system* (find-executable-path "termshot")
            "-cf"
            "/tmp/termshot.png"
            "--"
            cmd)
        (error 'termshot cmd)))

(define (termpict . cmd)
    (apply termshot cmd)
    (scale-to-fit (bitmap "/tmp/termshot.png") titleless-page))

(define (commit-pict name)
    (cond
        (
            (list? name)
            (let* ([id (car name)]
                   [msg (cadr name)]
                   [block (car (commit-pict id))]
                   [msg-pict (t msg)])
                (cons block (hc-append 32 block msg-pict))))
        (
            (string? name)
            (cc-superimpose
                (filled-rounded-rectangle 64 64 #:color "indigo")
                (colorize (t name) "white"))
            (let* ([block (filled-rounded-rectangle 64 64 #:color "indigo")]
                   [id-pict (colorize (t name) "white")]
                   [pict (cc-superimpose block id-pict)])
                (cons pict pict)))
        (
            (symbol? name)
            (commit-pict (symbol->string name)))))

(define (parent-pict pict parent child)
    (pin-arrow-line 25 pict
        parent cb-find
        child ct-find))

(define (id commit)
    (cond
        ((list? commit) (car commit))
        ((symbol? commit) commit)
        ((string? commit) (string->symbol commit))))

;; Draw a DAG
(define (dag nodelistlist edges)
    (println (apply append nodelistlist))
    (define nodemap 
        (make-hash
            (map (lambda (node) (cons (id node) (commit-pict node)))
                (apply append nodelistlist))))
    (println nodemap)
    (define (node-pict nodename)
        (cdr (hash-ref nodemap (id nodename))))
    (define (row nodes)
        (apply hc-append 64 (map node-pict nodes)))
    (define base-pict
        (apply vl-append 64 (map row nodelistlist)))
    (define (edge-pict edge)
        (parent-pict base-pict
            (car (hash-ref nodemap (car edge)))
            (car (hash-ref nodemap (cadr edge)))))
    (apply cc-superimpose
        base-pict
        (map edge-pict edges)))

(slide
    #:name "Thalia Git workshop 2023"
    (titlet "Thalia Git workshop 2023"))

(system "rm -rf /tmp/git-workshop-demo")
(system "mkdir /tmp/git-workshop-demo")
(current-directory "/tmp/git-workshop-demo")

(slide 
    #:title "Why Git?"
    (para
        (item "Collaboration")
        (subitem
            (tt "git pull") "and" (tt "git push") ":"
                "send and receive changes")
        (subitem
            (tt "git merge") "or" (tt "git rebase") ":"
                "combine changes")
        (para)
        (subitem "Often done with PRs/MRs on a service like Github/Gitlab/Gitea")
        (item "History")
        (subitem (tt "git log") ":"
            "view history of changes")
        (subitem (tt "git diff") ":"
            "view what changed between two points in time")
        (subitem (tt "git bisect") ":"
            "find last working version")))

(slide
    #:title "Fundamentals of Git"
    (para
        (item "Best with text files (including code or even LaTeX)")
        (item "Basic building block:" (bt "commits") "(like snapshots)")
        (subitem "Consists of a message, a list of changes, and a parent"))

    (dag
        '(
            ((A "Initial commit"))
            ((B "Add file1"))
            ((C "Change file2")))
        '(
            (A B)
            (B C))))

(slide
    #:title "Branching"
    (para
        (item "Branches point to commits")
        (item "Default branch is usually called" (tt "main") "or" (tt "master"))
        (item "Useful for working in parallel"))

    (dag
        '(
            (A)
            (B D)
            (C E))
        '(
            (A B)
            (B C)
            (A D)
            (D E))))


(slide
    #:title "Merging and rebasing"

    (hc-append 128
        (para #:width 640
            (item #:width 640 "Merging")
            (subitem #:width 640 "Combine changes from two branches")
            (item #:width 640 "Rebasing")
            (subitem #:width 720 "Redo changes on top of another commit"))
        (dag
            '(
                (A)
                (B D)
                (C E)
                (F))
            '(
                (A B)
                (B C)
                (A D)
                (D E)
                (E F)
                (C F)))
        (dag
            '(
                (A)
                (B)
                (C)
                (D1)(E1))
            '(
                (A B)
                (B C)
                (C D1)
                (D1 E1)))))    

(slide
    #:title "Creating a new repo"
    (termpict
        "ls -a"
        "git init"
        "ls -a"))

(slide
    #:title "Showing status"
    (termpict "git status"))
