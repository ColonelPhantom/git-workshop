#lang slideshow/widescreen

(define (termshot cmd)
    (or
        (system* (find-executable-path "termshot")
            "-cf"
            "/tmp/termshot.png"
            "--"
            cmd)
        (error 'termshot cmd)))

(define (termpict cmd)
    (termshot cmd)
    (scale-to-fit (bitmap "/tmp/termshot.png") titleless-page))

(slide
    #:name "Thalia Git workshop 2023"
    (titlet "Thalia Git workshop 2023"))

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
    #:title "Creating a new repo"
    (termpict "git init"))

(slide
    #:title "Showing status"
    (termpict "git status"))
