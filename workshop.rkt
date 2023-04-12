#lang slideshow/widescreen

;; Utilities for taking screenshots of terminal output
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

;; Utilities for drawing commit graphs
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
(define (dag nodelistlist edges)
    (define nodemap 
        (make-hash
            (map (lambda (node) (cons (id node) (commit-pict node)))
                (apply append nodelistlist))))
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

;; Initialize the demo setup
; first, create the directory
(system "rm -rf /tmp/git-workshop-demo")
(system "rm -rf /tmp/demo-clone")
(system "mkdir /tmp/git-workshop-demo")
(define old-pwd (current-directory))
(current-directory "/tmp/git-workshop-demo")
; Make git not use a pager so it doesn't hang
(putenv "GIT_PAGER" "")

(system "gh repo delete ColonelPhantom/workshop-demo --yes")
(system "gh repo create --private ColonelPhantom/workshop-demo")

;; Slideshow starts here

;; Introduction
(slide #:name "Thalia Git workshop 2023"
    (titlet "Thalia Git workshop 2023"))

(slide #:title "Why Git?"
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
            "find last working version")
        (subitem (tt "git blame") ": find commit that introduced a change")
        (subitem (tt "git revert") ": undo a change")))

(slide #:title "Fundamentals of Git"
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

(slide #:title "Branching"
    (para
        (item "Branches point to commits")
        (item "Default branch is usually called" (tt "main") "or" (tt "master"))
        (item "Useful for working in parallel")
        (item "Remote branches are branches on a server"))

    (dag
        '(
            (A)
            (B (D "feature1"))
            (C (E "origin/feature1")))
        '(
            (A B)
            (B C)
            (A D)
            (D E))))

(slide #:title "Merging and rebasing"

    (hc-append 128
        (para #:width 640
            (item #:width 640 "Merging")
            (subitem #:width 640 "Combine changes from two branches")
            (item #:width 640 "Rebasing")
            (subitem #:width 640 "Redo changes on top of another commit")
            (subitem #:width 640 "Rewrites history and gives commits new IDs")
            (subitem #:width 640 "Cleaner history, but interacts oddly with remote branches"))
        (dag
            '(
                (A)
                (B D)
                (C E)
                ((F "merge")))
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

;; Demonstration
(slide #:title "Creating a new repo"
    (termpict
        "ls -a"
        "git init"
        "ls -a")
    (para
        (item "The" (tt ".git") "directory contains all the information about the repository")
        (item "This can also be done in non-empty directories")))

(slide #:title "Showing status"
    (termpict "git status"))

(slide #:title "Let's create a file"
    (termpict
        "echo 'Hello world' > hello.txt"
        "cat hello.txt"
        "git status"))

(slide #:title "Add the file to the index"
    (para 
        (item "The index is a list of files that will be committed")
        (item "You can add files to the index with" (tt "git add"))
        (subitem "To only add some changes to a file, use" (tt "git add -p"))
        (item "If a file should never be added, you can add it to the" (tt ".gitignore") "file"))
    (scale (termpict "git add hello.txt" "git status") 0.75))
    
(slide #:title "Commit the file"
    (termpict
        "git commit -m 'Created hello.txt'"
        "git status"))

(slide #:title "Commit message conventions"
    (para
        (item "There are multiple ways to set commit messages")
        (subitem "You can use" (tt "git commit -m 'message'") "to set a message")
        (subitem "To create a longer message, pass the" (tt "-m") "flag multiple times")
        (subitem "If you do not pass the" (tt "-m") "flag, git will open" (tt "$EDITOR") "(usually vim or nano) to let you write a message")
        (item "First line is a short summary")
        (item "Second line is blank")
        (item "Third line (or second " (tt "-m") "flag) is a longer description")
        (item "Use imperative mood for summary (e.g. 'Add' instead of 'Added')")
        (subitem "Matches commit messages created by commands like " (tt "git merge") "and " (tt "git revert"))
        (subitem "Hint: the sentence 'If applied, this commit will' should make sense")))

(slide #:title "Correcting commit messages"
    (para
        (item "Wait, I made a mistake in my commit message, it's not imperative!")
        (item "No worries, we can fix it with " (tt "git commit --amend"))
        (item "Amend is also useful to fix other mistakes in the commit itself, like forgetting to add a file or a typo"))
    (scale (termpict "git commit --amend -m 'Add hello.txt'") 0.8))

(slide #:title "Another commit"
    (termpict
        "echo 'Imperative Programming' >> courses.txt"
        "echo 'Object Oriented Programming' >> courses.txt"
        "cat courses.txt"
        "git add courses.txt"
        "git commit -m 'Add courses.txt'"))

(slide #:title "Viewing the history"
    (termpict "git log"))

(slide #:title "Working in another branch"
    (termpict
        "git switch -c used-langs"
        "echo 'Imperative Programming - C++' > courses.txt"
        "echo 'Object Oriented Programming - Java' >> courses.txt"
        "cat courses.txt"
        "git add courses.txt"
        "git commit -m 'Add used languages to courses.txt'"))

; update main in the background
(system "git switch main")
(system "echo 'Functional Programming' >> courses.txt")
(system "git add courses.txt")
(system "git commit -m 'Add FP to courses.txt'")
(system "git switch used-langs")

(slide #:title "Merging back"
    (termpict
        "git switch main"
        "git merge used-langs"
        "git status"))

(slide #:title "Merge conflicts"
    (para "Merging failed because of a conflict:")
    (termpict
        "git log --graph --oneline --all"
        "cat courses.txt"))

(system "echo 'Imperative Programming - C++' > courses.txt")
(system "echo 'Object Oriented Programming - Java' >> courses.txt")
(system "echo 'Functional Programming - Haskell' >> courses.txt")

(putenv "GIT_EDITOR" "echo")

(slide #:title "Resolving merge conflicts"
    (para
        (item "Git marked the conflict with" (tt "<<<<<<< HEAD") "and" (tt ">>>>>>>") "lines")
        (item "The lines between the two are the conflicting changes")
        (item "Edit the file(s) to resolve the conflict(s)")
        (item #:width 1200 "Once you are done, you can add the file to the index and continue the merge")
        (subitem "You can also abort the merge with " (tt "git merge --abort")))
    (termpict
        "cat courses.txt"
        "git add courses.txt"
        "git merge --continue"))

(slide #:title "Working with remotes"
    (para
        (item "Remotes are other repositories that you can push and pull from")
        (item "You can have multiple remotes, but usually there's only one"))
    (scale (bitmap (normalize-path "github-init.png" old-pwd)) 0.8))

(slide #:title "Pushing to a remote"
    (termpict
        "git remote add origin git@github.com:ColonelPhantom/workshop-demo.git"
        "git push"))

(slide #:title "Wait, what?"
    (para
        "Git says we need to specify where to push to. Luckily it tells us how!"
        "(Instead of" (tt "--set-upstream") ", we can use" (tt "-u") "for short.)"
        "If it fails, you can use --force but use it only if you're sure!")
    (scale (termpict "git push -u origin main ") 0.8))

(current-directory "/tmp")
(slide #:title "Cloning"
    (para
        "You can download a copy of a repo with" (tt "git clone <url>") "."
        "It's also possible to specify where to put it:" (tt "git clone <url> <directory>") ".")
    (termpict
        "cd .."
        "git clone git@github.com:ColonelPhantom/workshop-demo.git demo-clone"
        "cd demo-clone"))

(current-directory "/tmp/demo-clone")
(slide #:title "Working in the cloned repo"
    (termpict 
        "echo 'Data Mining - Python' >> courses.txt"
        "git add courses.txt"
        "git commit -m 'Add Data Mining to courses.txt'"
        "git push"))

(current-directory "/tmp/git-workshop-demo")
; create new branch in background
(system "git switch -c add-readme")
(system "echo 'This is a demo repo for the git workshop' > README.md")
(system "git add README.md")
(system "git commit -m 'Add README'")
(system "git switch main")

(slide #:title "Fetching"
    (para
        "Fetching is a way to get changes from a remote without merging them"
        "It's useful when you want to see what's changed without changing your local history")
    (scale 
        (termpict
            "cd ../git-workshop-demo"
            "git fetch"
            "git log --graph --oneline --all")
        0.8))

(slide #:title "Pulling"
    (para
        "With" (tt "git pull") ", you can merge the remote changes."
        "Pulling also fetches, so you don't need to do that separately.")
    (scale 
        (termpict
            "git pull"
            "git log --graph --oneline --all")
        0.8))


(slide #:title "Rebasing"
    (para
        "Rebasing is a way to integrate changes from one branch into another."
        "It's useful when you want to keep a nice linear history."
        "Do be careful though, as the hash changes and you will need to force push."
        "So you should avoid rebasing branches that others are also working on.")
    (scale
        (termpict
            "git switch add-readme"
            "git rebase main"
            "git log --graph --oneline --all")
        0.8))

(slide #:title "Other useful commands"
    (item (tt "git stash") "and" (tt "git stash pop") "are useful for temporarily saving changes")
    (item (tt "git diff") "can show what changed between two points in time")
    (item (tt "git bisect") "can help you find the commit that introduced a bug")
    (item (tt "git tag") "can be used to mark a commit with a name, e.g. a version number")
    (item (tt "git rebase -i") "can be used to edit the history of a branch")
    (subitem "For example, squashing commits, rewording commit messages, reordering commits, removing commits"))

(slide #:title "Useful resources"
    (item "Git has manpages for all of its commands:" (tt "man git-<command>"))
    (subitem "Can be very technical")
    (subitem "Usually the first few paragraphs give a good overview")
    (subitem "You can type '/EXAMPLES' <Enter> for more practical information")
    (item "The repository containing these slides also contains some links to useful resources.")
    (subitem "https://github.com/ColonelPhantom/git-workshop")
    (subitem "For example an interactive tutorial for git branching or some cheat sheets"))
