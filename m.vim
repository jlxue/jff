function! AddBookmarksToLocationList()
    redir => tmp
    silent marks
    redir END
    let bookmarks=split(tmp, '\n')
    let locations=[]

    for bookmark in bookmarks
        let l=matchlist(bookmark, '^\s*\([0-9a-zA-Z]\)\s\+\(\d\+\)\s\+\(\d\+\)\s\+\(.\+\)')
        if empty(l)
            continue
        endif

        let f=substitute(l[4], "^\\~/", $HOME . "/", "")
        if filereadable(f)
            let l[5]=l[1]
            let l[4]=f
        else
            let l[5]=l[4]
            let l[4]=expand("%:p")
            if ! filereadable(l[4])
                continue
            endif
        endif
        call add(locations, join(l[1:5], " "))
    endfor

    let save_efm=&efm
    set efm=%t\ %l\ %c\ %f\ %s
    laddexpr locations
    let &efm=save_efm

    lopen
endfunction

:command! Bookmark :call AddBookmarksToLocationList()

