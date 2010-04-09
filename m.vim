function! AddBookmarksToLocationList()
    redir => tmp
    marks
    redir END
    let bookmarks=split(tmp, '\n')
    let locations=[]

    for bookmark in bookmarks
        let l=matchlist(bookmark, '^\s*\(.\)\s\+\(\d\+\)\s\+\(\d\+\)\s\+\(.\+\)')
        if empty(l)
            continue
        endif
        if ! filereadable(l[4])
            let n=4
            let l[5]=l[4]
            let l[4]=expand("%:p")
        else
            let n=4
        endif
        if ! filereadable(l[4])
            continue
        endif
        call add(locations, join(l[1:n], " "))
    endfor

    let save_efm=&efm
    set efm=%t\ %l\ %c\ %f
    laddexpr locations
    let &efm=save_efm

    lopen
endfunction

:command! Bookmark :call AddBookmarksToLocationList()

