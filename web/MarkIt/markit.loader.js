(function() {
    var dialog = document.getElementById('markit-dialog');

    if (! dialog) {
        var s = document.createElement('script');
        s.setAttribute('src', '##MARKIT_URL##');
        document.getElementsByTagName('body')[0].appendChild(s);
    } else if (dialog.getAttribute('ready')) {
        $(dialog).toggle();
    } else {
        alert("Loading MarkIt dialog, please try again later.");
    }

})()

