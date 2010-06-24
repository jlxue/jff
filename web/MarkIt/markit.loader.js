(function() {
    var dialog = document.getElementById('markit-dialog');
    if (dialog) {
        jQuery(dialog).toggle();
        return;
    }

    var scripts = document.getElementsByTagName('script');
    var url='##MARKIT_URL##';

    for (var i = 0; i < scripts.length; ++i) {
        if (url == scripts[i].getAttribute('src')) {
            alert('Loading MarkIt dialog, please try again later.');
            return;
        }
    }

    var s = document.createElement('script');
    s.setAttribute('src', url);
    document.getElementsByTagName('body')[0].appendChild(s);
})()

