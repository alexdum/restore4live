$(document).on('shiny:connected', function () {
    // Notify server when modal closes
    $(document).on('hidden.bs.modal', '.modal', function () {
        Shiny.setInputValue('modal_closed', new Date().getTime(), { priority: 'event' });
    });

    // Handle freeze window close
    $(document).on('click', '.frozen-overlay-close', function () {
        Shiny.setInputValue('cancel_loading', new Date().getTime(), { priority: 'event' });
    });

    // Auto-collapse mobile menu when a link is clicked
    // bslib doesn't strictly have an option for this, so we use the native Bootstrap API
    document.addEventListener('click', function (event) {
        var navLink = event.target.closest('.navbar-collapse .nav-link, .navbar-collapse .dropdown-item');
        if (!navLink) {
            return;
        }

        var collapseEl = navLink.closest('.navbar-collapse');
        if (!collapseEl || !collapseEl.classList.contains('show')) {
            return;
        }

        var toggler = document.querySelector('.navbar-toggler');
        if (!toggler || window.getComputedStyle(toggler).display === 'none') {
            return;
        }

        if (window.bootstrap && window.bootstrap.Collapse) {
            var bsCollapse = window.bootstrap.Collapse.getInstance(collapseEl);
            if (!bsCollapse) {
                bsCollapse = new window.bootstrap.Collapse(collapseEl, { toggle: false });
            }
            bsCollapse.hide();
        } else {
            toggler.click();
        }
    }, true);

    // Hide Home tab robustly by checking text content
    function hideHomeTab() {
        $('.navbar-nav .nav-link').filter(function () {
            return $(this).text().trim() === "Home";
        }).parent().hide();
    }

    hideHomeTab();
    // Retry a few times in case of dynamic rendering delays
    setTimeout(hideHomeTab, 100);
    setTimeout(hideHomeTab, 500);
    setTimeout(hideHomeTab, 1000);
});

// Functions to freeze/unfreeze UI
Shiny.addCustomMessageHandler('freezeUI', function (message) {
    $('body').addClass('ui-frozen');
    $('.frozen-overlay').addClass('active');
    if (message.station) {
        $('.frozen-overlay-station').html(message.station);
    }
    $('.frozen-overlay-message').html(message.text || 'Loading data...');
});

Shiny.addCustomMessageHandler('unfreezeUI', function (message) {
    $('body').removeClass('ui-frozen');
    $('.frozen-overlay').removeClass('active');
});
