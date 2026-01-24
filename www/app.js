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
    $(document).on('click', '.navbar-collapse.show .nav-link', function (e) {
        // Only act if the toggler is visible (mobile mode)
        if ($('.navbar-toggler').is(':visible')) {
            // Use Native Bootstrap 5 API to find and hide the collapsible element
            // This is more robust than clicking the toggler
            var collapseEl = $(this).closest('.navbar-collapse')[0];
            if (collapseEl) {
                // Try getting existing instance or creating new one
                var bsCollapse = bootstrap.Collapse.getInstance(collapseEl);
                if (!bsCollapse) {
                    bsCollapse = new bootstrap.Collapse(collapseEl, { toggle: false });
                }
                bsCollapse.hide();
            }
        }
    });

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
