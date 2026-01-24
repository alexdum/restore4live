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
    $(document).on('click', '.navbar-collapse.in .nav-link', function () {
        var toggle = $('.navbar-toggler');
        if (toggle.is(':visible')) {
            toggle.click();
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
