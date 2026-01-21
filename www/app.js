$(document).on('shiny:connected', function () {
    // Notify server when modal closes
    $(document).on('hidden.bs.modal', '.modal', function () {
        Shiny.setInputValue('modal_closed', new Date().getTime(), { priority: 'event' });
    });

    // Handle freeze window close
    $(document).on('click', '.frozen-overlay-close', function () {
        Shiny.setInputValue('cancel_loading', new Date().getTime(), { priority: 'event' });
    });
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
