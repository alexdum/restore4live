function closeNavbarMenu() {
    var togglers = document.querySelectorAll('.navbar-toggler');
    var closedViaToggler = false;

    togglers.forEach(function (toggler) {
        var expanded = toggler.getAttribute('aria-expanded');
        var isOpen = expanded === 'true' || !toggler.classList.contains('collapsed');
        if (!isOpen) {
            return;
        }
        toggler.click();
        closedViaToggler = true;
    });

    if (closedViaToggler) {
        return;
    }

    var collapseEl = document.querySelector('.navbar-collapse.show');
    if (collapseEl && window.bootstrap && window.bootstrap.Collapse) {
        var bsCollapse = window.bootstrap.Collapse.getInstance(collapseEl);
        if (!bsCollapse) {
            bsCollapse = new window.bootstrap.Collapse(collapseEl, { toggle: false });
        }
        bsCollapse.hide();
        return;
    }

    var offcanvasEl = document.querySelector('.offcanvas.show');
    if (offcanvasEl && window.bootstrap && window.bootstrap.Offcanvas) {
        var bsOffcanvas = window.bootstrap.Offcanvas.getInstance(offcanvasEl);
        if (!bsOffcanvas) {
            bsOffcanvas = new window.bootstrap.Offcanvas(offcanvasEl, { backdrop: true });
        }
        bsOffcanvas.hide();
        return;
    }
}

function initAppHandlers() {
    if (window.__restore4liveHandlersInitialized) {
        return;
    }
    window.__restore4liveHandlersInitialized = true;

    var $document = window.jQuery ? window.jQuery(document) : null;

    if ($document) {
        // Notify server when modal closes
        $document.on('hidden.bs.modal', '.modal', function () {
            if (window.Shiny && window.Shiny.setInputValue) {
                Shiny.setInputValue('modal_closed', new Date().getTime(), { priority: 'event' });
            }
        });

        // Handle freeze window close
        $document.on('click', '.frozen-overlay-close', function () {
            if (window.Shiny && window.Shiny.setInputValue) {
                Shiny.setInputValue('cancel_loading', new Date().getTime(), { priority: 'event' });
            }
        });
    } else {
        document.addEventListener('hidden.bs.modal', function (event) {
            if (!event.target || !event.target.closest('.modal')) {
                return;
            }
            if (window.Shiny && window.Shiny.setInputValue) {
                Shiny.setInputValue('modal_closed', new Date().getTime(), { priority: 'event' });
            }
        });

        document.addEventListener('click', function (event) {
            var closeButton = event.target.closest('.frozen-overlay-close');
            if (!closeButton) {
                return;
            }
            if (window.Shiny && window.Shiny.setInputValue) {
                Shiny.setInputValue('cancel_loading', new Date().getTime(), { priority: 'event' });
            }
        });
    }

    // Auto-collapse mobile menu when a link is clicked
    // bslib doesn't strictly have an option for this, so we use the native Bootstrap API
    document.addEventListener('click', function (event) {
        var target = event.target && event.target.closest ? event.target : null;
        var navLink = target ? target.closest('.navbar .nav-link, .navbar .dropdown-item') : null;
        if (!navLink) {
            return;
        }
        window.setTimeout(closeNavbarMenu, 0);
    }, true);

    if ($document) {
        $document.on('shiny:inputchanged', function (event) {
            if (event.name === 'navbar') {
                window.setTimeout(closeNavbarMenu, 0);
            }
        });
    }

    // Hide Home tab robustly by checking text content
    function hideHomeTab() {
        if ($document) {
            $document.find('.navbar-nav .nav-link').filter(function () {
                return window.jQuery(this).text().trim() === "Home";
            }).parent().hide();
            return;
        }

        var navLinks = document.querySelectorAll('.navbar-nav .nav-link');
        navLinks.forEach(function (link) {
            if (link.textContent.trim() === 'Home') {
                var parentItem = link.closest('.nav-item');
                if (parentItem) {
                    parentItem.style.display = 'none';
                } else {
                    link.style.display = 'none';
                }
            }
        });
    }

    hideHomeTab();
    // Retry a few times in case of dynamic rendering delays
    setTimeout(hideHomeTab, 100);
    setTimeout(hideHomeTab, 500);
    setTimeout(hideHomeTab, 1000);

}

if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initAppHandlers);
} else {
    initAppHandlers();
}

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
