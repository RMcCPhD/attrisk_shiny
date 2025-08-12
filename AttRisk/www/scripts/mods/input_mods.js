
// Config
const UI_HELPERS_CONFIG = {
  placeholders: {
    duration:   'Total (2 - 260)',
    sample:     'Total (30 - 20,000)',
    facilities: 'Total (1 - 500)',
    arms:       'Total (2 - 6)',
    mean_age:   'Mean (18 - 150)',
    mean_bmi:   'Mean (18 - 50)',
    mean_sbp:   'Mean (120 - 140)',
    mean_hdl:   'Mean (1 - 4)',
    mean_total: 'Mean (1 - 4)',
    mean_t2dur: 'Mean (0.01 - 50)',
    mean_hba1c: 'Mean (6 - 9)',
    mean_egfr:  'Mean (60 - 120)',
    con_med:    'Total (0 - 20)',
    comorbs:    'Total (0 - 20)'
  }
};

// Apply placeholders (i.e. greyed out text) to each input
function applyPlaceholders(map) {
  if (!map) return;
  Object.keys(map).forEach(function(id) {
    const $el = $('#' + id);
    if ($el.length) $el.attr('placeholder', map[id]);
  });
}

// Initiate application
$(document).on('shiny:connected', function() {
  applyPlaceholders(UI_HELPERS_CONFIG.placeholders);
});

// Reset user inputs upon restart button activation
Shiny.addCustomMessageHandler('resetBox', function(id) {
  const box = document.getElementById(id);
  if (box) {
    const inputs = box.querySelectorAll('input, select, textarea');
    inputs.forEach(el => {
      if (el.type == 'checkbox' || el.type == 'radio') {
        el.checked = el.defaultChecked;
      } else {
        el.value = el.defaultValue;
      }
      el.dispatchEvent(new Event('change', { bubbles: true }));
    });
  }
});