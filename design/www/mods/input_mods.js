
// Config
const UI_HELPERS_CONFIG = {
  placeholders: {
    duration:    '12 - 260 weeks',
    sample:      '30 - 20,000',
    facilities:  '1 - 500',
    arms:        '2 - 6',
    mean_age:    '18 - 100',
    mean_bmi:    '18 - 50',
    mean_sbp:    '60 - 140',
    mean_hdl:    '1 - 4',
    mean_total:  '1 - 4',
    mean_t2dur:  '0.01 - 50',
    mean_hba1c:  '6 - 9',
    mean_egfr:   '60 - 120',
    con_med:     '0 - 20',
    comorbs:     '0 - 20'
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