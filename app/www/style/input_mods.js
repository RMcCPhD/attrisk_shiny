
// Config
const UI_HELPERS_CONFIG = {
  placeholders: {
    duration:    '12 - 260',
    sample:      '30 - 1000',
    facilities:  '1 - 500',
    arms:        '2 - 6',
    age:         '18 - 100',
    bmi:         '18 - 50',
    sbp:         '60 - 140',
    hdl:         '1 - 4',
    totalc:      '1 - 4',
    t2dur:       '0.01 - 50',
    hba1c:       '6 - 9',
    egfr:        '60 - 120',
    conmed:      '0 - 20',
    comorbs:     '0 - 10'
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