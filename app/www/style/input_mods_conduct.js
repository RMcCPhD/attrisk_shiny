
// Conduct placeholders
(() => {
  // Config
  const UI_HELPERS_CONFIG2 = {
    placeholders2: {
      'conduct-duration':    '12 - 520',
      'conduct-age':         '18 - 100',
      'conduct-bmi':         '18 - 50',
      'conduct-sbp':         '60 - 140',
      'conduct-hdl':         '1 - 4',
      'conduct-totalc':      '1 - 4',
      'conduct-t2dur':       '0.01 - 50',
      'conduct-hba1c':       '6 - 9',
      'conduct-egfr':        '60 - 120',
      'conduct-conmed':      '0 - 20',
      'conduct-comorbs':     '0 - 10'
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
    applyPlaceholders(UI_HELPERS_CONFIG2.placeholders2);
  });
  
  // Reset user inputs upon restart button activation
  if (!window.__resetBox2Registered) {
    window.__resetBox2Registered = true;
    Shiny.addCustomMessageHandler('resetBox2', function(id) {
      const box = document.getElementById(id);
      if (!box) return;
      const inputs = box.querySelectorAll('input, select, textarea');
      inputs.forEach(el => {
        if (el.type === 'checkbox' || el.type === 'radio') {
          el.checked = el.defaultChecked;
        } else {
          el.value = el.defaultValue;
        }
        el.dispatchEvent(new Event('change', { bubbles: true }));
      });
    });
  }
  
  // Reset baseline characteristic inputs upon restart button activation
  if (!window.__resetBox3Registered) {
    window.__resetBox3Registered = true;
    Shiny.addCustomMessageHandler('resetBox3', function(id) {
      const box = document.getElementById(id);
      if (!box) return;
      const inputs = box.querySelectorAll('input, select, textarea');
      inputs.forEach(el => {
        if (el.type === 'checkbox' || el.type === 'radio') {
          el.checked = el.defaultChecked;
        } else {
          el.value = el.defaultValue;
        }
        el.dispatchEvent(new Event('change', { bubbles: true }));
      });
    });
  }
})();