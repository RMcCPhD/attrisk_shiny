
// Placeholders for greyed-out text indicating numeric range
(() => {
  // Config
  const PLACEHOLDERS = {
  // design
  'design-duration':   '12 - 520',
  'design-facilities': '1 - 500',
  'design-arms':       '2 - 6',
  // conduct
  'conduct-duration':  '12 - 520',
  'conduct-age':       '18 - 100',
  'conduct-bmi':       '18 - 50',
  'conduct-sbp':       '60 - 140',
  'conduct-hdl':       '1 - 3',
  'conduct-totalc':    '4 - 10',
  'conduct-t2dur':     '0.01 - 5',
  'conduct-hba1c':     '6 - 10',
  'conduct-egfr':      '60 - 120',
  'conduct-conmed':    '0 - 10',
  'conduct-comorbs':   '0 - 10'
  };

  // Apply placeholders (i.e. greyed out text) to each input
  function applyPlaceholders(map) {
    Object.keys(map).forEach(id => {
      const $el = $('#' + id);
      if ($el.length) $el.attr('placeholder', map[id]);
    });
  }
  
  // Initiate application
  $(document).on('shiny:connected', function() {
    applyPlaceholders(PLACEHOLDERS);
  });
  
  // Reset user inputs upon restart button activation
  // Design box
  if (!window.__resetBoxRegistered) {
    window.__resetBoxRegistered = true;
    Shiny.addCustomMessageHandler('resetBox', function(id) {
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
  
  // Design box (conduct page)
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
  
  // Baseline characteristics
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
